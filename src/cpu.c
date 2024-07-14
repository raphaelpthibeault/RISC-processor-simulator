#include <cpu.h>

void runtime_error(char*);

/************************************************ MEMORY SECTION **************************************************/

/*
 * a word of memory contains an instruction, an integer, or 4 bytes.
 * It also has 2 flags that indicate if the word contains an instruction, and if the word is a breakpoint
 *
 * content:
 * 'a' -> instruction in format A
 * 'b' -> instruction in format B
 * 'd' -> "data"
 * 'u' -> undefined
 *
 */
struct {
    word_type word;
    char content;
    short breakpoint;
} memory[MEMSIZE];

long ic;                /* instruction counter: address of instr to be executed next */
word_type ir;           /* instruction register */
long mar;               /* memory address register; holds word address of last access */
word_type mdr;          /* memory data register; holds result of last access */

long entry_point = -1;  /* address of first instruction */
long cycles = 0;        /* memory cycle count */

/* Report a runtime error if the address is out or range.
 * Return 1 if error.
 * Return 0 if success.
 */
short
out_of_range(long addr) {
    if (addr < 0 || (addr >> 2) > MEMSIZE) {
        char *err = &"Address out of range error @ " [addr];
        runtime_error(err);
        return 1;
    }
    return 0;
}

/* Report a runtime error if the address is not on a 4-byte boundary.
 * Return 1 if error.
 * Return 0 if success.
 */
short
misaligned(long addr) {
    if (addr & 0b11) {
        char *err = &"Address alignment error @ " [addr];
        runtime_error(err);
        return 1;
    }
    return 0;
}


/*
 * init to:
 * data = empty, type = undefined, breakpoint = no
 */
void
init_mem() {
    long word_addr;
    for (word_addr = 0; word_addr < MEMSIZE; ++word_addr) {
        memory[word_addr].word.data = 0;
        memory[word_addr].content = 'u';
        memory[word_addr].breakpoint = FALSE;
    }
    ic = -1;
    mar = -1;
}

/*
 * fetch instruction @ ic, put it in ir, then increment ic.
 * Return 0 if runtime error.
 * Return content type if success.
 */
char
fetch() {
    char content;
    if (out_of_range(ic)) {
        ir.data = 0;
        return 0;
    }

    content = memory[ic >> 2].content;
    if (!(content == 'a' || content == 'b')) {
        char *err = &"Illegal content type " [content];
        runtime_error(err);
        return 0;
    }

    ir = memory[ic >> 2].word;
    ic += 4;
    cycles += 10;
    return content;
}

/* Fetch a data word from memory and return it
 * Return 0 if error.
 * Return data if success.
 */
long
fetch_mem_word(long addr) {
    if (out_of_range(addr) || misaligned(addr)) {
        return 0;
    }

    long word_addr = addr >> 2;

    if (word_addr == mar) {
        cycles += 1;
    } else {
        mar = word_addr;
        mdr = memory[word_addr].word;
        cycles += 10;
    }

    return mdr.data;
}

/* Put word in memory.
 * Exits out early if error.
 */
 void
 store_mem_word(long addr, long data) {
     if (out_of_range(addr) || misaligned(addr)) {
         return;
     }

     long word_addr = addr >> 2;

     if (memory[word_addr].content == 'a' || memory[word_addr].content == 'b') {
         char *err = &"Overwriting instructions error @ " [word_addr];
         runtime_error(err);
         return;
     }

     mdr.data = data;
     mar = word_addr;
     memory[mar].word = mdr;
     memory[mar].content = 'd';
     cycles += 10;
 }

 /* Fetch a BYTE from memory and return it.
  * Return 0 if error.
  * Return a BYTE if success.
  */
BYTE
fetch_mem_byte(long addr) {
    if (out_of_range(addr)) {
        return 0;
    }

    long word_addr = addr >> 2;
    short offset = addr & 0b11;

    if (word_addr == mar) {
        cycles += 1;
    } else {
        mar = word_addr;
        cycles += 10;
    }

    mdr = memory[mar].word;
    return mdr.bytes[offset];
}

/* Put byte in memory.
 * Exit out early if error.
 */
void
store_mem_byte(long addr, BYTE byte) {
    if (out_of_range(addr)) {
        return;
    }

    long word_addr = addr >> 2;
    short offset = addr & 0b11;

    if (memory[word_addr].content == 'a' || memory[word_addr].content == 'b') {
        char *err = &"Overwriting instructions error @ " [word_addr];
        runtime_error(err);
        return;
    }

    memory[word_addr].word.bytes[offset] = byte & 0b11111111;
    memory[word_addr].content = 'd';
}

/* Put instruction in memory. Should only be used by the assembler.
 * Exit out early if error.
 */
void
store_mem_instr(long addr, word_type word, char content) {
    if (addr & 0b11) {
        char *err = &"Address alignment error @ " [addr];
        runtime_error(err); // TODO: syntax error?
        return;
    } else {
        long word_addr = addr >> 2;
        memory[word_addr].word = word;
        memory[word_addr].content = content;
    }
}

/* Put character in memory.Should only be used by the assembler
 */
void
store_mem_char(long addr, short byte, char content) {
    long word_addr = addr >> 2;
    short offset = addr & 0b11;

    memory[word_addr].word.bytes[offset] = byte;
    memory[word_addr].content = content;
}

/************************************************ REGISTERS SECTION **************************************************/
/*
16 registers, R0, ... R15. R0 = 0. Each register is a 32-bit word.
*/

long registers[MAX_REG];

/* Fetch the value of a register.
 * Return 0 if error.
 * Return value if success.
 */
 long
 fetch_reg(unsigned short reg_num) {
     if (reg_num < 0 || reg_num > 15) {
         char *err = &"Simulator error: illegal register code = " [reg_num];
         runtime_error(err);
         return 0;
     }
     return registers[reg_num];
 }

 /* Put a value in a register.
  * Exit out early if error.
  */
  void
  store_reg(unsigned short reg_num, long data) {
      if (reg_num < 0 || reg_num > 15) {
          char *err = &"Simulator error: illegal register code = " [reg_num];
          runtime_error(err);
          return;
      }
      registers[reg_num] = data;
  }

/************************************************ INSTRUCTIONS SECTION **************************************************/
/* 0 is an illegal instruction */
enum op_type {
    illegal, lw, lb, sw, sb, add, sub, mul, newdiv, mod, and, or , not, ceq, cne, clt, cle, cgt, cge,
    addi, subi, muli, divi, modi, andi, ori, ceqi, cnei, clti, clei, cgti, cgei, sl, sr, gtc, ptc,
    bz, bnz, j, jr, jl, jlr, nop, hlt, entry, align, org, dw, db, res, last
};

char op_names[last][6] = {
	"", "lw", "lb", "sw", "sb", "add", "sub", "mul", "div", "mod",	"and", "or", "not", "ceq", "cne", "clt", "cle", "cgt", "cge",
	"addi", "subi", "muli", "divi", "modi", "andi", "ori", "ceqi", "cnei", "clti", "clei", "cgti", "cgei", "sl", "sr", "getc", "putc",
	"bz", "bnz", "j", "jr", "jl", "jlr", "nop", "hlt", "entry", "align", "org", "dw", "db", "res"
};

/* Display an address if its instruction is in format a
 */
void
show_format_a(long addr, word_type word) {
    char *op_code = op_names[word.format_a.op];
    switch (word.format_a.op) {
        /* operands Ri, Rj, Rk */
        case add:
    	case sub:
    	case mul:
    	case newdiv:
    	case mod:
    	case and:
    	case or:
    	case ceq:
    	case cne:
    	case clt:
    	case cle:
    	case cgt:
    	case cge:
      		printf("%5ld %-6s   r%d, r%d, r%d",
    			addr, op_code, word.format_a.ri,
    			word.format_a.rj, word.format_a.rk);
    		break;

        /* Operands Ri, Rj */
    	case not:
    	case jlr:
    		printf("%5ld %-6s   r%d, r%d",
    			addr, op_code, word.format_a.ri, word.format_a.rj);
    		break;

  		/* No operands */
    	case nop:
    	case hlt:
    		printf("%5ld %-6s",
    			addr, op_code);
    		break;
    }
}

/* Display an address if its instruction is in format b
 */
void
show_format_b(long addr, word_type word) {
    char *op_code = op_names[word.format_b.op];
    switch (word.format_b.op) {
        /* Operands Ri, K(Rj) */
        case lw:
        case lb:
            printf("%5ld %-6s   r%d, %d(r%d)",
    			addr, op_code, word.format_b.ri,
    			word.format_b.k, word.format_b.rj);
    		break;

        /* Operands K(Rj), Ri */
        case sw:
        case sb:
            printf("%5ld %-6s   %d(r%d), r%d",
    			addr, op_code, word.format_b.k,
    			word.format_b.rj, word.format_b.ri);
    		break;

        /* Operands Ri, Rj, K */
        case addi:
    	case subi:
    	case muli:
    	case divi:
    	case modi:
    	case andi:
    	case ori:
    	case ceqi:
    	case cnei:
    	case clti:
    	case clei:
    	case cgti:
    	case cgei:
    		printf("%5ld %-6s   r%d, r%d, %d",
    			addr, op_code, word.format_b.ri,
    			word.format_b.rj, word.format_b.k);
    		break;

        /* Operands Ri, K */
     	case sl:
    	case sr:
    	case bz:
    	case bnz:
    	case jl:
    		printf("%5ld %-6s   r%d, %d",
    			addr, op_code, word.format_b.ri, word.format_b.k);
    		break;

        /* Operands Ri */
     	case gtc:
    	case ptc:
    	case jr:
    		printf("%5ld %-6s   r%d",
    			addr, op_code, word.format_b.ri);
    		break;

        /* Operands K */
       	case j:
    	printf("%5ld %-6s   %d",
    		addr, op_code, word.format_b.k);
    	break;
	}
}

/* Convert a word to a string of 4 chars.
 * The output depends the endianness of the host.
 * Returns the word in the buffer.
 */
char *
word_to_chars(char *buf, word_type word) {
    int i;
    for (i = 0; i < 4; ++i) {
        char c = word.bytes[i];
        if (32 <= c && c <= 126) {
            buf[i] = c;
        } else {
            buf[i] = '.';
        }
    }
    buf[4] = '\0';
    return buf;
}

/* Display 1 word of memory.
 * Exits the program if eror.
 */
void
show_word(long addr) {
    if (addr & 0b11) {
        char *err = &"Internal error: invalid address " [addr];
        exit(1);
    }

    char char_buf[5];
    long word_addr = addr >> 2;
    word_type word = memory[word_addr].word;
    switch (memory[word_addr].content) {
        case 'a':
            show_format_a(addr, word);
            break;
        case 'b':
            show_format_b(addr, word);
            break;
        case 'd':
            printf("%5ld  %08lX  %s  %4ld", addr, word.data,
			word_to_chars(char_buf, word), word.data);
    		break;
        case 'u':
            printf("%5ld  ??", addr);
    		break;
    }
}


/************************************************ SYMTAB SECTION **************************************************/

/************************************************ PARSING SECTION **************************************************/


/************************************************ EXECUTION SECTION **************************************************/
short running;          /* true: processor running, false: processor not running (error) */

void
runtime_error(char *msg) {
   	printf("\n%5ld Run-time error: %s.\n", ic, msg);
	running = FALSE;
}
