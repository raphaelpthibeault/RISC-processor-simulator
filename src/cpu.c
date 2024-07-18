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

struct use_node {
  long addr;
  struct use_node *next;
};

struct sym_node {
  char *name;
  long val;
  short defs;
  struct use_node *uses;
  struct sym_node *next;
};

struct sym_node *symbols = NULL;    /* ptr to base of symbol table */

/* Return a pointer to a symbol entry.
 * It always succeeds, because it creates a new entry if it can't find a matching entry.
 */
struct sym_node *
find_symbol(char *name) {
    struct sym_node *p = symbols;
    while (p) {
  		if (!strcmp(name, p->name))
	    	return p;
		p = p->next;
    }
    /* no entry exists, so create one */
    p = (struct sym_node *)malloc(sizeof(struct sym_node));
    if (p == NULL) {
        printf("Out of memory.\n");
        exit(1);
    }
   	p->name = (char *)malloc(strlen(name) + 1);
	strcpy(p->name, name);
	p->val = 0;
	p->defs = 0;
	p->uses = NULL;
	p->next = symbols;
	symbols = p;
	return p;
}

/* Define a symbol
 */
void
def_symbol(char *name, long val) {
    struct sym_node *p = find_symbol(name);
    p->val = val;
    ++p->defs;
}

/* Use a symbol
 */
void
use_symbol(char *name, long addr) {
    struct sym_node *p = find_symbol(name);
    struct use_node *u = (struct use_node *)malloc(sizeof(struct use_node));
    if (u == NULL) {
        printf("Out of memory.\n");
        exit(1);
    }
   	u->addr = addr;
	u->next = p->uses;
	p->uses = u;
}

/* Returns value of a symbol.
 * Returns -1 if the symbol does not exist
 */
 long
 get_symbol_val(char *name) {
     struct sym_node *p = symbols;
    	while (p) {
		if (!strcmp(name, p->name))
			return (p->val);
		p = p->next;
	}
	return -1;
 }

 /* Display all symbols and their uses.
  */
void
show_symbols() {
   	short count = 0;
	char reply[80];
	struct sym_node *p = symbols;
	while (p) {
		struct use_node *u = p->uses;
		printf("%-8s = %4ld  Used at: ", p->name, p->val);
		while (u) {
			printf("%ld ", u->addr);
			u = u->next;
		}
		printf("\n");
		p = p->next;
		if (++count > 20) {
			printf("Press enter to continue");
			fgets(reply, sizeof(reply), stdin);
			count = 0;
		}
	}
}

/* check symbol list for errors and return the number of errors
 */
int
check_symbols() {
   	int errors = 0;
	struct sym_node* p = symbols;
	while (p) {
		if (p->defs == 0) {
			printf("Undefined symbol: %s.\n", p->name);
			errors++;
		}
		else if (p->defs > 1) {
			printf("Redefined symbol: %s.\n", p->name);
			errors++;
		}
		p = p->next;
	}
	return errors;
}

/* store symbols
 */
void
store_symbols() {
   	struct sym_node* p = symbols;
	while (p) {
		struct use_node* u = p->uses;
		while (u) {
			long wordaddr = (u->addr) >> 2;
			switch (memory[wordaddr].content) {
			case 'b':
				memory[wordaddr].word.format_b.k = (int)p->val;
				break;
			case 'd':
				memory[wordaddr].word.data = p->val;
				break;
			default:
				printf("Symbol storage error\n");
				break;
			}
			u = u->next;
		}
		p = p->next;
	}
}

/************************************************ LEXING & PARSING SECTION **************************************************/

enum token_type {
    T_ILLEGAL, T_REG, T_OP, T_SYM, T_NUM, T_STR, T_COMMA, T_LP, T_RP, T_NULL
};

struct {
    char sym_val[TOK_LEN];          /* characters of the token */
    enum token_type kind;           /* kind of the token */
    char *pos;                      /* pointer to start of token */
    short reg;                      /* register number for T_REG */
    short op;                       /* op code for T_OP */
    long int_val;                   /* value for T_NUM */
} token;

char old_val[TOK_LEN];
char *err_msg;                      /* error message */
int error_count = 0;                /* number of errors detected */
char *bp;                           /* buffer pointer */

/* Record an error message for later use. Only the first error is recorded.
 */
void
syntax_error(char *msg) {
    ++error_count;
    if (!strcmp(err_msg, "")) {
      	strcpy(err_msg, "Error at `");
		strcat(err_msg, old_val);
		strcat(err_msg, " ");
		strcat(err_msg, token.sym_val);
		strcat(err_msg, "': ");
		strcat(err_msg, msg);
    }
}

/* is char a valid symbol
 */
short
is_sym_char(char c) {
    return isalnum(c) || c == '_';
}

/* is string a valid register; p should be a pointer to the beginning of the string
 */
short
is_reg_str(char *p) {
    long reg_num = 0;
    if (!(*p == 'R' || *p == 'r')) {
        return FALSE;
    }
    ++p;
    while (*p) {
        if (isdigit(*p)) {
            reg_num = 10 * reg_num + *p - '0';
        } else {
            return FALSE;
        }
        ++p;
    }

    if (reg_num >= MAX_REG) {
        char *err = &"Illegal register: R" [reg_num];
        syntax_error(err);
        return FALSE;
    }

    token.reg = (short)reg_num;
    return TRUE;
}

/* The classic lexer next() function, with the result being stored in the token struct
 */
void
next() {
    char *t = token.sym_val;
    short op;
    strcpy(old_val, token.sym_val);

    while (*bp == ' ' || *bp == '\t')
        ++bp;
    token.pos = bp;

    if (isalpha(*bp)) {
        /* it's a reg, op code, directive or symbol */
        while (is_sym_char(*bp))
            *t++ = *bp++;
        *t = '\0';

        if (is_reg_str(token.sym_val)) {
            token.kind = T_REG;
            return;
        }

        for (op = lw; op < last; ++op) {
            if (!strcmp(token.sym_val, op_names[op])) {
                token.op = op;
                token.kind = T_OP;
                return;
            }
        }

        token.kind = T_SYM;
        return;
    } else if (*bp == '-' || *bp == '+' || isdigit(*bp)) {
        /* signed decimal integer */
        *t++ = *bp++;
        while (isdigit(*bp))
            *t++ = *bp++;
        *t = '\0';
        sscanf(token.sym_val, "%ld", &token.int_val);
		token.kind = T_NUM;
		return;
    } else if (*bp == '"') {
        /* string in enclosing quotes */
        ++bp;
        while (1) {
            if (*bp == '"') {
                *t = '\0';
                token.kind = T_STR;
                ++bp;
                break;
            }
            if (*bp == '\0' || *bp == '\n') {
                syntax_error("Unterminated string.");
                token.kind = T_ILLEGAL;
                *t = '\0';
                break;
            }
            *t++ = *bp++;
        }
        return;
    } else if (*bp == ',') {
        ++bp;
        token.kind = T_COMMA;
        strcpy(token.sym_val, ",");
        return;
    } else if (*bp == '(') {
        ++bp;
        token.kind = T_LP;
        strcpy(token.sym_val, "(");
        return;
    } else if (*bp == ')') {
        ++bp;
        token.kind = T_RP;
        strcpy(token.sym_val, ")");
        return;
    } else if (*bp == '%' || *bp == '\n' || *bp == '\0') {
        token.kind = T_NULL;
        strcpy(token.sym_val, " ");
        return;
    } else {
        token.kind = T_ILLEGAL;
        strcpy(token.sym_val, " ");
    }
}

/* Match the token type
 */
void
match(enum token_type kind) {
    if (token.kind == kind) {
		next();
		return;
	}
	switch (kind) {
    	case T_COMMA:
    		syntax_error("',' expected");
    		break;
    	case T_LP:
    		syntax_error("'(' expected");
    		break;
    	case T_RP:
    		syntax_error("')' expected");
    		break;
    	default:
    		syntax_error("Syntax error");
    		break;
	}
}

/* Parse an opcode
 */
short
get_op() {
    if (token.kind == T_OP) {
        short res = token.op;
        next();
        return res;
    }
   	syntax_error("Opcode expected");
	return 0;
}

/* Parse ar eg and return the register number
 */
short
get_reg() {
    if (token.kind == T_REG) {
        short res = token.reg;
        next();
        return res;
    }
    syntax_error("Register expected");
    return 0;
}

/* Parse a constant (number or symbol) and return value
 */
long
get_long(long addr) {
    if (token.kind == T_NUM) {
        long res = token.int_val;
        next();
        return res;
    } else if (token.kind == T_SYM) {
        use_symbol(token.sym_val, addr);
        next();
        return 0;
    }
    syntax_error("Constant expected");
    return 0;
}

/* get_long(), but checks that its argument can be stored in 16 bits.
 */
int
get_int(long addr) {
   	long val = get_long(addr);
	if (labs(val) <= 32767)
		return (int)val;
	syntax_error("Value cannot be represented with 16 bits");
	return 0;
}

char buffer[BUF_LEN];		/* Input buffer */
long addr = 0;
int line_num = 0;

/* read a line of source code (assembly) from the buffer
*/
void
read_line() {
    int c;
    word_type word;
    word.data = 0;
    bp = buffer;
    strcpy(err_msg, "");
    strcpy(old_val, "");
    next();
    while (token.kind == T_SYM) {
		def_symbol(token.sym_val, addr);
		next();
	}
    if (token.kind == T_OP) {
        switch (token.op) {
			/* Format A -- registers only */
			/* Operands Ri, Rj, Rk */
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
                word.format_a.op = get_op();
                word.format_a.ri = get_reg();
                match(T_COMMA);
                word.format_a.rj = get_reg();
                match(T_COMMA);
                word.format_a.rk = get_reg();
                store_mem_instr(addr, word, 'a');
                addr += 4;
                break;

                /* Operands Ri, Rj */
            case not:
            case jlr:
                word.format_a.op = get_op();
                word.format_a.ri = get_reg();
                match(T_COMMA);
                word.format_a.rj = get_reg();
                store_mem_instr(addr, word, 'a');
                addr += 4;
                break;

                /* No operands */
            case nop:
            case hlt:
                word.format_a.op = get_op();
                store_mem_instr(addr, word, 'a');
                addr += 4;
                break;

                /* Format B - operands and constant fields */

                /* Operands Ri, K(Rj) */
            case lw:
            case lb:
                word.format_b.op = get_op();
                word.format_b.ri = get_reg();
                match(T_COMMA);
                word.format_b.k = get_int(addr);
                match(T_LP);
                word.format_b.rj = get_reg();
                match(T_RP);
                store_mem_instr(addr, word, 'b');
                addr += 4;
                break;

                /* Operands K(Rj), Ri */
            case sw:
            case sb:
                word.format_b.op = get_op();
                word.format_b.k = get_int(addr);
                match(T_LP);
                word.format_b.rj = get_reg();
                match(T_RP);
                match(T_COMMA);
                word.format_b.ri = get_reg();
                store_mem_instr(addr, word, 'b');
                addr += 4;
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
                word.format_b.op = get_op();
                word.format_b.ri = get_reg();
                match(T_COMMA);
                word.format_b.rj = get_reg();
                match(T_COMMA);
                word.format_b.k = get_int(addr);
                store_mem_instr(addr, word, 'b');
                addr += 4;
                break;

                /* Operands Ri, K */
            case sl:
            case sr:
            case bz:
            case bnz:
            case jl:
                word.format_b.op = get_op();
                word.format_b.ri = get_reg();
                match(T_COMMA);
                word.format_b.k = get_int(addr);
                store_mem_instr(addr, word, 'b');
                addr += 4;
                break;

                /* Operands Ri */
            case gtc:
            case ptc:
            case jr:
                word.format_b.op = get_op();
                word.format_b.ri = get_reg();
                store_mem_instr(addr, word, 'b');
                addr += 4;
                break;

                /* Operands K */
            case j:
                word.format_b.op = get_op();
                word.format_b.k = get_int(addr);
                store_mem_instr(addr, word, 'b');
                addr += 4;
                break;

                /* Set the entry point of the program. */
            case entry:
                next();
                if (entry_point < 0) {
                    entry_point = addr;
                    break;
                }
                syntax_error("More than one entry point");
                break;

                /* Adjust the address to the next word boundary. */
            case align:
                next();
                if (addr & 3)
                    addr = (addr & ~3) + 4;
                break;

                /* Set the address to the given value. */
            case org:
                next();
                addr = get_long(addr);
                break;

                /* Store words. */
            case dw:
                next();
                while (token.kind == T_NUM || token.kind == T_SYM) {
                    word.data = get_long(addr);
                    store_mem_instr(addr, word, 'd');
                    addr += 4;
                    if (token.kind == T_COMMA)
                        next();
                    else
                        break;
                }
                break;

                /* Store bytes */
            case db:
                next();
                while (1) {
                    if (token.kind == T_NUM) {
                        if (0 <= token.int_val && token.int_val <= 255) {
                            store_mem_char(addr, token.int_val, 'd');
                            addr++;
                        }
                        else
                            syntax_error("Value cannot be represented with 8 bits");
                        next();
                    }
                    else if (token.kind == T_STR) {
                        char* t = token.sym_val;
                        while (*t) {
                            store_mem_char(addr, *t++, 'd');
                            addr++;
                        }
                        next();
                    }
                    if (token.kind == T_COMMA)
                        next();
                    else if (token.kind == T_NULL)
                        break;
                    else {
                        syntax_error("Syntax error in byte list");
                        break;
                    }
                }
                break;

                /* Reserve the given number of words. */
            case res:
                next();
                addr += get_long(addr);
                break;

                /* Should never get here. */
            default:
                syntax_error("Unrecognized statement");
                break;
		}


    }
    if (token.kind != T_NULL && error_count < 5) {
		printf("Warning: junk following `%s' on next line.\n", token.sym_val);
		printf("%4d  %s\n", line_num, buffer);
		++error_count;
	}
}

/* load a source file
 */
void
load(FILE* f_in, FILE* f_out, short listing) {
    line_num = 0;
    while (fgets(buffer, BUF_LEN - 1, f_in)) {
        long old_addr = addr;
        ++line_num;
        read_line();
        if (listing)
			fprintf(f_out, "%5d %5ld %s", line_num, old_addr, buffer);
		if (strcmp(err_msg, "")) {
			printf("%5d %5ld %s", line_num, old_addr, buffer);
			printf("      >>>>> %s\n", err_msg);
			if (listing)
    			fprintf(f_out, "      >>>>> %s\n", err_msg);
		}
    }
}

/************************************************ EXECUTION SECTION **************************************************/
short running;          /* true: processor running, false: processor not running (error) */
short new_reg;          /* address of a register that has changed */
long new_mem;           /* address of a memory location that has changed */
long num_steps;         /* number of instructions executed in trace mode */


void
runtime_error(char *msg) {
   	printf("\n%5ld Run-time error: %s.\n", ic, msg);
	running = FALSE;
}

/* execute the instruction at ic
 */
void 
exec_instr(short tracing) {
    
}

/* Dump words of memory from addr-10 to addr+10.
 */
void 
dump(long adr) {

}

/* Display a register 
 */
void 
show_reg(short reg_num) {

}

/* execute an instruction in trace mode
 */
void 
trace_instr() {

}

/* execute steps number of instructions in trace mode
 */
void 
run_for(long steps) {

}

/* Fetch the operand of a trace instruction. 
 * The operand should be either a number or a symbol.
 */
long 
get_operand(char *cp) {
    return 0L;
}

void 
show_trace_usage() {
	printf("The tracer prompts with `IC:-\'.  IC is the instruction counter.\n");
	printf("Upper or lower case letters are accepted.  n must be positive.\n");
	printf("\n");
	printf("<cr>     Trace K instructions.\n");
	printf("n        Trace n instructions.\n");
	printf("B        Display breakpoints.\n");
	printf("Bn       Set a breakpoint at n.\n");
	printf("C        Clear all breakpoints.\n");
	printf("Cn       Clear the breakpoint at n.\n");
	printf("D        Dump memory near IC.\n");
	printf("Dn       Dump memory near n.\n");
	printf("I        Set IC to entry point.\n");
	printf("In       Set IC to n.\n");
	printf("K        Set K (# steps executed by <cr>) to 10.\n");
	printf("Kn       Set K to n.\n");
	printf("Q        Quit.\n");
	printf("R        Show registers.\n");
	printf("S        Show symbols.\n");
	printf("X        Run to next break point.\n");
	printf("Xn       Run until IC = n.\n");
	printf("\n");
}

void 
exec_trace() {

}

void 
exec() {
    ic = entry_point;
    running = TRUE;
    while (running) {
        exec_instr(FALSE);
    }
    printf("\n%ld cycles.\n", cycles);
}

