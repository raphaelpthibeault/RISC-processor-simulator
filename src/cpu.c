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
get_mem_word(long addr) {
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
 put_mem_word(long addr, long data) {
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
get_mem_byte(long addr) {
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
put_mem_byte(long addr, BYTE byte) {
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
put_mem_instr(long addr, word_type word, char content) {
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
put_mem_char(long addr, short byte, char content) {
    long word_addr = addr >> 2;
    short offset = addr & 0b11;

    memory[word_addr].word.bytes[offset] = byte;
    memory[word_addr].content = content;
}

/************************************************ Parsing SECTION **************************************************/


/************************************************ EXECUTION SECTION **************************************************/
short running;          /* true: processor running, false: processor not running (error) */

void
runtime_error(char *msg) {
   	printf("\n%5ld Run-time error: %s.\n", ic, msg);
	running = FALSE;
}
