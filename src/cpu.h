#ifndef CPU_H
#define CPU_H

#include <stdio.h>
#include <stdlib.h>
#include <ctype.h>
#include <string.h>
#include <math.h>

/*
Instruction format:

Format A: 3 register operands
000000|0000|0000|0000|00000000000000
opcode| Ri | Rj | Rk | _

Format B: 2 register operands and a 16-bit 3rd operand
000000|0000|0000|00|0000000000000000
opcode| Ri | Rj | _ | K
*/

#define MEMSIZE 4000            /* arbitrary memory size at 4000 32-bit/4-byte words */
#define MAX_REG 16              /* 16 registers */
#define MAX_INPUT_FILES 10      /* restrict number of input files */
#define MAX_FNAME_LEN 60        /* restrict file/path name length */
#define BUF_LEN 255             /* scanner input buffer */
#define TOK_LEN 255             /* scanner token buffer */
#define RANGE 20                /* size of memory dump */

#define FALSE 0
#define TRUE 1

#define BYTE unsigned char

typedef union {
    struct {
        unsigned op : 6;
        unsigned ri: 4;
        unsigned rj: 4;
        unsigned rk: 4;
    } format_a;
    struct {
        unsigned op : 6;
        unsigned ri: 4;
        unsigned : 2;
        unsigned k: 16;
    } format_b;
    BYTE bytes[4];
    long data;
} word_type;

/* MEMORY */
void init_mem();
char fetch();
long fetch_mem_word(long);
void store_mem_word(long, long);
BYTE fetch_mem_byte(long);
void store_mem_byte(long, BYTE);
void store_mem_instr(long, word_type, char);
void store_mem_char(long, short, char);


#endif // CPU_H
