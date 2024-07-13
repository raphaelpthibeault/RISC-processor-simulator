#ifndef COMMON_H
#define COMMON_H

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


#endif
