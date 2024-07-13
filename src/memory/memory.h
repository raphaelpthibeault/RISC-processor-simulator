#ifndef MEMORY_H
#define MEMORY_H

#include <common.h>

/*

a word of memory contains an instruction, an integer, a float, or 4 bytes.
It also has 2 flags that indicate if the word contains an instruction, and if the word is a breakpoint

*/

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
    long integer_num;
    BYTE float_num;
} word_type;

/*
content:
'a' -> instruction in format A
'b' -> instruction in format B
'i' -> integer
'f' -> encoded float
'u' -> undefined


*/
typedef union {
    word_type word;
    char content;
    short breakpoint;
} memory[MEMSIZE];

#endif // MEMORY_H
