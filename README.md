# 32-bit RISC processor simulator

This project is an imaginary processor based on RISC architecture. 

## Architecture
All instructions occupy one word and require one memory cycle to execute (additional time may be required for data access).

## Processor
There are sixteen registers, R0, R1, . . . , R15. R0 always contains zero. There is a 32-bit program counter that contains the address of the next instruction to be executed.

## Memory
A memory address is a value in the range 0, 1, ..., 2^31 . The amount of memory actually available is typically less than this.

Each address identifies one 8-bit byte. The addresses 0, 4, ..., 4N are word addresses. The processor can load and store bytes and words.

## Design decisions:
- word: 32 bits
- byte: 8 bits
- integer: 32 bits; stored as two's complement
- float/double: TODO
- address: 32 bits; interpreted as unsigned
- registers: there are 16: R0, .., R15. Each register stores a word. R0 = 0.
- instructions: occupy 1 word of memory, and can be in one of 2 formats defined in the next section
