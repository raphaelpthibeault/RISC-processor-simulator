# 32-bit RISC processor

## Design decisions:

- word: 32 bits
- byte: 8 bits
- integer: 32 bits; stored as two's complement
- float: 32 bits; single precision floating point
- float/double: TODO
- address: 32 bits; interpreted as unsigned
- registers: there are 16: R0, .., R15. Each register stores a word. R0 = 0.
- instructions: occupy 1 word of memory, and can be in one of 2 formats defined in the next section
