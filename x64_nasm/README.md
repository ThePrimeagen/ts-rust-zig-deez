# x86_64 NASM Assembly Implementation

An assembly implementation would already be limited to a single platform seeing as things like calling conventions have to be hard-coded if you prefer keeping your sanity.

Since this wouldn’t work on Windows anyway and because ASM is much less portable, this implementation only works on Linux.

## Dependencies
- [NASM](https://www.nasm.us)
- A C compiler such as GCC (for linking and extracting information from C headers)

## Build
```bash
make lexer
```