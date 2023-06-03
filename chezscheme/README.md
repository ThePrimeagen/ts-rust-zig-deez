# README

- Run without compiling
  ```
  $ scheme --script lexer.scm
  ```
- Compile with optimizations
  ```
  $ scheme --script compile.scm --optimize-level 3
  ```
- Run compiled program
  ```
  $ scheme --program lexer.so
  ```
- Pipe source to lexer
  ```
  $ echo let x = 5 | scheme --program lexer.so
  Let
  Ident x
  Equal
  Int 5
  Eof
  ```
