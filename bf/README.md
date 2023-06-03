# A Brainf*ck interpreter

## Testing

I wrote the tests using Rust and with the
[brainfuck crate](https://crates.io/crates/brainfuck) to run the program. Use
the following command to run the tests:

```sh
$ cargo test
```

## Binary Format

The lexer outputs tokens using the following binary format:

| Name                  | Value      |
| --------------------- | ---------- |
| End of Tokens         | 0          |
| Left Parenthesis `(`  | 1          |
| Right Parenthesis `)` | 2          |
| Left Curly Brace `{`  | 3          |
| Right Curly Brace `}` | 4          |
| Comma `,`             | 5          |
| Semi Colon `;`        | 6          |
| Plus `+`              | 7          |
| Equal `=`             | 8          |
| Identifier            | 9 byte* 0  |
| Number                | 10 byte* 0 |
| Illegal               | 11 byte    |

Note we do not recognize keywords in the lexer because it requires backtracking
input bytes which would need an ident buffer store which is quite hard to manage
while lexing.

## Sources

- Equal algorithm adapted from
  https://www.reddit.com/r/brainfuck/comments/7ddo14/test_for_2_equal_values/
