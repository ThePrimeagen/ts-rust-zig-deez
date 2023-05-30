# A Brainf*ck interpreter

## Testing

I wrote the tests using Rust and with the
[brainfuck crate](https://crates.io/crates/brainfuck) to run the program. Use
the following command to run the tests:

```sh
$ cargo test
```

## Data Flow

The program takes in the input source into the program and loads each byte using
the comma operator (`,`).

The tokens are outputted to the caller using the period `.` operator. The
following byte table is used to define the the operators:

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
| Illegal               | 11         |

Later, we can pipe the lexer to a parser brainf*ck program to create an AST.

## Sources

- If-equal algorithm adapted from
  https://www.reddit.com/r/brainfuck/comments/7ddo14/test_for_2_equal_values/
