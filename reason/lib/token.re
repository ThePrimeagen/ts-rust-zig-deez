[@deriving show]
type t =
  | Illegal
  | Eof
  // Identifiers + literals
  | Ident(string) // add, foobar, x, y, ...
  | Integer(string) // 1343456
  // Operators
  | Assign
  | Plus
  // Delimiters
  | Comma
  | Semicolon
  | Left_paren
  | Right_paren
  | Left_squirly
  | Right_squirly
  // Keywords
  | Function
  | Let;

let lookup_ident =
  fun
  | "fn" => Function
  | "let" => Let
  | s => Ident(s);
