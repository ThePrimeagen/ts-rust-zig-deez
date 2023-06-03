type t =
  | Illegal
  | Ident(string)
  | Integer(string)
  | Assign
  | Plus
  | Minus
  | Bang
  | Asterisk
  | Slash
  | LessThan
  | GreaterThan
  | Equal
  | NotEqual
  | Comma
  | Semicolon
  | LParen
  | RParen
  | LSquirly
  | RSquirly
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
  | EOF

let lookupIdent = (str: string) => {
  switch str {
  | "fn" => Function
  | "let" => Let
  | "true" => True
  | "false" => False
  | "if" => If
  | "else" => Else
  | "return" => Return
  | _ => Ident(str)
  }
}
