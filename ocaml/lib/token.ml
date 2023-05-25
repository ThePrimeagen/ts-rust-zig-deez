type t =
  | Illegal
  (* Identifiers *)
  | Ident of string
  | Integer of string
  (* Operators *)
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
  (* Delimiters *)
  | Comma
  | Semicolon
  | LeftParen
  | RightParen
  | LeftBrace
  | RightBrace
  (* Keyword *)
  | Function
  | Let
  | True
  | False
  | If
  | Else
  | Return
[@@deriving show, eq]

let lookup_ident str =
  match str with
  | "fn" -> Function
  | "let" -> Let
  | "true" -> True
  | "false" -> False
  | "if" -> If
  | "else" -> Else
  | "return" -> Return
  | _ -> Ident str
;;
