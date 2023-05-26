type node =
  | Program of program
  | Expression of expression
  | Statement of statement

and expression =
  | Identifier of identifier
  | Integer of int
  | Boolean of bool
  | Prefix of
      { operator : Token.t
      ; right : expression
      }
  | Infix of
      { left : expression
      ; operator : Token.t
      ; right : expression
      }
[@@deriving show]

and statement =
  | Let of
      { token : Token.t
      ; name : identifier
      ; value : expression
      }
  | Return of
      { token : Token.t
      ; expr : expression
      }
  | ExpressionStatement of expression
[@@deriving show]

and identifier =
  { token : Token.t
  ; value : string
  }

and program = { statements : statement list }

let token_literal = function
  | Program _ -> "program"
  | Expression _ -> "expression"
  | Statement _ -> "statement"
;;
