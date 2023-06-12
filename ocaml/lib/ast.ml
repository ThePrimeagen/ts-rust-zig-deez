type node =
  | Program of program
  | Expression of expression
  | Statement of statement

and expression =
  | Identifier of identifier
  | Integer of int
  | Boolean of bool
  | String of string
  | Prefix of
      { operator : Token.t
      ; right : expression
      }
  | Infix of
      { left : expression
      ; operator : Token.t
      ; right : expression
      }
  | If of
      { condition : expression
      ; consequence : block
      ; alternative : block option
      }
  | FunctionLiteral of
      { parameters : identifier list
      ; body : block
      }
  | Call of
      { fn : expression
      ; args : expression list
      }
  | Array of expression list
  | Index of
      { left : expression
      ; index : expression
      }
  | Hash of (expression * expression) list
  | Macro of
      { parameters : identifier list
      ; body : block
      }
[@@deriving show { with_path = false }, sexp]

and statement =
  | Let of
      { name : identifier
      ; value : expression
      }
  | Return of expression
  | ExpressionStatement of expression
  | BlockStatement of block
[@@deriving show { with_path = false }, sexp]

and identifier = { identifier : string } [@@deriving sexp]
and block = { block : statement list }
and program = { statements : statement list }

let token_literal = function
  | Program _ -> "program"
  | Expression _ -> "expression"
  | Statement _ -> "statement"
;;
