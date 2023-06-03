type 'a environment = 'a Environment.environment

type t =
  | Return of t
  | Integer of int
  | Boolean of bool
  | StringLit of string
  | Function of func
  | Null

and func =
  | UserDef of { parameters : Ast.identifier list
    ; body : Ast.block
    ; env : t environment [@opaque]
    }
  | Builtin of builtin

and builtin = 
  | StringToInt of (string -> int)
[@@deriving show]

let monkey_true = Boolean true
let monkey_false = Boolean false
