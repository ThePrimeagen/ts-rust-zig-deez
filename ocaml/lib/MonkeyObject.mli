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
  ; env : t Environment.environment [@opaque]
  }
  | Builtin of builtin

and builtin = 
  | StringToInt of (string -> int)

val show : t -> string
(* val pp : Format.formatter -> t -> unit *)

val monkey_true : t
val monkey_false : t
