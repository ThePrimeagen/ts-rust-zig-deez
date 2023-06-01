type t =
  | Return of t
  | Integer of int
  | Boolean of bool
  | Function of func
  | Null

and func =
  { parameters : Ast.identifier list
  ; body : Ast.block
  ; env : t Environment.environment [@opaque]
  }

val show : t -> string
(* val pp : Format.formatter -> t -> unit *)

val monkey_true : t
val monkey_false : t
