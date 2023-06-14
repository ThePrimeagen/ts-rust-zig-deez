type t =
  | Return of t
  | Integer of int
  | String of string
  | Boolean of bool
  | Function of func
  | Builtin of builtin
  | Array of t list
  | Hash of (t * t) list
  | Null

and func =
  { parameters : Ast.identifier list
  ; body : Ast.block
  ; env : t Environment.environment [@opaque]
  }

and builtin = BuiltinFn of (t list -> (t, string) result)

val show : t -> string
val pp : Format.formatter -> t -> unit

val monkey_true : t
val monkey_false : t

val builtin_fn : (t list -> (t, string) result) -> t
