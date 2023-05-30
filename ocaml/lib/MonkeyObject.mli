type 'a environment 

type t =
  | Return of t
  | Integer of int
  | Boolean of bool
  | Function of
      { parameters : Ast.identifier list
      ; body : Ast.block
      ; env : t environment
      }
  | Null

val show : t -> string
(* val pp : Format.formatter -> t -> unit *)

module Environment : sig
  val empty : t environment
  val get : t environment -> string -> t option
  val set : t environment -> string -> t -> unit
end

val monkey_true : t
val monkey_false : t
