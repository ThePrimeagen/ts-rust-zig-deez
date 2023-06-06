type 'a environment

(* val empty : 'a environment *)
val init : unit -> 'a environment
val enclosed : 'a environment -> 'a environment
val get : 'a environment -> string -> 'a option
val set : 'a environment -> string -> 'a -> unit
