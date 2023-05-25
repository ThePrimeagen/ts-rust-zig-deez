type t


val init : string -> t
val next_token : t -> t * Token.t option

(** Formatter for printing the lexer *)
val pp: Format.formatter -> t -> unit
