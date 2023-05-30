open Base

type 'a environment = { mutable store : 'a Map.M(String).t }

type t =
  | Return of t
  | Integer of int
  | Boolean of bool
  | Function of
      { parameters : Ast.identifier list
      ; body : Ast.block
      ; env : t environment [@opaque]
      }
  | Null
[@@deriving show]

module Environment = struct
  let empty = { store = Map.empty (module String) }
  let get t key = Map.find t.store key
  let set t key data = t.store <- Map.set ~key ~data t.store
end

let monkey_true = Boolean true
let monkey_false = Boolean false
