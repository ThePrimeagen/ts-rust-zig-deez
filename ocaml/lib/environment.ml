open Base

type 'a environment =
  { mutable store : 'a Map.M(String).t
  ; outer : 'a environment option
  }

let init () = { store = Map.empty (module String); outer = None }

let enclosed outer =
  let empty = init () in
  { empty with outer = Some outer }
;;

let rec get t key =
  match Map.find t.store key with
  | Some data -> Some data
  | None -> Option.bind t.outer ~f:(fun store -> get store key)
;;

let set t key data = t.store <- Map.set ~key ~data t.store
