type 'a t =
  { mutable arr : 'a array
  ; mutable len : int
  }

let create init_size dummy = { arr = Array.make init_size dummy; len = 0 }

let push t el =
  if Array.length t.arr = t.len
  then
    t.arr <- Array.init ((2 * t.len) + 1) (fun i -> if i < t.len then t.arr.(i) else el);
  t.arr.(t.len) <- el;
  t.len <- t.len + 1
;;

let get t i = if i >= t.len || i < 0 then None else Some t.arr.(i)

let to_list t =
  let arr = Array.init t.len (fun i -> t.arr.(i)) in
  Array.to_list arr
;;
