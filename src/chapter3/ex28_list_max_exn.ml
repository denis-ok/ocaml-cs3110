(* Exercise: list max exn [★★]

   Write a function list_max : int list -> int that returns
   the maximum integer in a list, or raises Failure "list_max"
   if the list is empty. *)

let rec list_max_iter (current : int) (rest : int list) =
  match rest with
  | [] -> current
  | head :: tail -> list_max_iter (max head current) tail

let list_max (l : int list) =
  match l with
  | [] -> failwith "list_max: empty list"
  | head :: tail -> list_max_iter head tail
