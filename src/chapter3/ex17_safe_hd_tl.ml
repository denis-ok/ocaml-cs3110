(* Exercise: safe hd and tl [★★]

   Write a function safe_hd : 'a list -> 'a option that
   returns Some x if the head of the input list is x, and None if the input list is empty.

   Also write a function safe_tl : 'a list -> 'a list option
   that returns the tail of the list, or None if the list is empty. *)

let safe_hd l =
  match l with
  | [] -> None
  | head :: _ -> Some head

let safe_tl l =
  match l with
  | [] -> None
  | _ :: tail -> Some tail
