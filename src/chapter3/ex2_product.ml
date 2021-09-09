(* Exercise: product [★★]
   Write a function that returns the product of all the elements in a list.
   The product of all the elements of an empty list is *)

let rec product l =
  match l with
  | [] -> 1
  | h :: tail -> h * product tail
