(* Exercise: concat [★★]
   Write a function that concatenates all the strings in a list.
   The concatenation of all the strings in an empty list is the empty string "". *)

let rec concat l =
  match l with
  | [] -> ""
  | h :: tail -> h ^ concat tail
