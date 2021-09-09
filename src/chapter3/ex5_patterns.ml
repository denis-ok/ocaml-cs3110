(* Exercise: patterns [★★★]
   Using pattern matching, write three functions, one for each of the following properties.
   Your functions should return true if the input list has the property and false otherwise.
*)

(* the list’s first element is "bigred" *)
let _match_list l =
  match l with
  | "bigred" :: _ -> true
  | _ -> false

(* the list has exactly two or four elements; do not use the length function *)
let _match_list l =
  match l with
  | [ _; _ ] -> true
  | [ _; _; _; _ ] -> true
  | _ -> false

(* the first two elements of the list are equal *)
let _match_list l =
  match l with
  | one :: two :: _ when one = two -> true
  | _ -> false
