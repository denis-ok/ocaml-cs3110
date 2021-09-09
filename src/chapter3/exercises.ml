(* Exercise: list expressions [★] *)
(* Construct a list that has the integers 1 through 5 in it. Use the square bracket notation for lists. *)
let _ = [ 1; 2; 3; 4; 5 ]

(* Construct the same list, but do not use the square bracket notation. Instead use :: and []. *)
let _ = [ 1; 2; 3; 4; 5 ]

(* Construct the same list again. This time, the following expression must appear in your answer: [2; 3; 4].
   Use the @ operator, and do not use ::. *)
let _ = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]

(* Exercise: product [★★]
   Write a function that returns the product of all the elements in a list.
   The product of all the elements of an empty list is *)
let rec product l =
  match l with
  | [] -> 1
  | h :: tail -> h * product tail

(* Exercise: concat [★★]
   Write a function that concatenates all the strings in a list.
   The concatenation of all the strings in an empty list is the empty string "". *)
let rec concat l =
  match l with
  | [] -> ""
  | h :: tail -> h ^ concat tail
