(* Exercise: list expressions [★] *)

(* Construct a list that has the integers 1 through 5 in it. Use the square bracket notation for lists. *)
let _ = [ 1; 2; 3; 4; 5 ]

(* Construct the same list, but do not use the square bracket notation. Instead use :: and []. *)
let _ = [ 1; 2; 3; 4; 5 ]

(* Construct the same list again. This time, the following expression must appear in your answer: [2; 3; 4].
   Use the @ operator, and do not use ::. *)
let _ = [ 1 ] @ [ 2; 3; 4 ] @ [ 5 ]
