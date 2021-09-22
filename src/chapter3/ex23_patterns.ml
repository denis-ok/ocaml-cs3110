(* Exercise: matching [★]

   For each pattern in the list below, give a value of type int option list
   that does not match the pattern and is not the empty list,
   or explain why that’s impossible. *)

(* Some x :: tl *)
let _ = [ None ]

(* [Some 3110; None] *)
let _ = [ None; None ]

(* [Some x; _] *)
let _ = [ None; None ]

(* h1 :: h2 :: tl *)
(* Any list with two or more elements will match. List with single element will not match. *)
let _ = [ None ]

(* h :: tl *)
(* Any list with one or more elements will match. Only empty list doesn't match *)
let _ = []
