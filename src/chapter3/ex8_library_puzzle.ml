(* Exercise: library puzzle [★★★] *)

(* Your solutions will be only one or two lines of code each. *)

(* Write a function that returns the last element of a list.
   Your function may assume that the list is non-empty.
   Hint: Use two library functions, and do not write any pattern matching code of your own. *)

let get_last_elem l = l |> List.rev |> List.hd

(* Write a function any_zeroes : int list -> bool that returns true
   if and only if the input list contains at least one 0. Hint: use one library function,
   and do not write any pattern matching code of your own. *)

let any_zeroes l = List.mem 0 l
