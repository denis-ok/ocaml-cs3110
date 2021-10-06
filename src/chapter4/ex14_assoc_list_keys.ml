(* Exercise: association list keys [★★★]

   Recall that an association list is an implementation of a dictionary in terms of a list of pairs,
   in which we treat the first component of each pair as a key and the second component as a value.

   Write a function keys: ('a * 'b) list -> 'a list that returns a list of the unique keys in an association list.
   Since they must be unique, no value should appear more than once in the output list.
   The order of values output does not matter.
   How compact and efficient can you make your solution?
   Can you do it in one line and linearithmic space and time?

   Hint: List.sort_uniq. *)

let keys l =
  List.fold_left
    (fun acc (key, _) -> if List.mem key acc then acc else key :: acc)
    [] l

let keys' l =
  List.map (fun (k, _v) -> k) l
  |> List.sort_uniq (fun a b -> Stdlib.compare a b)
