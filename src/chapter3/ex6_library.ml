(* Exercise: library [★★★]
   Consult the List standard library to solve these exercises:

   Write a function that takes an int list and returns the fifth element of that list, if such an element exists.
   If the list has fewer than five elements, return 0. Hint: List.length and List.nth.

   Write a function that takes an int list and returns the list sorted in descending order.
   Hint: List.sort with Stdlib.compare as its first argument, and List.rev. *)

let get_fifth_element (l : int list) =
  if List.length l < 5 then 0 else List.nth l 4

let sort_desc (l : int list) = l |> List.sort Stdlib.compare |> List.rev
