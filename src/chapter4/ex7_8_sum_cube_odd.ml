(* Exercise: sum_cube_odd [★★]
   Write a function sum_cube_odd n that computes the sum of the cubes of all the odd numbers between 0 and n inclusive.
   Do not write any new recursive functions.
   Instead, use the functionals map, fold, and filter, and the ( -- ) operator (defined in the discussion of pipelining). *)

let rec ( -- ) i j = if i > j then [] else i :: (i + 1 -- j)

let is_odd n = n mod 2 = 0

let cube x = x * x * x

let sum_cube_odd n =
  List.fold_left ( + ) 0 (List.map cube (List.filter is_odd (0 -- n)))

(* Exercise: sum_cube_odd pipeline [★★]
   Rewrite the function sum_cube_odd to use the pipeline operator |>. *)

let sum_cube_odd' n =
  0 -- n |> List.filter is_odd |> List.map cube |> List.fold_left ( + ) 0
