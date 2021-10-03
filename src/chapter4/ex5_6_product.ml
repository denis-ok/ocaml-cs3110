(* Exercise: product [★]

   Use fold_left to write a function product_left that computes the product of a list of floats.
   The product of the empty list is 1.0. Hint: recall how we implemented sum in just one line of code in lecture.
   Use fold_right to write a function product_right that computes the product of a list of floats. Same hint applies. *)

(* Exercise: terse product [★★]

   How terse can you make your solutions to the product exercise?
   Hints: you need only one line of code for each, and you do not need the fun keyword.
   For fold_left, your function definition does not even need to explicitly take a list argument.
   If you use ListLabels, the same is true for fold_right. *)

let product_left = List.fold_left ( *. ) 1.0

let product_left' = ListLabels.fold_left ~f:( *. ) ~init:1.0

let product_right l = List.fold_right ( *. ) l 1.0

let product_right' = ListLabels.fold_right ~f:( *. ) ~init:1.0
