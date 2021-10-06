(* Exercise: more list fun [★★★]

   Write functions that perform the following computations.
   Each function that you write should use one of List.fold, List.map or List.filter.
   To choose which of those to use, think about what the computation is doing:
   combining, transforming, or filtering elements. *)

(* Find those elements of a list of strings whose length is strictly greater than 3. *)

let find_longer_than_3 = List.filter (fun x -> String.length x > 3)

(* Add 1.0 to every element of a list of floats. *)

let increment_floats = List.map (fun x -> x +. 1.0)

(* Given a list of strings strs and another string sep,
   produce the string that contains every element of strs separated by sep.
   For example, given inputs ["hi";"bye"] and ",", produce "hi,bye",
   being sure not to produce an extra comma either at the beginning or end of the result string. *)

let join_strings strings separator =
  match strings with
  | [] -> ""
  | head :: tail ->
      List.fold_left (fun acc value -> acc ^ separator ^ value) head tail

(* Library function for that: String.concat *)
