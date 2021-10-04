(* Exercise: exists [★★]

   Consider writing a function exists: ('a -> bool) -> 'a list -> bool, such that exists p [a1; ...; an] returns
   whether at least one element of the list satisfies the predicate p.
   That is, it evaluates the same as (p a1) || (p a2) || ... || (p an).
   When applied to an empty list, it evaluates to false.

   Write three solutions to this problem, as we did above:

   exists_rec, which must be a recursive function that does not use the List module,
   exists_fold, which uses either List.fold_left or List.fold_right, but not any other List module functions nor the rec keyword, and
   exists_lib, which uses any combination of List module functions other than fold_left or fold_right, and does not use the rec keyword. *)

let rec exists_rec f l =
  match l with
  | [] -> false
  | head :: tail -> f head || exists_rec f tail

let exists_fold f l = List.fold_left (fun acc value -> acc || f value) false l

let exists f l =
  match List.find_opt f l with
  | Some _ -> true
  | None -> false
