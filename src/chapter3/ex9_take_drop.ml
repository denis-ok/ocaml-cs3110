(* Exercise: take drop [★★★] *)

(* Write a function take : int -> 'a list -> 'a list such that take n lst returns the first n elements of lst.
   If lst has fewer than n elements, return all of them. *)

let rec take count l =
  match l with
  | [] -> []
  | head :: tail -> if count <= 0 then [] else head :: take (count - 1) tail

(* Write a function drop : int -> 'a list -> 'a list such that drop n lst returns all but the first n elements of lst.
   If lst has fewer than n elements, return the empty list. *)

let rec drop count l =
  match l with
  | [] -> []
  | _head :: tail -> if count <= 0 then l else drop (count - 1) tail
