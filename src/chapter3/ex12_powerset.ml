let rec prepend (item : int) (l : int list list) =
  match l with
  | [] -> []
  | head :: tail -> (item :: head) :: prepend item tail

let powerset (l : int list) : int list list =
  let rec powerset (l : int list) : int list list =
    match l with
    | [] -> []
    | head :: tail -> ([ head ] :: prepend head (powerset tail)) @ powerset tail
  in
  [] :: powerset l
