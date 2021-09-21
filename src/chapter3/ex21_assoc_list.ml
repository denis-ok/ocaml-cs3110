(* Exercise: assoc list [★]
   Use the functions insert and lookup from the section on association lists to construct an association list that maps
   the integer 1 to the string “one”, 2 to “two”, and 3 to “three”. Lookup the key 2. Lookup the key 4. *)

(** [insert k v lst] is an association list that binds key [k] to value [v]
    and otherwise is the same as [lst] *)
let insert k v lst = (k, v) :: lst

(** [find k lst] is [Some v] if association list [lst] binds key [k] to
    value [v]; and is [None] if [lst] does not bind [k]. *)
let rec lookup k = function
  | [] -> None
  | (k', v) :: t -> if k = k' then Some v else lookup k t

let numbers = insert 1 "one" [] |> insert 2 "two" |> insert 3 "three"

(* val two : string option = Some "two" *)
let two = lookup 2 numbers

(* val four : string option = None *)
let four = lookup 4 numbers
