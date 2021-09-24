(* Exercise: list max exn [★★]

   Write a function list_max : int list -> int that returns
   the maximum integer in a list, or raises Failure "list_max"
   if the list is empty. *)

let rec list_max_iter (current : int) (rest : int list) =
  match rest with
  | [] -> current
  | head :: tail -> list_max_iter (max head current) tail

let list_max_exn (l : int list) =
  match l with
  | [] -> failwith "list_max_exn: empty list"
  | head :: tail -> list_max_iter head tail

(* Exercise: list max exn string [★★]

   Write a function list_max_string : int list -> string that returns
   a string containing the maximum integer in a list,
   or the string "empty" (note, not the exception Failure "empty" but just the string "empty")
   if the list is empty.
   Hint: string_of_int in the standard library will do what its name suggests. *)

let list_max_string l =
  match list_max_exn l with
  | v -> string_of_int v
  | exception Failure _s -> "empty"

(* Exercise: list max exn ounit [★]
   Write two OUnit tests to determine whether your solution to list max exn,
   above, correctly raises an exception when its input is the empty list,
   and whether it correctly returns the max value
   of the input list when that list is nonempty. *)

module Test = struct
  open OUnit2

  let make_test test_name expected_output l =
    test_name >:: fun _ -> assert_equal expected_output (list_max_exn l)

  let make_test_exn test_name expected_exn l =
    test_name >:: fun _ -> assert_raises expected_exn (fun () -> list_max_exn l)

  let tests_list_max_exn =
    "tests for find_earliest_date function"
    >::: [
           make_test_exn "empty list" (Failure "list_max_exn: empty list") [];
           make_test "single item" 1 [ 1 ];
           make_test "first 2" 2 [ 2; 1 ];
           make_test "second 2" 2 [ 1; 2 ];
           make_test "third 5" 5 [ 1; 2; 5 ];
           make_test "first 5" 5 [ 5; 1; 2; 3 ];
         ]

  let test () = run_test_tt_main tests_list_max_exn
end
