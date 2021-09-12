(* Exercise: unimodal [★★★]

   Write a function is_unimodal : int list -> bool that takes an integer list
   and returns whether that list is unimodal. A unimodal list is a list that monotonically
   increases to some maximum value then monotonically decreases after that value.
   Either or both segments (increasing or decreasing) may be empty.
   A constant list is unimodal, as is the empty list. *)

let is_unimodal (l : int list) : bool =
  let rec iter is_increasing prev rest =
    match rest with
    | [] -> true
    | head :: tail ->
        if is_increasing && head >= prev then iter true head tail
        else if is_increasing && head < prev then iter false head tail
        else if head <= prev then iter false head tail
        else false
  in

  match l with
  | [] -> true
  | head :: tail -> iter true head tail

module Test = struct
  open OUnit2

  let make_test test_name expected_output input =
    test_name >:: fun _ -> assert_equal expected_output (is_unimodal input)

  let tests =
    "tests for is_unimodal function"
    >::: [
           make_test "empty list" true [];
           make_test "single elem" true [ 0 ];
           make_test "constant list" true [ 1; 1 ];
           make_test "increasing" true [ 0; 1 ];
           make_test "increasing" true [ 0; 1; 2; 10 ];
           make_test "decreasing" true [ 1; 0 ];
           make_test "decreasing" true [ 1; 0; -1 ];
           make_test "increasing,decreasing" true [ 0; 1; 2; 1; 0 ];
           make_test "increasing,decreasing" true
             [ 0; 0; 1; 2; 3; 3; 2; 2; 1; 0 ];
           make_test "falsy test 1" false [ 0; 1; 0; 1 ];
           make_test "falsy test 2" false [ 1; 0; 1 ];
         ]

  let test () = run_test_tt_main tests
end
