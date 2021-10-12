(* Exercise: row vector add [★★★]

   Implement a function add_row_vectors: int list -> int list -> int list
   for the element-wise addition of two row vectors.
   For example, the addition of [1; 1; 1] and [9; 8; 7] is [10; 9; 8].
   If the two vectors do not have the same number of entries, the behavior of your function is unspecified—that is,
   it may do whatever you like.
   Hint: there is an elegant one-line solution using List.map2. Unit test the function. *)

exception DifferentSizeOfVectors

let add_row_vectors v1 v2 =
  try List.map2 (fun a b -> a + b) v1 v2
  with _ -> raise DifferentSizeOfVectors

module Test = struct
  open OUnit2

  let make_test test_name expected_output (row1, row2) =
    test_name >:: fun _ ->
    assert_equal expected_output (add_row_vectors row1 row2)

  let make_test_exn test_name expected_exn (row1, row2) =
    test_name >:: fun _ ->
    assert_raises expected_exn (fun () -> add_row_vectors row1 row2)

  let tests =
    "tests for add_row_vectors function"
    >::: [
           make_test "valid, both rows are empty" [] ([], []);
           make_test "valid, rows with single element" [ 3 ] ([ 1 ], [ 2 ]);
           make_test "valid, rows with three elements" [ 10; 9; 8 ]
             ([ 1; 1; 1 ], [ 9; 8; 7 ]);
           make_test_exn "invalid, different row sizes" DifferentSizeOfVectors
             ([], [ 1 ]);
         ]

  let test () = run_test_tt_main tests
end
