(* Exercise: matrix multiply [★★★★]

   Implement a function multiply_matrices: int list list -> int list list -> int list list for matrix multiplication.
   If the two input matrices are not of sizes that can be multiplied together, the behavior is unspecified.
   Unit test the function.
   Hint: define functions for matrix transposition and row vector dot product. *)

exception DotProductFailure

exception EmptyMatrix

let dot_product r1 r2 =
  try [ List.map2 ( * ) r1 r2 |> List.fold_left ( + ) 0 ]
  with _ -> raise DotProductFailure

let transpose (m : int list list) =
  match m with
  | [] -> raise EmptyMatrix
  | first_row :: _ ->
      let new_height = List.length first_row in
      let new_row_size = List.length m in
      List.init new_height (fun prev_elem_idx ->
          List.init new_row_size (fun prev_row_idx ->
              List.nth (List.nth m prev_row_idx) prev_elem_idx))

let multiply_matrices m1 m2 : int list list =
  let m2_transposed = transpose m2 in
  List.map
    (fun row -> List.map (dot_product row) m2_transposed |> List.flatten)
    m1

module Test = struct
  open OUnit2

  let make_test test_name expected_output (m1, m2) =
    test_name >:: fun _ ->
    assert_equal expected_output (multiply_matrices m1 m2)

  let make_test_exn test_name expected_exn (m1, m2) =
    test_name >:: fun _ ->
    assert_raises expected_exn (fun () -> multiply_matrices m1 m2)

  let identity_matrix = [ [ 1; 0; 0 ]; [ 0; 1; 0 ]; [ 0; 0; 1 ] ]

  let three_x_three_matrix = [ [ 1; 2; 3 ]; [ 4; 5; 6 ]; [ 7; 8; 9 ] ]

  let tests =
    "tests for multiply_matrices function"
    >::: [
           make_test "valid, 1x3 * 3x1" [ [ 32 ] ]
             ([ [ 1; 2; 3 ] ], [ [ 4 ]; [ 5 ]; [ 6 ] ]);
           make_test "valid, 3x1 * 1x3"
             [ [ 4; 8; 12 ]; [ 5; 10; 15 ]; [ 6; 12; 18 ] ]
             ([ [ 4 ]; [ 5 ]; [ 6 ] ], [ [ 1; 2; 3 ] ]);
           make_test "valid, 2x2 * 2x2" [ [ -16; 20 ]; [ 16; 2 ] ]
             ([ [ 2; -2 ]; [ 5; 3 ] ], [ [ -1; 4 ]; [ 7; -6 ] ]);
           make_test "valid, first identity" three_x_three_matrix
             (identity_matrix, three_x_three_matrix);
           make_test "valid, second identity" three_x_three_matrix
             (three_x_three_matrix, identity_matrix);
           make_test_exn "invalid, second matrix must have 3 rows"
             DotProductFailure
             ([ [ 1; 2; 3 ] ], [ [ 4 ]; [ 5 ]; [ 6 ]; [ 7 ] ]);
           make_test_exn "invalid, first matrix is empty" DotProductFailure
             ([ [] ], [ [ 1; 2; 3 ] ]);
           make_test_exn "invalid, second matrix is empty" EmptyMatrix
             ([ [ 1; 2; 3 ] ], []);
         ]

  let test () = run_test_tt_main tests
end
