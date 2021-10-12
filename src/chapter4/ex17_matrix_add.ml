(* Exercise: matrix add [★★★]

   Implement a function add_matrices: int list list -> int list list -> int list list for matrix addition.
   If the two input matrices are not the same size, the behavior is unspecified.
   Hint: there is an elegant one-line solution using List.map2 and add_row_vectors. Unit test the function. *)

exception DifferentSizeOfMatrices

let add_row_vectors = Ex16_row_vector_add.add_row_vectors

let matrix_add m1 m2 =
  match List.map2 (fun m1 m2 -> add_row_vectors m1 m2) m1 m2 with
  | matrix -> matrix
  | exception (Ex16_row_vector_add.DifferentSizeOfVectors as exn) -> raise exn
  | exception _ -> raise DifferentSizeOfMatrices

module Test = struct
  open OUnit2

  let make_test test_name expected_output (m1, m2) =
    test_name >:: fun _ -> assert_equal expected_output (matrix_add m1 m2)

  let make_test_exn test_name expected_exn (m1, m2) =
    test_name >:: fun _ ->
    assert_raises expected_exn (fun () -> matrix_add m1 m2)

  let tests =
    "tests for matrix_add function"
    >::: [
           make_test "valid, add two 1x1" [ [ 2 ] ] ([ [ 1 ] ], [ [ 1 ] ]);
           make_test "valid, add two 1x2" [ [ 4; 6 ] ]
             ([ [ 1; 2 ] ], [ [ 3; 4 ] ]);
           make_test "valid, add two 2x3"
             [ [ 6; -7; 8 ]; [ 11; 2; -3 ] ]
             ([ [ 1; -7; 5 ]; [ 0; 3; -10 ] ], [ [ 5; 0; 3 ]; [ 11; -1; 7 ] ]);
           make_test_exn "Different size of vectors"
             Ex16_row_vector_add.DifferentSizeOfVectors
             ([ [ 1 ] ], [ [ 1; 1 ] ]);
           make_test_exn "Different size of matrices" DifferentSizeOfMatrices
             ([ [ 1 ] ], [ [ 1 ]; [ 1 ] ]);
         ]

  let test () = run_test_tt_main tests
end
