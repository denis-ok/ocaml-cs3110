(* Exercise: valid matrix [★★★]

   A mathematical matrix can be represented with lists. In row-major representation, this matrix

   [191817]
   would be represented as the list [[1; 1; 1]; [9; 8; 7]].
   Let’s represent a row vector as an int list. For example, [9; 8; 7] is a row vector.

   A valid matrix is an int list list that has at least one row,
   at least one column,
   and in which every column has the same number of rows.
   There are many values of type int list list that are invalid, for example,

   []

   [[1; 2]; [3]]

   Implement a function is_valid_matrix: int list list -> bool that returns whether the input matrix is valid.
   Unit test the function. *)

let rec every f l =
  match l with
  | [] -> true
  | head :: tail -> if f head then every f tail else false

let is_valid_matrix (matrix : int list list) =
  match matrix with
  | [] -> false
  | row :: rows ->
      let width = List.length row in
      if width = 0 then false
      else every (fun row -> List.length row = width) rows

module Test = struct
  open OUnit2

  let make_test test_name expected_output input =
    test_name >:: fun _ -> assert_equal expected_output (is_valid_matrix input)

  let tests =
    "tests for is_valid_matrix function"
    >::: [
           make_test "valid 1x1" true [ [ 1 ] ];
           make_test "valid 1x2" true [ [ 1; 2 ] ];
           make_test "valid 2x2" true [ [ 1; 2 ]; [ 3; 4 ] ];
           make_test "valid 2x3" true [ [ 1; 2; 3 ]; [ 4; 5; 6 ] ];
           make_test "empty" false [];
           make_test "nested empty" false [ [] ];
           make_test "invalid, second row is bigger" false [ [ 1 ]; [ 2; 3 ] ];
           make_test "invalid, second row is smaller" false [ [ 1; 2 ]; [ 3 ] ];
         ]

  let test () = run_test_tt_main tests
end
