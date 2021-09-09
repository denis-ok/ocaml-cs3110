open OUnit2

let product = Ex2_product.product

let test_product test_name expected_output input =
  test_name >:: fun _ ->
  assert_equal expected_output (product input) ~printer:string_of_int

let tests =
  "tests for product function"
  >::: [
         test_product "empty list" 1 [];
         test_product "one element" 1 [ 1 ];
         test_product "two elements" 2 [ 1; 2 ];
         test_product "four elements" 24 [ 1; 2; 3; 4 ];
       ]

let test () = run_test_tt_main tests
