open OUnit2

let test_get_fifth_element test_name expected_output input =
  test_name >:: fun _ ->
  assert_equal expected_output
    (Ex6_library.get_fifth_element input)
    ~printer:string_of_int

let tests_get_fifth_element =
  "tests for get_fifth_element function"
  >::: [
         test_get_fifth_element "empty list" 0 [];
         test_get_fifth_element "5 elements" 5 [ 1; 2; 3; 4; 5 ];
       ]

let test_get_sort test_name expected_output input =
  test_name >:: fun _ ->
  assert_equal expected_output (Ex6_library.sort_desc input)

let tests_get_sort =
  "tests for get_sort function"
  >::: [
         test_get_sort "empty list" [] [];
         test_get_sort "1 element" [ 1 ] [ 1 ];
         test_get_sort "3 elements" [ 9; 5; 1 ] [ 1; 9; 5 ];
       ]

let test () =
  run_test_tt_main tests_get_fifth_element;
  run_test_tt_main tests_get_sort
