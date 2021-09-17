(* Exercise: earliest date [★★★]
   Write a function earliest : (int*int*int) list -> (int * int * int) option.
   It evaluates to None if the input list is empty, and to Some d if date d is the earliest date in the list.
   Hint: use is_before.
   As in the previous exercise, your function needs to work correctly only for dates,
   not for arbitrary date-like triples. *)

exception InvalidDate of (int * int * int)

let is_date_before = Ex19_date_before.is_date_before

let find_earliest_date (dates : (int * int * int) list) :
    (int * int * int) option =
  let rec find current_earliest rest =
    match rest with
    | [] -> Some current_earliest
    | head :: tail ->
        if is_date_before head current_earliest then find head tail
        else find current_earliest tail
  in
  match dates with
  | [] -> None
  | head :: tail -> find head tail

module Test = struct
  open OUnit2

  let make_test test_name expected_output dates =
    test_name >:: fun _ ->
    assert_equal expected_output (find_earliest_date dates)

  let make_test_exn test_name expected_exn dates =
    test_name >:: fun _ ->
    assert_raises expected_exn (fun () -> find_earliest_date dates)

  let tests_find_earliest_date =
    "tests for find_earliest_date function"
    >::: [
           make_test "empty list" None [];
           make_test "single date" (Some (2021, 09, 17)) [ (2021, 09, 17) ];
           make_test "two dates, first is earliest"
             (Some (2021, 09, 17))
             [ (2021, 09, 17); (2022, 09, 17) ];
           make_test "two dates, second is earliest"
             (Some (2022, 09, 17))
             [ (2023, 09, 17); (2022, 09, 17) ];
           make_test "three dates, first is earliest"
             (Some (2020, 06, 17))
             [ (2020, 06, 17); (2021, 06, 17); (2021, 09, 17) ];
           make_test "three dates, second is earliest"
             (Some (2021, 06, 17))
             [ (2021, 08, 17); (2021, 06, 17); (2021, 09, 17) ];
           make_test "three dates, third is earliest"
             (Some (2021, 09, 17))
             [ (2023, 08, 17); (2021, 09, 18); (2021, 09, 17) ];
           make_test_exn "contains invalid date"
             (Ex19_date_before.InvalidDate (0, 0, 0))
             [ (2023, 08, 17); (2021, 09, 18); (0, 0, 0) ];
         ]

  let test () = run_test_tt_main tests_find_earliest_date
end
