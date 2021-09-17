(* Exercise: date before [★★]

   Define a date-like triple to be a value of type int * int * int.
   Examples of date-like triples include (2013, 2, 1) and (0, 0, 1000).
   A date is a date-like triple whose first part is a positive year (i.e., a year in the common era),
   second part is a month between 1 and 12, and third part is a day between 1 and 31 (or 30, 29, or 28,
   depending on the month and year). (2013, 2, 1) is a date; (0, 0, 1000) is not.

   Write a function is_before that takes two dates as input and evaluates to true or false.
   It evaluates to true if the first argument is a date that comes before the second argument.
   (If the two dates are the same, the result is false.)

   Your function needs to work correctly only for dates, not for arbitrary date-like triples.
   However, you will probably find it easier to write your solution
   if you think about making it work for arbitrary date-like triples.
   For example, it’s easier to forget about whether the input is truly a date,
   and simply write a function that claims (for example) that January 100, 2013 comes before February 34,
   2013—because any date in January comes before any date in February,
   but a function that says that January 100, 2013 comes after February 34,
   2013 is also valid. You may ignore leap years. *)

(* simplest functions for validations *)

exception InvalidDate of (int * int * int)

let is_valid_year year = year > 1000

let is_valid_month month = month >= 1 && month <= 12

let is_valid_day day = day >= 1 && day <= 31

let is_valid_date date =
  let year, month, day = date in
  if is_valid_year year && is_valid_month month && is_valid_day day then true
  else raise (InvalidDate date)

let is_date_before date1 date2 =
  let _ = is_valid_date date1 in
  let _ = is_valid_date date2 in
  if date1 <> date2 then
    let y1, m1, d1 = date1 in
    let y2, m2, d2 = date2 in

    if y2 > y1 then true
    else if m2 > m1 then true
    else if d2 > d1 then true
    else false
  else false

module Test = struct
  open OUnit2

  let make_test test_name expected_output date1 date2 =
    test_name >:: fun _ ->
    assert_equal expected_output (is_date_before date1 date2)

  let make_test_exn test_name expected_exn date1 date2 =
    test_name >:: fun _ ->
    assert_raises expected_exn (fun () -> is_date_before date1 date2)

  let tests_is_date_before =
    "tests for is_date_before function"
    >::: [
           make_test_exn "first date is invalid"
             (InvalidDate (0, 0, 0))
             (0, 0, 0) (2021, 9, 24);
           make_test_exn "second date is invalid"
             (InvalidDate (0, 0, 0))
             (2021, 9, 24) (0, 0, 0);
           make_test "same dates" false (2021, 9, 24) (2021, 9, 24);
           make_test "before 1 day" true (2021, 9, 23) (2021, 9, 24);
           make_test "before 1 month" true (2021, 7, 23) (2021, 8, 23);
           make_test "before 1 year" true (2020, 8, 23) (2021, 8, 23);
           make_test "before 1 year 1 month 1 day" true (2020, 7, 22)
             (2021, 8, 23);
         ]

  let test () = run_test_tt_main tests_is_date_before
end
