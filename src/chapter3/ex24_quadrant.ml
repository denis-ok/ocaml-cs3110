(* Exercise: quadrant [★★]

   Quadrant 1: x, and y both positive.
   Quadrant 2: x negative, y positive.
   Quadrant 3: both x and y negative.
   Quadrant 4: x positive, y negative.

   Complete the quadrant function below, which should return the quadrant of the given x,
   y point according to the diagram on the right (borrowed from Wikipedia).
   Points that lie on an axis do not belong to any quandrant.
   Hints: (a) define a helper function for the sign of an integer, (b) match against a pair. *)

type quadrant = One | Two | Three | Four

type sign = Negative | NoSign | Positive

let sign n = if n < 0 then Negative else if n = 0 then NoSign else Positive

let quadrant point : quadrant option =
  let x_sign, y_sign =
    let x, y = point in
    (sign x, sign y)
  in
  match (x_sign, y_sign) with
  | NoSign, _ | _, NoSign -> None
  | Positive, Positive -> Some One
  | Negative, Positive -> Some Two
  | Negative, Negative -> Some Three
  | Positive, Negative -> Some Four

module Test = struct
  open OUnit2

  let make_test test_name expected_output point =
    test_name >:: fun _ -> assert_equal expected_output (quadrant point)

  let tests_quadrant =
    "tests for quadrant function"
    >::: [
           make_test "doesn't belong (0,0)" None (0, 0);
           make_test "doesn't belong (1,0)" None (1, 0);
           make_test "doesn't belong (0,1)" None (0, 1);
           make_test "quadrant 1" (Some One) (1, 1);
           make_test "quadrant 2" (Some Two) (-1, 1);
           make_test "quadrant 3" (Some Three) (-1, -1);
           make_test "quadrant 4" (Some Four) (1, -1);
         ]

  let test () = run_test_tt_main tests_quadrant
end
