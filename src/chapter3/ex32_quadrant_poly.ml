(* Exercise: quadrant poly [★★]
   Modify your definition of quadrant to use polymorphic variants. The types of your functions should become these:
   val sign : int -> [> `Neg | `Pos | `Zero ]
   val quadrant : int * int -> [> `I | `II | `III | `IV ] option
*)

let sign n = if n < 0 then `Neg else if n = 0 then `Zero else `Pos

let quadrant point =
  let x_sign, y_sign =
    let x, y = point in
    (sign x, sign y)
  in
  match (x_sign, y_sign) with
  | `Zero, _ | _, `Zero -> None
  | `Pos, `Pos -> Some `I
  | `Neg, `Pos -> Some `II
  | `Neg, `Neg -> Some `III
  | `Pos, `Neg -> Some `IIII

let quadrant_when point =
  match point with
  | x, y when x > 0 && y > 0 -> Some `I
  | x, y when x < 0 && y > 0 -> Some `II
  | x, y when x < 0 && y < 0 -> Some `III
  | x, y when x > 0 && y < 0 -> Some `IIII
  | _ -> None

module Test = struct
  open OUnit2

  let make_test func test_name expected_output point =
    test_name >:: fun _ -> assert_equal expected_output (func point)

  let make_tests func =
    "tests for quadrant function"
    >::: [
           make_test func "doesn't belong (0,0)" None (0, 0);
           make_test func "doesn't belong (1,0)" None (1, 0);
           make_test func "doesn't belong (0,1)" None (0, 1);
           make_test func "quadrant 1" (Some `I) (1, 1);
           make_test func "quadrant 2" (Some `II) (-1, 1);
           make_test func "quadrant 3" (Some `III) (-1, -1);
           make_test func "quadrant 4" (Some `IIII) (1, -1);
         ]

  let test () =
    run_test_tt_main (make_tests quadrant);
    run_test_tt_main (make_tests quadrant_when)
end
