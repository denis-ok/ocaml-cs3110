(* Exercise: values [★] *)
(* What is the type and value of each of the following OCaml expressions? *)
let (_ : int) = 7 * (1 + 2 + 3)

let (_ : string) = "CS " ^ string_of_int 3110

(* Exercise: operators [★★] *)
(* Write an expression that computes 4.2 raised to the seventh power.
   Note: there is no built-in integer exponentiation operator in OCaml (nor is there in C, by the way),
   in part because it is not an operation provided by most CPUs. *)
let _ = 42 * 10

(* Write an expression that divides 3.14 by 2.0.
   Hint: integer and floating-point operators are written differently in OCaml. *)
let _ = 3.14 /. 2.0

(* Write an expression that multiplies 42 by 10. *)
let _ = 4.2 ** 7.0

(* Exercise: equality [★] *)
(* Write an expression that compares 42 to 42 using structural equality. *)
let _ = 42 = 42

(* Write an expression that compares "hi" to "hi" using structural equality. What is the result? *)
let _ = "hi" = "hi"
(* true *)

(* Write an expression that compares "hi" to "hi" using physical equality. What is the result? *)
let _ = "hi" == "hi"
(* false *)

(* Exercise: assert [★] *)
(* Enter assert true;; into utop and see what happens. *)
let _ = assert true
(* - : unit = () *)

(* Enter assert false;; into utop and see what happens. *)
(* let _ = assert false *)
(* Exception: Assert_failure ("//toplevel//", 1, 0). *)

(* Write an expression that asserts 2110 is not (structurally) equal to 3110. *)
let _ = assert (2100 <> 3110)

(* Exercise: if [★] *)
(* Write an if expression that evaluates to 42 if 2 is greater than 1 and otherwise evaluates to 7. *)
let _ = if 2 > 1 then 42 else 7

(* Exercise: double fun [★] *)
(* Define a function double that multiplies its input by 2. For example, double 7 would be 14.
   Test your function by applying it to a few inputs.
   Turn those test cases into assertions. *)
let double x = x * 2

let test_double =
  assert (double 0 = 0);
  assert (double 1 = 2);
  assert (double 7 = 14);
  assert (double ~-3 = ~-6)

(* Exercise: more fun [★★] *)
(* Define a function that computes the cube of a floating-point number.
   Test your function by applying it to a few inputs. *)
let cube x = x *. x *. x

let test_cube =
  assert (cube 0. = 0.);
  assert (cube 1. = 1.);
  assert (cube (-1.) = -1.);
  assert (cube 2. = 8.)

(* Define a function that computes the sign (1, 0, or -1) of an integer.
   Use a nested if expression. Test your function by applying it to a few inputs. *)
let int_sign n = if n < 0 then -1 else if n = 0 then 0 else 1

let test_int_sign =
  assert (int_sign 0 = 0);
  assert (int_sign ~-1 = ~-1);
  assert (int_sign 1 = 1)

(* Define a function that computes the area of a circle given its radius.
   Test your function with assert. For the latter, bear in mind that floating-point arithmetic is not exact.
   Instead of asserting an exact value, you should assert that the result is “close enough”, e.g., within 1e-5. *)
let circle_area radius = Float.pi *. (radius *. radius)

(* tolerance is relaxed to 0.01 *)
let is_close_enough ref_value result = Float.abs (ref_value -. result) < 0.01

let test_circle_area =
  assert (circle_area 1. = Float.pi);
  assert (is_close_enough (circle_area 1.) 3.1415);
  assert (is_close_enough (circle_area 10.) 314.15);
  assert (is_close_enough (circle_area 2.) 12.566);
  assert (is_close_enough (circle_area 3.) 28.2735)

(* Exercise: RMS [★★]
   Define a function that computes the root mean square of two numbers. Test your function with assert. *)
let mean_root_square x y = sqrt (((x ** 2.) +. (y ** 2.)) /. 2.)

let test_mean_root_square =
  assert (mean_root_square 0. 0. = 0.);
  assert (mean_root_square 1. 1. = 1.);
  assert (mean_root_square 2. 2. = 2.);
  assert (mean_root_square 3. 3. = 3.);
  assert (is_close_enough (mean_root_square 1. 0.) 0.7);
  assert (is_close_enough (mean_root_square 2. 0.) 1.41)

(* Exercise: date fun [★★★]
   Define a function that takes an integer d and string m as input
   and returns true just when d and m form a valid date.
   Here, a valid date has a month that is one of the following abbreviations:
   Jan, Feb, Mar, Apr, May, Jun, Jul, Aug, Sept, Oct, Nov, Dec.
   And the day must be a number that is between 1 and the minimum number of days in that month, inclusive.
   For example, if the month is Jan, then the day is between 1 and 31, inclusive,
   whereas if the month is Feb, then the day is between 1 and 28, inclusive. *)
let is_valid_date m d =
  let valid month max_days = m = month && d >= 1 && d <= max_days in
  valid "Jan" 31 || valid "Feb" 28 || valid "Mar" 31 || valid "Apr" 30
  || valid "May" 31 || valid "Jun" 30 || valid "Jul" 31 || valid "Aug" 31
  || valid "Sept" 30 || valid "Oct" 31 || valid "Nov" 30 || valid "Dec" 31

let test_is_valid_date =
  assert (is_valid_date "Jan" 1);
  assert (is_valid_date "Jan" 31);
  assert (is_valid_date "Jan" 0 = false);
  assert (is_valid_date "Jan" 32 = false);
  assert (is_valid_date "J" 32 = false)

(* Exercise: fib [★★]
   Define a recursive function fib : int -> int, such that fib n is the nth number in the Fibonacci sequence, which is 1, 1, 2, 3, 5, 8, 13, … That is:
   fib 1 = 1,
   fib 2 = 1, and
   fib n = fib (n-1) + fib (n-2) for any n > 2.
   Test your function in the toplevel. *)
let rec fib n = if n = 1 || n = 2 then 1 else fib (n - 1) + fib (n - 2)

(* Exercise: fib fast [★★★]
   How quickly does your implementation of fib compute the 50th Fibonacci number?
   If it computes nearly instantaneously, congratulations!
   But the recursive solution most people come up with at first will seem to hang indefinitely.
   The problem is that the obvious solution computes subproblems repeatedly.
   For example, computing fib 5 requires computing both fib 3 and fib 4,
   and if those are computed separately, a lot of work (an exponential amount, in fact) is being redone.
   Create a function fib_fast that requires only a linear amount of work.
   Hint: write a recursive helper function h : int -> int -> int -> int, where h n pp p is defined as follows:
   h 1 pp p = p, and
   h n pp p = h (n-1) p (pp+p) for any n > 1.
   The idea of h is that it assumes the previous two Fibonacci numbers were pp and p,
   then computes forward n more numbers. Hence, fib n = h n 0 1 for any n > 0.
   What is the first value of n for which fib_fast n is negative, indicating that integer overflow occurred? *)
let fib_fast n =
  let rec iter counter pp p =
    if counter < n then iter (counter + 1) (pp + p) pp else pp + p
  in
  iter 2 1 0

let test_fib_fast =
  assert (fib_fast 1 = 1);
  assert (fib_fast 2 = 1);
  assert (fib_fast 3 = 2);
  assert (fib_fast 4 = 3);
  assert (fib_fast 5 = 5);
  assert (fib_fast 7 = 13);
  assert (fib_fast 50 = 12586269025)

let find_fib_int_overflow () =
  let rec find_fib_int_overflow n =
    if fib_fast n < 0 then n else find_fib_int_overflow (n + 1)
  in
  find_fib_int_overflow 1

let test_fib_in_overflow =
  assert (fib_fast 90 > 0);
  assert (fib_fast 91 < 0);
  assert (find_fib_int_overflow () = 91)

let check () = print_endline "Hello from Chapter 2 exercises"
