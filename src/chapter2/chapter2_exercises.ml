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
let _ = assert false
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
  assert (double 7 = 14);
  assert (double (-3) = -6)
