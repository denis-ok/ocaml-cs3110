(* Exercise: twice, no arguments [★]
   Consider the following definitions: *)

let double x = 2 * x

let square x = x * x

let twice f x = f (f x)

let quad = twice double

let fourth = twice square

(* Use the toplevel to determine what the types of quad and fourth are.
   Explain how it can be that quad is not syntactically written
   as a function that takes an argument, and yet its type shows that
   it is in fact a function. *)

(* Answer: Because of partial application / currying. *)

(* Exercise: mystery operator 1 [★★]
   What does the following operator do?
   Hint: investigate square $ 2 + 2 vs. square 2 + 2. *)

let ( $ ) f x = f x

(* Answer: It's function application operator.
   Sometimes can be useful instead of parenthesis. *)

(* Exercise: mystery operator 2 [★★]
   What does the following operator do? *)

let ( @@ ) f g x = x |> g |> f

(* Answer: It's composition operator. Composes f and g to apply f after g. *)
