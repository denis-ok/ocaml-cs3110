(* Exercise: library uncurried [★★]

   Here is an uncurried version of List.nth:
   let uncurried_nth (lst, n) = List.nth lst n
   In a similar way, write uncurried versions of these library functions:

   List.append
   Char.compare
   Stdlib.max *)

let list_append (l1, l2) = List.append l1 l2

let char_compare (c1, c2) = Char.compare c1 c2

let max (v1, v2) = Stdlib.max v1 v2
