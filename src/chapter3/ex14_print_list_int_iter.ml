(* Exercise: print int list iter [★★]
   Write a function print_int_list' : int list -> unit whose specification is the same as print_int_list.
   Do not use the keyword rec in your solution, but instead to use the List module function List.iter.
   Here is some code to get you started: *)

let print_int_list (l : int list) =
  List.iter (fun v -> print_endline (string_of_int v)) l
