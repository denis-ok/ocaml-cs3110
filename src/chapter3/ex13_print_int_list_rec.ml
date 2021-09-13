(* Exercise: print int list rec [★★]
   Write a function print_int_list : int list -> unit that prints its input list, one number per line.
   For example, print_int_list [1; 2; 3] should result in this output: *)

let rec print_int_list (l : int list) =
  match l with
  | [] -> ()
  | head :: tail ->
      print_endline (string_of_int head);
      print_int_list tail
