(* Exercise: map composition [★★★]

   Show how to replace any expression of the form List.map f (List.map g lst)
   with an equivalent expression that calls List.map only once. *)

let list_map_compose f g = List.map (fun x -> f (g x))
