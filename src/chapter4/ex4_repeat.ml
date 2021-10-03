(* Exercise: repeat [★★]

   Generalize twice to a function repeat, such that repeat f n x applies f to x a total of n times. That is,
   repeat f 0 x yields x
   repeat f 1 x yields f x
   repeat f 2 x yields f (f x) (which is the same as twice f x)
   repeat f 3 x yields f (f (f x))
   … *)

let repeat f count x =
  let rec repeat_iter count result =
    match count with
    | 0 -> result
    | _ -> repeat_iter (count - 1) (f result)
  in
  repeat_iter count x
