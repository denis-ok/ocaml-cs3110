(* take drop tail [★★★★] *)

(* Revise your solutions for take and drop to be tail recursive, if they aren’t already.
   Test them on long lists with large values of n to see whether they run out of stack space.
   To construct long lists, use the -- operator from the lists section. *)

let take_tail count l =
  let rec iter counter acc rest =
    match (counter, rest) with
    | 0, _ -> List.rev acc
    | _, [] -> List.rev acc
    | _, head :: tail -> iter (counter - 1) (head :: acc) tail
  in
  iter count [] l

let drop_tail = Ex9_take_drop.drop

module Test = struct
  open OUnit2

  (** [from i j l] is the list containing the integers from [i] to [j],
    inclusive, followed by the list [l].
    Example:  [from 1 3 [0] = [1; 2; 3; 0]] *)
  let rec from i j l = if i > j then l else from i (j - 1) (j :: l)

  (** [i -- j] is the list containing the integers from [i] to [j], inclusive. *)
  let ( -- ) i j = from i j []

  let big_list_size = 1_000_000

  let take_count = 500_000

  let test_take_tail test_name expected_output (input_count, input_list) =
    test_name >:: fun _ ->
    assert_equal expected_output (take_tail input_count input_list)

  let tests_take_tail =
    "tests for get_fifth_element function"
    >::: [
           test_take_tail "empty list" [] (1, []);
           test_take_tail "zero count" [] (0, [ 1 ]);
           test_take_tail "small list" [ 1; 2; 3 ] (3, [ 1; 2; 3; 4; 5 ]);
           test_take_tail "giant list" (1 -- take_count)
             (take_count, 1 -- big_list_size);
         ]

  let test () = run_test_tt_main tests_take_tail
end
