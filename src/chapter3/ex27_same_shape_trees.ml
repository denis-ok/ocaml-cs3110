(* Exercise: shape [★★★]

   Write a function same_shape : 'a tree -> 'b tree -> bool that determines whether two trees have the same shape,
   regardless of whether the values they carry at each node are the same.
   Hint: use a pattern match with three branches, where the expression being matched is a pair of trees. *)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let rec same_shape (t1 : 'a tree) (t2 : 'a tree) : bool =
  match (t1, t2) with
  | Leaf, Leaf -> true
  | Leaf, Node _ | Node _, Leaf -> false
  | Node (_, left1, right1), Node (_, left2, right2) ->
      same_shape left1 left2 && same_shape right1 right2

module Test = struct
  open OUnit2

  let tree1 = Node ((), Leaf, Leaf)

  let tree2 = Node ((), tree1, tree1)

  let make_test test_name expected_output t1 t2 =
    test_name >:: fun _ -> assert_equal expected_output (same_shape t1 t2)

  let tests_same_shape =
    "tests for same_shape function"
    >::: [
           make_test "leaves" true Leaf Leaf;
           make_test "left leaf" false Leaf tree1;
           make_test "right leaf" false tree1 Leaf;
           make_test "same trees" true tree1 tree1;
           make_test "not same trees" false
             (Node ((), tree1, tree2))
             (Node ((), tree2, tree1));
         ]

  let test () = run_test_tt_main tests_same_shape
end
