(* Exercise: depth [★★]

   Write a function depth : 'a tree -> int that returns the number of nodes
   in any longest path from the root to a leaf. For example,
   the depth of an empty tree (simply Leaf) is 0,
   and the depth of tree t above is 3.
   Hint: there is a library function max : 'a -> 'a -> 'a
   that returns the maximum of any two values of the same type. *)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

let t =
  Node
    ( 4,
      Node (2, Node (1, Leaf, Leaf), Node (3, Leaf, Leaf)),
      Node (5, Node (6, Leaf, Leaf), Node (7, Leaf, Leaf)) )

let depth (tree : 'a tree) : int =
  let rec depth tree =
    match tree with
    | Leaf -> 0
    | Node (_, left, right) -> 1 + max (depth left) (depth right)
  in
  depth tree

module Test = struct
  open OUnit2

  let tree0 = Leaf

  let tree1 = Node ((), Leaf, Leaf)

  let tree2 = Node ((), tree1, tree1)

  let tree3 = Node ((), tree2, tree2)

  let tree4 = Node ((), tree3, Leaf)

  let tree4' = Node ((), Leaf, tree3)

  let make_test test_name expected_output input =
    test_name >:: fun _ -> assert_equal expected_output (depth input)

  let tests_depth =
    "tests for depth function"
    >::: [
           make_test "single leaf" 0 tree0;
           make_test "one" 1 tree1;
           make_test "two" 2 tree2;
           make_test "three" 3 t;
           make_test "three" 3 tree3;
           make_test "four" 4 tree4;
           make_test "four" 4 tree4';
         ]

  let test () = run_test_tt_main tests_depth
end
