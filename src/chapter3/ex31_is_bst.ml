(* Exercise: is_bst [★★★★] *)

(* Write a function is_bst : ('a*'b) tree -> bool
   that returns true if and only if the given tree satisfies the binary search tree invariant.
   An efficient version of this function that visits each node at most once
   is somewhat tricky to write.
   Hint: write a recursive helper function that
   takes a tree and either gives you
   (i) the minimum and maximum value in the tree, or
   (ii) tells you that the tree is empty, or
   (iii) tells you that the tree does not satisfy the invariant.
   Your is_bst function will not be recursive,
   but will call your helper function and pattern match on the result.
   You will need to define a new variant type for the return type of your helper function. *)

type 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

type helper_result = MinMax of int * int | Empty | NotBST

let is_value_in_range v (min, max) = v > min && v < max

let rec bst_helper (tree : int tree) min_max =
  let min_value, max_value = min_max in
  match tree with
  | Leaf -> Empty
  | Node (value, left_tree, right_tree) when is_value_in_range value min_max
    -> (
      match
        ( bst_helper left_tree (min_value, value),
          bst_helper right_tree (value, max_value) )
      with
      | Empty, Empty -> MinMax (value, value)
      | MinMax (left_min, _), Empty -> MinMax (left_min, value)
      | Empty, MinMax (_, right_max) -> MinMax (value, right_max)
      | MinMax (left_min, _), MinMax (_, right_max) ->
          MinMax (left_min, right_max)
      | NotBST, _ | _, NotBST -> NotBST)
  | _ -> NotBST

let is_bst (tree : int tree) =
  match bst_helper tree (min_int, max_int) with
  | NotBST -> false
  | Empty -> true
  | MinMax (_min, _max) -> true

module Test = struct
  open OUnit2

  let make_test test_name expected_output input =
    test_name >:: fun _ ->
    assert_equal expected_output (bst_helper input (min_int, max_int))

  let tests_bst_helper =
    "tests for bst_helper function"
    >::: [
           (* Valid BST *)
           make_test "empty" Empty Leaf;
           make_test "MinMax 1, 1" (MinMax (1, 1)) (Node (1, Leaf, Leaf));
           make_test "MinMax 7, 9"
             (MinMax (7, 9))
             (Node (8, Node (7, Leaf, Leaf), Node (9, Leaf, Leaf)));
           make_test "MinMax 1, 14"
             (MinMax (1, 14))
             (Node
                ( 8,
                  Node
                    ( 3,
                      Node (1, Leaf, Leaf),
                      Node (6, Node (4, Leaf, Leaf), Node (7, Leaf, Leaf)) ),
                  Node (10, Leaf, Node (14, Node (13, Leaf, Leaf), Leaf)) ));
           (* Not BST *)
           make_test "not valid, same left value" NotBST
             (Node (8, Node (8, Leaf, Leaf), Node (9, Leaf, Leaf)));
           make_test "not valid, same right value" NotBST
             (Node (8, Node (7, Leaf, Leaf), Node (8, Leaf, Leaf)));
           make_test "not valid, left subtree value is bigger than value" NotBST
             (Node (8, Node (9, Leaf, Leaf), Node (10, Leaf, Leaf)));
           make_test
             "not valid, value 9 in left subtree bigger than parent value 7"
             NotBST
             (Node
                (8, Node (7, Leaf, Node (9, Leaf, Leaf)), Node (11, Leaf, Leaf)));
           make_test
             "not valid, right subtree value 9 is smaller than parent value 10"
             NotBST
             (Node (10, Node (7, Leaf, Leaf), Node (9, Leaf, Leaf)));
           make_test
             "not valid, right subtree value 6 is smaller than parent value 8 \
              and smaller than value 7 in left subtree"
             NotBST
             (Node (8, Node (7, Leaf, Leaf), Node (6, Leaf, Leaf)));
           make_test
             "not valid, value 6 in right subtree is smaller than value 7 in \
              left subtree "
             NotBST
             (Node
                (8, Node (7, Leaf, Leaf), Node (11, Node (6, Leaf, Leaf), Leaf)));
           make_test "not valid, deep value 15 in left subtree" NotBST
             (Node
                ( 7,
                  Node
                    ( 3,
                      Node (1, Node (0, Leaf, Leaf), Node (2, Leaf, Leaf)),
                      Node (6, Node (4, Leaf, Node (16, Leaf, Leaf)), Leaf) ),
                  Node
                    ( 12,
                      Node
                        ( 9,
                          Node (8, Leaf, Leaf),
                          Node (11, Node (10, Leaf, Leaf), Leaf) ),
                      Node (13, Leaf, Node (15, Node (14, Leaf, Leaf), Leaf)) )
                ));
           make_test "not valid, deep value 1 in right subtree" NotBST
             (Node
                ( 7,
                  Node
                    ( 3,
                      Node (1, Node (0, Leaf, Leaf), Node (2, Leaf, Leaf)),
                      Node (6, Node (4, Leaf, Node (5, Leaf, Leaf)), Leaf) ),
                  Node
                    ( 12,
                      Node
                        ( 9,
                          Node (8, Leaf, Leaf),
                          Node (11, Node (10, Leaf, Leaf), Leaf) ),
                      Node (13, Leaf, Node (15, Node (1, Leaf, Leaf), Leaf)) )
                ));
         ]

  let test () = run_test_tt_main tests_bst_helper
end
