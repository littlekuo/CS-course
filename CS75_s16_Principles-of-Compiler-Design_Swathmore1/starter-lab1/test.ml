open OUnit2
open Avl
open Expr
open ExtLib

(* A helper for testing primitive values (won't print datatypes well) *)
let t_any name value expected = name>::
  (fun _ -> assert_equal expected value ~printer:dump);;

(* Feel free to add any new testing functions you may need *)



let a_tree = Node(0, "a", 5, Leaf, Leaf);;

let b_tree = Node (1, "a", 5, Leaf, Node(0, "c", 10, Leaf, Leaf));;

(* It can be useful to aggregate tests into lists if they test separate
functions, and put them together at the end *)

let get_tests = [
  t_any "get1" (get a_tree "a") (Some(5));
  t_any "get2" (get (Node(1, "b", 15, a_tree, Leaf)) "a") (Some(5));
  t_any "get3" (get (Node(1, "b", 15, a_tree, Leaf)) "c") None;
];;

let set_tests = [
  t_any "set1" (set a_tree "a" 10) (Node(0, "a", 10, Leaf, Leaf));
  t_any "set2" (set a_tree "c" 10) (Node (1, "a", 5, Leaf, Node(0, "c", 10, Leaf, Leaf)));
  t_any "set3" (set b_tree "d" 6) (Node (1, "c", 10, Node(0, "a", 5, Leaf, Leaf), Node(0, "d", 6, Leaf , Leaf)));
];;

let contains_tests = [
  t_any "contains1" (contains a_tree "c") false;
];;

let evaluate_tests = [
  t_any "evaluate1" (evaluate (Times(Num(0), Num(5))) Leaf) 0
];;

let pretty_tests = [
  t_any "prettytest1" (pretty (Plus(Plus(Times(Plus(Num(5), Variable("y")), Variable("x")), Num(2)), Num(1)))) "(5 + y)x + 2 + 1";
  t_any "prettytest2" (pretty (Plus(Times(Num(5), Num(3)), Variable("y")))) "5 * 3 + y";
  t_any "prettytest3" (pretty (Times(Num(1), Variable("x")))) "1x";
  t_any "prettytest4" (pretty (Times(Num(5), Plus(Variable("y"), Num(10))))) "5(y + 10)";
  t_any "prettytest5" (pretty (Times(Plus(Variable("y"), Num(10)), Num(5)))) "(y + 10) * 5";
];;

let all_tests =
  get_tests @
  set_tests @
  contains_tests @
  evaluate_tests @
  pretty_tests
;;


let suite = "suite">:::all_tests;;

run_test_tt_main suite

