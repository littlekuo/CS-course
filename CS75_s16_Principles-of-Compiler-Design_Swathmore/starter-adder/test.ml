open Compile
open Runner
open Printf
open OUnit2

let t name program expected = name>::test_run program name expected;;
let te name program expected_err = name>::test_err program name expected_err;;

let suite =
"suite">:::
 [
  t "nu" "let x = 5, y = sub1(x) in sub1(y)" "3"; 
  t "nu1" "let x = 10 in x" "10";
  t "nu2" "sub1(add1(sub1(4)))" "3";
  t "nu3" "5" "5";
  t "nu5" "let x=100 in let x=1 in x" "1";
  t "nu6" "let x = 5 in add1(x)" "6";
  t "nu7" "let x=1 in let y=add1(x) in y" "2";
  t "nu8" "let x = 5, y = sub1(x) in sub1(y)" "3";
  t "nu9" "let x=1 in let x=add1(let x=6 in add1(x)) in x" "8";
  t "nu10" "let y=sub1(add1(sub1(let x=5 in x))) in add1(add1(add1(y)))" "7";

  te "err1" " let x=1 in z" "not found id";
  te "err2" "let x=10, y=20, x=5, y=30, x=40 in x" "here is a binding list containing two ";
  
   ]
;;


let () =
  run_test_tt_main suite
;;
