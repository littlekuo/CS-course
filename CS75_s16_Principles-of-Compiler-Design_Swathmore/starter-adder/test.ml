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

  t "nyi" "let x = 10 in x" "10";
  
  

  ]
;;


let () =
  run_test_tt_main suite
;;
