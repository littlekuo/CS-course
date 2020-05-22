open Avl

(*

  This is a datatype that describes simple algebra expressions.

  For example, the expression

    5 * 3 + y
  
  would be represented as

    Plus(Times(Num(5), Num(3)), Variable("y"))

  Note that the multiplication is inside the addition because the order of
  operations demands that we do it first.


  Here are some other examples:

    1 + x               Plus(Num(1), Variable("x"))
    x + x               Plus(Variable("x"), Variable("x"))
    5(y + 10)           Times(Num(5), Plus(Variable("y"), Num(10)))

*)

type arith =
  | Plus of arith * arith
  | Times of arith * arith
  | Variable of string
  | Num of int


(*
  First, write evaluate, which takes an arithmetic expression and 
  an avl tree mapping from strings to integers, and evaluate the expression,
  using the given integer value for the variable.
  
  For example
  
     evaluate
       (Times(Plus(Variable("x"), Variable("y")), Num(5)))
       (add_all Leaf [("x", 5); ("y", 7)])

  should produce 60, and

     evaluate (Plus(Num(4), Num(5))) Leaf

  should produce 9.
  
  If there is a variable not contained in vars in the expression, throw an
  exception with failwith.

  You can test most of this without implementing avlnodes fully by simply
  testing expressions that don't have any variables.

*)

let rec evaluate (a : arith) (vars : (string, int) avlnode) : int =
    match a with
    | Num i -> i
    | Plus (a1, a2) -> (evaluate a1 vars) + (evaluate a2 vars)
    | Times (a1, a2) -> (evaluate a1 vars) * (evaluate a2 vars)
    | Variable x -> match (get vars x) with
                      | None -> failwith("no varible" ^ x ^ "in vars")
                      | Some y -> y
    

(*
  Next, write pretty, whilch takes an arithmetic expression and renders it in
  mathematical notation.

  It should print with minimal parentheses, assuming standard order of
  operations where multiplication binds more tightly than addition.

  Further, if there is a multiplication of a variable, it should be
  pretty-printed by putting the coefficient adjacent, for example:

    pretty (Plus(Plus(Times(Plus(Num(5), Variable("y")), Variable("x")), Num(2)), Num(1)))
  
  should pretty-print as

    (5 + y)x + 2 + 1

  HINT: it may be helpful to write a helper that keeps track of whether the
  current expression is part of of plus or times expression as an additional
  argument.

  NOTE: I expect lots of questions about "how pretty" your solution "has" to
  be.  See how well you can do – I'm not giving a formal specification of
  exactly what form you need to produce, though examples like the one above
  should work nicely.  There are several reasonable answers here.
*)

let rec pretty (a : arith) : string =
    let rec helper (a: arith) : string =
      match a with 
       | Plus(_) -> "(" ^ pretty a ^ ")"
       | _ -> pretty a
      in
    match a with
    | Plus(l, r) -> pretty l ^ " + " ^ pretty r
    | Variable var -> var
    | Num num -> string_of_int num
    | Times(l, r) ->   match r with
                       | Num _ -> helper l ^ " * " ^  helper r
                       | _ -> helper l ^ helper r


