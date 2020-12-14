#lang plait

(define-type Exp
   (numE [n : Number])
   (idE  [s : Symbol])
   (plusE [l : Exp][r : Exp])
   (multE [l : Exp][r : Exp])
   (appE [s : Symbol][arg : Exp])
   (letE [n : Symbol][rhs : Exp][body : Exp]))


(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `SYMBOL s) (idE (s-exp->symbol s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [(s-exp-match?`{let {[SYMBOL  ANY]} ANY} s)
       (let ([bs (s-exp->list (first (s-exp->list
                                        (second (s-exp->list s)))))])
         (letE  (s-exp->symbol (first bs)) (parse (second bs)) (parse (third (s-exp->list s)))))] 
    [else (error 'parse "invalid input")]))


