#lang plait

(define-type Exp
  (numE [n : Number])
  (plusE [l : Exp]
         [r : Exp])
  (multE [l : Exp]
         [r : Exp]))


(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `{+ ANY ANY} s)
     (plusE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(test (parse `2)
      (numE 2))
(test (parse `{+ 2 1})
      (plusE (numE 2) (numE 1)))
(test (parse `{* 3 4})
      (multE (numE 3) (numE 4)))
(test (parse `{+ {* 3 4} 8})
      (plusE (multE (numE 3) (numE 4))
             (numE 8)))

(define (interp [a : Exp]) : Number
  (type-case Exp a
    [(numE n) n]
    [(plusE l r) (+ (interp l) (interp r))]
    [(multE l r) (* (interp l) (interp r))]))

(test (interp (parse `2))
      2)
(test (interp (parse `{+ 2 1}))
      3)
(test (interp (parse `{* 2 1}))
      2)
(test (interp (parse `{+ {* 2 3}
                         {+ 5 8}}))
      19)