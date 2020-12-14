#lang plait

(define-type Exp
  (numE [n : Number])
  (plusE [l : Exp]
         [r : Exp])
  (multE [l : Exp]
         [r : Exp]))

(define (interp [a : Exp]) : Number
  (type-case Exp a
    [(numE n) n]
    [(plusE l r) (+ (interp l) (interp r))]
    [(multE l r) (* (interp l) (interp r))]))

(test (interp (numE 2))
      2)
(test (interp (plusE (numE 2) (numE 1)))
      3)
(test (interp (multE (numE 2) (numE 1)))
      2)
(test (interp (plusE (multE (numE 2) (numE 3))
                     (plusE (numE 5) (numE 8))))
      19)
