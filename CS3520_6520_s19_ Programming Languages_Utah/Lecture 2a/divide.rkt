#lang plait

;; Add division
;; Add absolute value
;; Change addition to allow any number of arguments

(print-only-errors #t)

(define-type Exp
  (numE [n : Number])
  (plusE [l : (Listof Exp)])
  (multE [l : Exp]
         [r : Exp])
  (divE [l : Exp]
        [r : Exp])
  (absE [e : Exp]))

(define (parse-list [l : (Listof S-Exp)]) : (Listof Exp)
  (type-case (Listof S-Exp) l
    [empty empty]
    [(cons f r) (cons (parse f) (parse-list r))]))

(module+ test
  (test (parse-list empty)
        empty)
  (test (parse-list (cons `3 empty))
        (cons (numE 3) empty)))

;; An EXP-S-EXP is either
;; - `NUMBER
;; - `{+ EXP-S-EXP EXP-S-EXP}
;; - `{* EXP-S-EXP EXP-S-EXP}
;; - `{/ EXP-S-EXP EXP-S-EXP}
;; - `{abs EXP-S-EXP}

(define (parse [s : S-Exp]) : Exp
  (cond
    [(s-exp-match? `NUMBER s) (numE (s-exp->number s))]
    [(s-exp-match? `{+ ANY ...} s)
     ;; (plusE (parse-list (rest (s-exp->list s))))
     (plusE (map parse (rest (s-exp->list s))))]
    [(s-exp-match? `{* ANY ANY} s)
     (multE (parse (second (s-exp->list s)))
            (parse (third (s-exp->list s))))]
    [(s-exp-match? `{/ ANY ANY} s)
     (divE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    [(s-exp-match? `{abs ANY} s)
     (absE (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(test (parse `2)
      (numE 2))
(test (parse `{+ 2 1})
      (plusE (list (numE 2) (numE 1))))
(test (parse `{* 3 4})
      (multE (numE 3) (numE 4)))
(test (parse `{+ {* 3 4} 8})
      (plusE (list (multE (numE 3) (numE 4))
                   (numE 8))))
(test (parse `{/ 3 1})
      (divE (numE 3) (numE 1)))
(test (parse `{abs -3})
      (absE (numE -3)))

(define (interp-list [l : (Listof Exp)]) : Number
  (type-case (Listof Exp) l
    [empty 0]
    [(cons f r) (+ (interp f) (interp-list r))]))

(module+ test
  (test (interp-list empty)
        0)
  (test (interp-list (list (numE 7) (numE 0) (numE -1)))
        6))

(define (interp [a : Exp]) : Number
  (type-case Exp a
    [(numE n) n]
    [(plusE l) (interp-list l)]
    [(multE l r) (* (interp l) (interp r))]
    [(divE l r) (/ (interp l) (interp r))]
    [(absE e) (local [(define n (interp e))]
                (if (< n 0)
                  (- 0 n)
                  n))]))

(test (interp (parse `2))
      2)
(test (interp (parse `{+ 2 1}))
      3)
(test (interp (parse `{* 2 1}))
      2)
(test (interp (parse `{+ {* 2 3}
                         {+ 5 8}}))
      19)
(test (interp (parse `{/ 2 1}))
      2)
(test (interp (parse `{abs -3}))
      3)
(test (interp (parse `{abs 3}))
      3)
(test (interp (parse `{+ 1 2 3}))
      6)
(test (interp (parse `{+}))
      0)