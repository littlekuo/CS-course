#lang plait

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  ;part1
  (maxE [l : Exp]
        [r : Exp])
  ;part2
  (appE [s : Symbol]
        [arg : (Listof Exp)]))


(define-type Func-Defn
  (fd [name : Symbol] 
      [arg : (Listof Symbol)] 
      [body : Exp]))


(module+ test
  (print-only-errors #t))


;; parse ----------------------------------------
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
    ;part1
    [(s-exp-match? `{max ANY ANY} s)
     (maxE (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s))))]
    ;part2
    [(s-exp-match? `{SYMBOL ANY ...} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (map parse (rest (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL ...} ANY} s)
     ;part2 and part3
     (local [(define args (map s-exp->symbol (rest (s-exp->list (second (s-exp->list s))))))
             (define (two-elem-equal? ls)
               (cond
                 [(empty? ls) #f]
                 [(member (first ls) (rest ls)) #t]
                 [else (two-elem-equal? (rest ls))]))
                    
            ]
       (cond
         [(two-elem-equal? args) (error 'parse-fundef "bad syntax")]
         [else (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
                   args
                   (parse (third (s-exp->list s))))]))]
    [else (error 'parse-fundef "invalid input")]))

(module+ test
  (test (parse `2)
        (numE 2))
  (test (parse `x)
        (idE 'x))
  (test (parse `{+ 2 1})
        (plusE (numE 2) (numE 1)))
  (test (parse `{* 3 4})
        (multE (numE 3) (numE 4)))
  (test (parse `{+ {* 3 4} 8})
        (plusE (multE (numE 3) (numE 4))
               (numE 8)))
  (test (parse `{double 9})
        (appE 'double (list (numE 9))))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double (list 'x) (plusE (idE 'x) (idE 'x))))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (test/exn (parse-fundef `{define {f x x} x})
            "bad syntax")

  (test (parse-fundef `{define {f x y} x})
        (fd 'f (list 'x 'y) (idE 'x)))

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}})))

;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    ;part1
    [(maxE l r) (max (interp l defs) (interp r defs))]
    ;part2
    [(appE s arg) (local [(define fd (get-fundef s defs))]
                    (cond
                      ;determine whether the lengths are match 
                      [(equal? (length arg) (length (fd-arg fd)))
                               ;
                        (local [(define (subst-all actual-params formal-params exp)
                                  (cond
                                    [(empty? actual-params) exp]
                                    [else (subst-all (rest actual-params) (rest formal-params)
                                                   (subst (numE (interp (first actual-params) defs)) (first formal-params) exp))]))]
                          (interp (subst-all arg (fd-arg fd) (fd-body fd)) defs))]
                      [else (error 'interp "wrong arity")]))]))
                    
             

(module+ test
  (test (interp (parse `2) empty)
        2)
  (test/exn (interp (parse `x) empty)
            "free variable")
  (test (interp (parse `{+ 2 1}) empty)
        3)
  (test (interp (parse `{* 2 1}) empty)
        2)
  (test (interp (parse `{+ {* 2 3}
                           {+ 5 8}})
                empty)
        19)
  (test (interp (parse `{double 8})
                (list double-def))
        16)
  (test (interp (parse `{quadruple 8})
                (list double-def quadruple-def))
        32)
  (test (interp (parse `{max 1 2})
                (list))
        2)
  (test (interp (parse `{max {+ 4 5} {+ 2 3}})
                (list))
        9)
  ;part2
  (test (interp (parse `{f 1 2})
                (list (parse-fundef `{define {f x y} {+ x y}})))
        3)
  (test (interp (parse `{+ {f} {f}})
                (list (parse-fundef `{define {f} 5})))
        10)
  (test/exn (interp (parse `{f 1})
                    (list (parse-fundef `{define {f x y} {+ x y}})))
            "wrong arity"))


  

;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s (fd-name def))
                             def
                             (get-fundef s rst-defs))]))

(module+ test
  (test (get-fundef 'double (list double-def))
        double-def)
  (test (get-fundef 'double (list double-def quadruple-def))
        double-def)
  (test (get-fundef 'double (list quadruple-def double-def))
        double-def)
  (test (get-fundef 'quadruple (list quadruple-def double-def))
        quadruple-def)
  (test/exn (get-fundef 'double empty)
            "undefined function"))

;; subst ----------------------------------------
(define (subst [what :  Exp] [for : Symbol] [in : Exp])
  (type-case Exp in
    [(numE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    ;part1
    [(maxE l r) (maxE (subst what for l)
                      (subst what for r))]
    [(appE s arg) (local [(define (subst-per exp) (subst what for exp))]
                    (appE s (map subst-per arg)))]))

(module+ test
  (test (subst (parse `8) 'x (parse `9))
        (numE 9))
  (test (subst (parse `8) 'x (parse `x))
        (numE 8))
  (test (subst (parse `8) 'x (parse `y))
        (idE 'y))
  (test (subst (parse `8) 'x (parse `{+ x y}))
        (parse `{+ 8 y}))
  (test (subst (parse `8) 'x (parse `{* y x}))
        (parse `{* y 8}))
  (test (subst (parse `8) 'x (parse `{double x}))
        (parse `{double 8})))