#lang plait

;; Add `abs` expression form
;; Add `if0` expression form
;; Add `alias` definition form

(define-type Exp
  (numE [n : Number])
  (idE [s : Symbol])
  (plusE [l : Exp] 
         [r : Exp])
  (multE [l : Exp]
         [r : Exp])
  (appE [s : Symbol]
        [arg : Exp])
  (absE [a : Exp])
  (if0E [e : Exp]
        [t : Exp]
        [f : Exp]))

(define-type Func-Defn
  (fd [name : Symbol]
      [arg : Symbol] 
      [body : Exp])
  (alias [name : Symbol]
         [realname : Symbol]))

(module+ test
  (print-only-errors #t))

;; An EXP is either
;; - `NUMBER
;; - `SYMBOL
;; - `{+ EXP EXP}
;; - `{* EXP EXP}
;; - `{SYMBOL EXP}
;; - `{abs EXP}
  

;; A FUNC-DEFN is
;; - `{define {SYMBOL SYMBOL} EXP}

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
    [(s-exp-match? `{abs ANY} s)
     (absE (parse (second (s-exp->list s))))]
    [(s-exp-match? `{if0 ANY ANY ANY} s)
     (if0E (parse (second (s-exp->list s)))
           (parse (third (s-exp->list s)))
           (parse (fourth (s-exp->list s))))]
    [(s-exp-match? `{SYMBOL ANY} s)
     (appE (s-exp->symbol (first (s-exp->list s)))
           (parse (second (s-exp->list s))))]
    [else (error 'parse "invalid input")]))

(define (parse-fundef [s : S-Exp]) : Func-Defn
  (cond
    [(s-exp-match? `{define {SYMBOL SYMBOL} ANY} s)
     (fd (s-exp->symbol (first (s-exp->list (second (s-exp->list s)))))
         (s-exp->symbol (second (s-exp->list (second (s-exp->list s)))))
         (parse (third (s-exp->list s))))]
    [(s-exp-match? `{alias SYMBOL SYMBOL} s)
     (alias (s-exp->symbol (second (s-exp->list s)))
            (s-exp->symbol (third (s-exp->list s))))]
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
        (appE 'double (numE 9)))
  (test/exn (parse `{{+ 1 2}})
            "invalid input")
  (test (parse `{abs -4})
        (absE (numE -4)))
  (test (parse `{abs 5})
        (absE (numE 5)))

  (test (parse-fundef `{define {double x} {+ x x}})
        (fd 'double 'x (plusE (idE 'x) (idE 'x))))
  (test (parse-fundef `{alias x y})
        (alias 'x 'y))
  (test/exn (parse-fundef `{def {f x} x})
            "invalid input")

  (define double-def
    (parse-fundef `{define {double x} {+ x x}}))
  (define quadruple-def
    (parse-fundef `{define {quadruple x} {double {double x}}}))
  (define absolute-def
    (parse-fundef `{define {absolute x} {abs x}})))

;; interp ----------------------------------------
(define (interp [a : Exp] [defs : (Listof Func-Defn)]) : Number
  (type-case Exp a
    [(numE n) n]
    [(idE s) (error 'interp "free variable")]
    [(plusE l r) (+ (interp l defs) (interp r defs))]
    [(multE l r) (* (interp l defs) (interp r defs))]
    [(absE a) (local [(define val (interp a defs))]
                (if (< val 0)
                    (* -1 val)
                    val))]
    [(if0E e t f) (if (= 0 (interp e defs))
                      (interp t defs)
                      (interp f defs))]
    [(appE s arg) (local [(define def (get-fundef s defs))]
                    ;; Wish we had a function to handle
                    ;; all Func-Defns...
                    (apply-function
                     def
                     (interp arg defs)
                     defs))]))

(define (apply-function [def : Func-Defn]
                        [arg : Number]
                        [defs : (Listof Func-Defn)]) : Number
  (type-case Func-Defn def
    [(fd name arg-name body)
     (interp (subst (numE arg)
                    arg-name
                    body)
             defs)]
    [(alias name realname)
     (apply-function (get-fundef realname defs)
                     arg
                     defs)]))

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
  (test (interp (parse `{abs -6})
                empty)
        6)
  (test (interp (parse `{abs 8})
                empty)
        8)
  (test (interp (parse `{absolute -8})
                (list absolute-def))
        8)
  (test (interp (parse `{+ {f 42} {f 0}})
                (list (parse-fundef
                       `{define {f x}
                          {if0 x 2 3}})))
        5)
  (test (interp (parse `{x2 8})
                (list double-def
                      (parse-fundef `{alias x2 double})))
        16))
  

;; get-fundef ----------------------------------------
(define (get-fundef [s : Symbol] [defs : (Listof Func-Defn)]) : Func-Defn
  (type-case (Listof Func-Defn) defs
    [empty (error 'get-fundef "undefined function")]
    [(cons def rst-defs) (if (eq? s
                                  (type-case Func-Defn def
                                    [(fd name arg body) name]
                                    [(alias name realname) name]
                                    ))
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
(define (subst [what : Exp] [for : Symbol] [in : Exp])
  (type-case Exp in
    [(numE n) in]
    [(idE s) (if (eq? for s)
                 what
                 in)]
    [(plusE l r) (plusE (subst what for l)
                        (subst what for r))]
    [(multE l r) (multE (subst what for l)
                        (subst what for r))]
    [(absE a) (absE (subst what for a))]
    [(if0E e t f) (if0E (subst what for e)
                        (subst what for t)
                        (subst what for f))]
    [(appE s arg) (appE s (subst what for arg))]))


(module+ test
  (test (subst (numE 8) 'x (numE 9))
        (numE 9))
  (test (subst (numE 8) 'x (idE 'x))
        (numE 8))
  (test (subst (numE 8) 'x (idE 'y))
        (idE 'y))
  (test (subst (numE 8) 'x (plusE (idE 'x) (idE 'y)))
        (plusE (numE 8) (idE 'y)))
  (test (subst (numE 8) 'x (multE (idE 'y) (idE 'x)))
        (multE (idE 'y) (numE 8)))
  (test (subst (numE 8) 'x (appE 'double (idE 'x)))
        (appE 'double (numE 8)))
  (test (subst (numE 8) 'x (absE (idE 'x)))
        (absE (numE 8))))

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
