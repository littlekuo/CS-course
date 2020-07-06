#lang plait

 (define-type Tree
    (leaf [val : Number])
    (node [val : Number]
          [left : Tree]
          [right : Tree]))
;part1
(define (sum [x : Tree])
  (type-case Tree x
    [(leaf val) val]
    [(node val left right) (+ val (+ (sum left) (sum right)))]))

(module+ test
  (test (sum (node 5 (leaf 6) (leaf 7))) 18)
  (test (sum (leaf 59)) 59))


;part2
(define (neg x)
  (- 0 x))

(define (negate [x : Tree])
  (type-case Tree x
    [(leaf val) (leaf (neg val))]
    [(node val left right) (node (neg val)  (negate left) (negate right))]))

(module+ test
  (test (negate (node 5 (leaf 6) (leaf 7))) (node -5 (leaf -6) (leaf -7))))


;part3
(define (contains? [x : Tree] y)
  (type-case Tree x
    [(leaf val) (equal? val y)]
    [(node val left right) (or (equal? val y) (or (contains? left y) (contains? right y)))]))

(module+ test
  (test (contains? (node 5 (leaf 6) (leaf 7)) 6) #t)
  (test (contains? (node 5 (leaf 6) (leaf 7)) 8) #f))


;part4

(define (big-leaves? [x : Tree])
  (local [(define (bigger-leaves? [t : Tree] sum)
            (type-case Tree  t
              [(leaf val) (> val sum)]
              [(node val left right) [and (bigger-leaves? left (+ sum val)) (bigger-leaves? right (+ sum val))]]))]
  (bigger-leaves? x 0)))

(module+ test
  (test (big-leaves? (node 5 (leaf 6) (leaf 7))) #t)
  (test  (big-leaves? (node 5 (node 2 (leaf 8) (leaf 6)) (leaf 7)))  #f))


;part5
(define (positive-trees? [l : (Listof Tree)])
  (type-case (Listof Tree) l
    [empty  #t]
    [(cons t l) (and (> (sum t) 0) (positive-trees? l))]))

(module+ test
  (test (positive-trees? (cons (leaf 6)
                               empty))
        #t)
  (test (positive-trees? (cons (leaf -6)
                               empty))
        #f)

  (test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               empty))
        #t)

  (test (positive-trees? (cons (node 1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1))
                                      empty)))
        #t)

  (test (positive-trees? (cons (node -1 (leaf 6) (leaf -6))
                               (cons (node 0 (leaf 0) (leaf 1))
                                      empty)))
        #f))

;part6
(define (flatten-helper? [t : Tree] acc)
  (type-case Tree t
    [(leaf v) (cons v acc)]
    [(node val l r) (flatten-helper? l (cons val (flatten-helper? r acc)))]))

(define (flatten [t : Tree])
  (flatten-helper? t empty))

(module+ test
  (test (flatten (leaf 6)) (cons 6 empty))
  (test (flatten (node -1 (leaf 6) (leaf -6))) (list 6 -1 -6))
  (test (flatten (node 22 (node 5 (leaf 4) (leaf 45)) (leaf 23))) (list 4 5 45 22 23)))
             