#lang plait

;part1
(define (3rd-power x)
  (* x (* x x)))

(module+ test
  (test (3rd-power 17) 4913)
  (test (3rd-power 2) 8)
  (test (3rd-power 1) 1)
  (test (3rd-power 3) 27))

;part2
(define (42nd-power n)
 (local [(define power
           (lambda (n start end ans)
             (cond
               [(equal? start end) ans ]
               [else (power n (+ 1 start) end (* n ans))])))]
  (power n 0 42 1)))


(module+ test
  (test (42nd-power 1) 1)
  (test (42nd-power 17) 4773695331839566234818968439734627784374274207965089)
  (test (42nd-power 2) 4398046511104))


;part3
(define (plural x)
  (let ([len (string-length x)])
    (let ([a (string-ref x (- len 1))]
          [b (substring x 0 (- len 1))])
      (cond
        [(equal? a #\y) (string-append b "ies")]
        [else (string-append x "s")]))))


(module+ test
  (test (plural "fish") "fishs")
  (test (plural "baby") "babies")
  (test (plural "y") "ies")
  (test (plural "book") "books"))

;part4
(define-type Light
    (bulb [watts : Number]
          [technology : Symbol])
    (candle [inches : Number]))


(define (energy-usage x)
  (type-case Light x
    [(bulb watts tech) (/ (* 24 watts) 1000)]
    [(candle inches) 0.0]))

(module+ test
  (test (energy-usage (bulb 100.0 'halogen)) 2.4)
  (test (energy-usage (candle 10.0)) 0.0))

;part5
(define (use-for-one-hour x)
  (type-case Light x
    [(bulb watts tech) x]
    [(candle inches)
     (cond
       [(> (- inches 1.0) 0) (candle (- inches 1.0))]
       [else (candle 0.0)])]))

(module+ test
  (test (use-for-one-hour (bulb 100.0 'halogen)) (bulb 100.0 'halogen))
  (test (use-for-one-hour (candle 10.0)) (candle 9.0))
  (test (use-for-one-hour (candle 3.4)) (candle 2.4))
  (test (use-for-one-hour (candle 1.0)) (candle 0.0))
  (test (use-for-one-hour (candle 0.5)) (candle 0.0)))

