#lang plait

;; Check whether a robot program has any U-turns,
;; where a U-turn is a left turn followed by a left
;; turrn (possibly with straight motion in between)
;; or a right turn followed by a right turn.

;; Writing this function demonstrates the need for
;; an accumulator, which accumulates information
;; about instructions seen so far --- i.e., accumulate
;; the "previous turn".

(print-only-errors #t)

(define-type Instruction
  (drive-forward [dist : Number])
  (turn-left)
  (turn-right))

;(define (how-far [ins : Instruction]) : Number
;  (type-case Instruction ins
;    [(drive-forward d) ... d]
;    [(turn-left) ...]
;    [(turn-right) ...]))

#;
(define-type (Listof X)
  empty
  (cons [f : X]
        [r : (Listof X)]))

;(define (total-distance [instrs : (Listof Instruction)]) : Number
;  (type-case (Listof Instruction) instrs
;    [empty ...]
;    [(cons f r) ...(how-far f) ... (total-distance r)...])

(define (is-uturn? [ins : Instruction] [pre : Symbol]) : Boolean
  (type-case Instruction ins
    [(drive-forward d) #f]
    [(turn-left) (symbol=? pre 'left)]
    [(turn-right) (symbol=? pre 'right)]))

(test (is-uturn? (drive-forward 3) 'left)
      #f)
(test (is-uturn? (drive-forward 3) 'right)
      #f)
(test (is-uturn? (drive-forward 3) 'none)
      #f)
(test (is-uturn? (turn-left) 'right)
      #f)
(test (is-uturn? (turn-left) 'left)
      #t)
(test (is-uturn? (turn-left) 'none)
      #f)
(test (is-uturn? (turn-right) 'left)
      #f)
(test (is-uturn? (turn-right) 'right)
      #t)
(test (is-uturn? (turn-right) 'none)
      #f)

(define (classify [ins : Instruction] [pre : Symbol]) : Symbol
  (type-case Instruction ins
    [(drive-forward d) pre]
    [(turn-left) 'left]
    [(turn-right) 'right]))

(test (classify (drive-forward 3) 'left)
      'left)
(test (classify (drive-forward 3) 'right)
      'right)
(test (classify (drive-forward 3) 'none)
      'none)
(test (classify (turn-left) 'left)
      'left)
(test (classify (turn-left) 'right)
      'left)
(test (classify (turn-left) 'none)
      'left)
(test (classify (turn-right) 'left)
      'right)
(test (classify (turn-right) 'right)
      'right)
(test (classify (turn-right) 'none)
      'right)

(define (has-uturns? [instrs : (Listof Instruction)] [pre : Symbol]) : Boolean
  (type-case (Listof Instruction) instrs
    [empty #f]
    [(cons f r) (or (is-uturn? f pre) (has-uturns? r (classify f pre)))])
  )

(test (has-uturns? empty 'left)
      #f)
(test (has-uturns? empty 'right)
      #f)
(test (has-uturns? empty 'none)
      #f)
(test (has-uturns? (cons (turn-left) empty) 'left)
      #t)
(test (has-uturns? (cons (turn-left) empty) 'right)
      #f)
(test (has-uturns? (cons (drive-forward 5) empty) 'right)
      #f)
(test (has-uturns? (cons (turn-left)
                         (cons (turn-left)
                               empty))
                   'none)
      #t)
(test (has-uturns? (cons (turn-right)
                         (cons (turn-right)
                               empty))
                   'none)
      #t)
(test (has-uturns? (cons (turn-left)
                         (cons (turn-right)
                               empty))
                   'none)
      #f)