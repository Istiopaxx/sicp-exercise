#lang sicp
(#%require sicp-pict)

; Exercise 2.45.

; Right-split and up-split can be expressed as instances of a general splitting operation.
; Define a procedure split with the property that evaluating.

; Produces procedures right-split and up-split with the same behaviors as the ones already defined.

(define (split first second)
  (define (f painter n)
    (if (= n 0)
        painter
        (let ((smaller (f painter (- n 1))))
           (first painter (second smaller smaller)))))
  f)

(define right-split (split beside below))
(define up-split (split below beside))

(paint (right-split einstein 4))
(paint (up-split einstein 4))