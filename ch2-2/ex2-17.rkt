#lang sicp

; Exercise 2.17.  Define a procedure last-pair that returns the list that contains only the last
; element of a given (nonempty) list:


(define (last-pair list1)
  (if (null? (cdr list1))
      (list (car list1))
      (last-pair (cdr list1))))

(last-pair (list 23 72 149 34)) ; (34)