#lang sicp

; Exercise 2.21.  The procedure square-list takes a list of numbers as argument and returns a list
; of the squares of those numbers.

; Here are two different definitions of square-list.
; Complete both of them by filling in the missing expressions:

(define (square-list-recur items)
  (if (null? items)
      nil
      ; (cons <??> <??>)))
      (cons ((lambda (x) (* x x)) (car items)) (square-list (cdr items)))))

(define (square-list items)
  ; (map <??> <??>))
  (map (lambda (x) (* x x)) items))


(square-list-recur (list 1 2 3 4))
; (1 4 9 16)

(square-list (list 1 2 3 4))
; (1 4 9 16)