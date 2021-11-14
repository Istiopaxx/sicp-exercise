#lang sicp

; Exercise 2.18.  Define a procedure reverse that takes a list as argument and returns a list of
; the same elements in reverse order:



(define (append list1 list2)
  (if (null? list1)
      list2
      (cons (car list1) (append (cdr list1) list2))))

(define (reverse input-list)
  (define (reverse-iter input result)
    (if (null? input)
        result
        (reverse-iter (cdr input)
                      (cons (car input) result))))
  (reverse-iter input-list nil))


(reverse (list 1 4 9 16 25))
; (25 16 9 4 1)
(reverse nil)
; ()