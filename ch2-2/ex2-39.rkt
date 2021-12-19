#lang sicp

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; Exercise 2.39.
; Complete the following definitions of reverse (exercise 2.18) in terms of fold-right and
; fold-left from exercise 2.38:

(define (reverse sequence)
  (fold-right (lambda (x y) (append y (list x)))
              nil
              sequence))
(define (reverse-l sequence)
  (fold-left (lambda (x y) (cons y x)) nil sequence))


(reverse (list 1 2 3 4))
; (4 3 2 1)
(reverse-l (list 1 2 3 4))
; (4 3 2 1)