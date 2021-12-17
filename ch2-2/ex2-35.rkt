#lang sicp

; Exercise 2.35.
; Redefine count-leaves from section 2.2.2 as an accumulation:

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (count-leaves t)
  (accumulate + 0 (map (lambda (tree)
                         (if (pair? tree)
                             (count-leaves tree)
                             1))
                       t)))

(count-leaves (list 1 2 3 4))
; 4
(count-leaves (list 1 2 (list 3) 4))
; 4 
(count-leaves (list 1 (list) (list) (list) (list) (list) 7))
; 7 
(count-leaves (list 1 2 (list 3 4 (list 5 6) 7 (list 8 9) 10) 11 (list 12 13)))
; 13