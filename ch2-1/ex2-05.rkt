#lang sicp

; Exercise 2.5.
; Show that we can represent pairs of nonnegative integers using only numbers and arithmetic
; operations if we represent the pair a and b as the integer that is the product 2^a * 3^b. Give the
; corresponding definitions of the procedures cons, car, and cdr.

; ==================================================
; (a, b) pair can be represented by one integer, 2^a * 3^b.
; define cons

(define (my-cons a b)
  (* (expt 2 a) (expt 3 b)))

(define (get-exp base num)
  (define (get-exp-iter num count)
    (if (= (remainder num base) 0)
        (get-exp-iter (/ num base) (+ count 1))
        count))
  (get-exp-iter num 0))

(define (my-car x)
  (get-exp 2 x))
(define (my-cdr x)
  (get-exp 3 x))


; ==================================================
; usage

(define RR (my-cons 3 5))
(my-car RR)     ; 3
(my-cdr RR)     ; 5

