#lang sicp

; ==================================================
; Exercise 2.7.  Alyssa's program is incomplete because she has not specified the implementation of
; the interval abstraction. Here is a definition of the interval constructor:

(define (make-interval a b) (cons a b))

; Define selectors upper-bound and lower-bound to complete the implementation.
(define (upper-bound i) (cdr i))
(define (lower-bound i) (car i))


; ==================================================
; Exercise 2.8.  Using reasoning analogous to Alyssa's, describe how the difference of two intervals
; may be computed. Define a corresponding subtraction procedure, called sub-interval.

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (mul-interval x
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

; ==================================================
; define sub-interval 
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))

