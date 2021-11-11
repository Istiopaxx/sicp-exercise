#lang sicp

; Exercise 2.12.  Define a constructor make-center-percent that takes a center and a percentage
; tolerance and produces the desired interval. You must also define a selector percent that
; produces the percentage tolerance for a given interval. The center selector is the same as the
; one shown above.

(define (make-center-percent center percent-tolerance)
  (cons center percent-tolerance))
(define (center x) (car x))
(define (percent-tolerance x) (cdr x))


; Exercise 2.13.  Show that under the assumption of small percentage tolerances there is a simple
; formula for the approximate percentage tolerance of the product of two intervals in terms of the
; tolerances of the factors. You may simplify the problem by assuming that all numbers are positive.

; percent-interval => pi
; old-interval => oi

; pi(a, p1) => oi(a - a*p1, a + a*p1)
; pi(b, p2) => oi(b - b*p2, b + b*p2)

; pi(c, p) = pi(a, p1) * pi(b, p2)
;         :=> oi((a-a*p1)*(b-b*p2), (a+a*p1)*(b+b*p2)) // all numbers are positive
; c => a*b*(1 + p1*p2) ≒ a*b        // p1, p2 are very small, so p1*p2 can ignore
; p => (p1+p2)/(1+p1*p2) ≒ p1+p2    // p1, p2 are very small, so p1*p2 can ignore

(define (mul-percent-interval a b)
  (make-center-percent (* (center a) (center b))
                       (+ (percent-tolerance a) (percent-tolerance b))))


(define a (make-center-percent 5.0 0.00002))
(define b (make-center-percent 2.0 0.0001))
(mul-percent-interval a b) ; (10.0 . 0.00012)

