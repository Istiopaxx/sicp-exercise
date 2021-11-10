#lang sicp

; Exercise 2.9.  The width of an interval is half of the difference between its upper and lower
; bounds. The width is a measure of the uncertainty of the number specified by the interval.
; For some arithmetic operations the width of the result of combining two intervals is a function
; only of the widths of the argument intervals, whereas for others the width of the combination is
; not a function of the widths of the argument intervals. Show that the width of the sum
; (or difference) of two intervals is a function only of the widths of the intervals being added
; (or subtracted). Give examples to show that this is not true for multiplication or division.


(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (lower-bound y))
                 (- (upper-bound x) (upper-bound y))))
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
(define (calc-width x)
  (/ (- (upper-bound x) (lower-bound x)) 2.0))



; ==================================================
; define make-interval only width
(define (make-interval-width width) (cons 0 width))
(define (middle x) (car x))
(define (width x) (cdr x))



; ==================================================
; define add, subtract with only width
(define (add-interval-width a b)
  (make-interval-width (+ (width a) (width b))))
(define (sub-interval-width a b)
  (make-interval-width (- (width a) (width b))))

; test
(define a (make-interval 1 5))
(define b (make-interval 4 10))
(define aw (make-interval-width 2.0))
(define bw (make-interval-width 3.0))

(calc-width (add-interval a b))     ; 5.0
(width (add-interval-width aw bw))  ; 5.0
(calc-width (sub-interval a b))     ; -1.0 
(width (sub-interval-width aw bw))  ; -1.0 
; add and sub can implemented only with width


; ==================================================
; define multiply, division with only width
(define (mul-interval-width a b)
  (make-interval-width (* (width a) (width b))))
(define (div-interval-width a b)
  (make-interval-width (/ (width a) (width b))))

; test
(define c (make-interval 1 5))
(define d (make-interval -1 5))
(define cw (make-interval-width 2.0))
(define dw (make-interval-width 3.0))

(calc-width (mul-interval a b))     ; 23.0
(width (mul-interval-width aw bw))  ; 6.0
(calc-width (div-interval a b))     ; 0.575
(width (div-interval-width aw bw))  ; 0.6666666666666666
; mul and div cannot implemented only with width