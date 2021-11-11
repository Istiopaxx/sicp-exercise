#lang sicp

; Exercise 2.14.  Demonstrate that Lem is right. Investigate the behavior of the system on a
; variety of arithmetic expressions. Make some intervals A and B, and use them in computing
; the expressions A/A and A/B. You will get the most insight by using intervals whose width
; is a small percentage of the center value. Examine the results of the computation in
; center-percent form (see exercise 2.12).

; ==================================================
; define interval data and operation
(define (make-interval l u) (cons l u))
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

; ==================================================
; define percent interval
(define (make-center-percent center percent-tolerance)
  (cons center percent-tolerance))
(define (center x) (car x))
(define (percent-tolerance x) (cdr x))


; ==================================================
; define changer
(define (percent-to-interval p)
  (let ((width (* (center p) (/ (percent-tolerance p) 100.0))))
    (make-interval (- (center p) width) (+ (center p) width))))

(define (interval-to-percent i)
  (let ((center (/ (+ (upper-bound i) (lower-bound i)) 2.0))
        (width (/ (- (upper-bound i) (lower-bound i)) 2.0)))
    (make-center-percent center (* (/ width center) 100.0))))



; ==================================================
; define interval A, B, C, D

(define a (percent-to-interval (make-center-percent 100 0.01)))
(define b (percent-to-interval (make-center-percent 100 0.1)))
(define c (percent-to-interval (make-center-percent 100 1)))
(define d (percent-to-interval (make-center-percent 100 10)))

; calculate A/A, B/B, C/C, and D/D
(div-interval a a) ; (0.9998000199980001 . 1.0002000200020003)
(div-interval b b) ; (0.9980019980019981 . 1.002002002002002)
(div-interval c c) ; (0.9801980198019802 . 1.0202020202020203)
(div-interval d d) ; (0.8181818181818181 . 1.2222222222222223)

; calculate A/D
(div-interval a d) ; (0.9089999999999999 . 1.1112222222222223)

; calculate one/one
(define one (percent-to-interval (make-center-percent 100 0)))
(div-interval one one) ; (1.0 1.0)


; div-interval
; => R1 / R2
; => R1 * (1 / R2)
