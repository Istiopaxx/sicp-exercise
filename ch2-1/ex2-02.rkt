#lang sicp

; Exercise 2.2.
; Consider the problem of representing line segments in a plane. Each segment is represented as a
; pair of points: a starting point and an ending point. Define a constructor make-segment and
; selectors start-segment and end-segment that define the representation of segments in terms of
; points. Furthermore, a point can be represented as a pair of numbers: the x coordinate and the
; y coordinate. Accordingly, specify a constructor make-point and selectors x-point and y-point
; that define this representation. Finally, using your selectors and constructors, define a procedure
; midpoint-segment that takes a line segment as argument and returns its midpoint
; (the point whose coordinates are the average of the coordinates of the endpoints).
; To try your procedures, you'll need a way to print points: -> print-point procedure.


; ==================================================
; define segment
(define (make-segment start-p end-p)
  (cons start-p end-p))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))

; ==================================================
; define point
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))

; ==================================================
; define midpoint-segment
(define (midpoint-segment segment)
  (make-point (/ (+ (x-point (start-segment segment)) (x-point (end-segment segment)))
                 2.0)
              (/ (+ (y-point (start-segment segment)) (y-point (end-segment segment)))
                 2.0)))

; ==================================================
; print result

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

(define a (make-point -11 4))
(define b (make-point 13 5))
(define ab (make-segment a b))
(define mid (midpoint-segment ab))

(print-point mid) ; (1.0,4.5)


