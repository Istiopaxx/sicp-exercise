#lang sicp
(#%require sicp-pict)


; Exercise 2.48.

; A directed line segment in the plane can be represented as a pair of vectors -- the vector running
; from the origin to the start-point of the segment, and the vector running from the origin to the
; end-point of the segment.
; Use your vector representation from exercise 2.46 to define a representation for segments with a
; constructor make-segment and selectors start-segment and end-segment.


; ==================================================
; Define data abstraction of vector
(define (make-vect x y)
  (cons x y))
(define (xcor-vect v)
  (car v))
(define (ycor-vect v)
  (cdr v))

; ==================================================
; Define add-vect, sub-vect, scale-vect
(define (add-vect v1 v2)
  (make-vect (+ (xcor-vect v1) (xcor-vect v2))
             (+ (ycor-vect v1) (ycor-vect v2))))
(define (sub-vect v1 v2)
  (make-vect (- (xcor-vect v1) (xcor-vect v2))
             (- (ycor-vect v1) (ycor-vect v2))))
(define (scale-vect s v)
  (make-vect (* s (xcor-vect v))
             (* s (ycor-vect v))))


; ==================================================
; Define data abstraction of segment
(define (make-segment v-start v-end)
  (cons v-start v-end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))


