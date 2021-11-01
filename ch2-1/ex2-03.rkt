#lang sicp

; Exercise 2.3.
; Implement a representation for rectangles in a plane. (Hint: You may want to make use of
; exercise 2.2.) In terms of your constructors and selectors, create procedures that compute the
; perimeter and the area of a given rectangle. Now implement a different representation for
; rectangles. Can you design your system with suitable abstraction barriers, so that the same
; perimeter and area procedures will work using either representation?


; ==================================================
; define segment
(define (make-segment start-p end-p)
  (cons start-p end-p))
(define (start-segment segment)
  (car segment))
(define (end-segment segment)
  (cdr segment))
(define (calc-length segment)
  (sqrt (+ (expt (- (x-point (start-segment segment)) (x-point (end-segment segment))) 2)
           (expt (- (y-point (start-segment segment)) (y-point (end-segment segment))) 2))))


; ==================================================
; define point
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
(define (calc-distance start end)
  (sqrt (+ (expt (- (x-point start) (x-point end)) 2)
           (expt (- (y-point start) (y-point end)) 2))))


; ==================================================
; define rectangle - primary
(define (make-rect bottom-seg left-seg)
  (cons bottom-seg left-seg))
(define (width-rect rect)
  (calc-length (car rect)))
(define (height-rect rect)
  (calc-length (cdr rect)))


; ==================================================
; define rectangle - alter
(define (make-rect-alter bottom-left-p top-right-p)
  (cons bottom-left-p top-right-p))
(define (width-rect-alter rect)
  (calc-distance (car rect) (make-point (x-point (cdr rect)) (y-point (car rect)))))
(define (height-rect-alter rect)
  (calc-distance (make-point (x-point (cdr rect)) (y-point (car rect))) (cdr rect)))


; ==================================================
; define perimeter-rect and area-rect
(define (perimeter-rect rect)
  (* (+ (width-rect rect) (height-rect rect)) 2))
(define (area-rect rect)
  (* (width-rect rect) (height-rect rect)))

(define (perimeter-rect-alter rect)
  (* (+ (width-rect-alter rect) (height-rect-alter rect)) 2))
(define (area-rect-alter rect)
  (* (width-rect-alter rect) (height-rect-alter rect)))


; ==================================================
; calculate
(define RR (make-rect (make-segment (make-point 4 2) (make-point 1 2))
                      (make-segment (make-point 1 5) (make-point 4 5))))
(perimeter-rect RR)     ; 12
(area-rect RR)          ; 9 

(define RRR (make-rect-alter (make-point 1 2)
                             (make-point 4 5)))
(perimeter-rect-alter RRR)  ; 12
(area-rect-alter RRR)       ; 9 


