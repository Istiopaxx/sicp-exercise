#lang sicp

; Exercise 2.10.  Ben Bitdiddle, an expert systems programmer, looks over Alyssa's shoulder and 
; comments that it is not clear what it means to divide by an interval that spans zero. Modify
; Alyssa's code to check for this condition and to signal an error if it occurs.


(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))
(define (div-interval x y)
  (define (printErr)
    (display "Error! Cannot divide by zero")
    (newline))
  (cond ((or (= (upper-bound y) 0)
             (= (lower-bound y) 0))
         (printErr))
        (else (mul-interval x
                            (make-interval (/ 1.0 (upper-bound y))
                                           (/ 1.0 (lower-bound y)))))))


(define a (make-interval 1 5))
(define b (make-interval 4 8))
(define c (make-interval 0 4))
(define d (make-interval -6 0))

(div-interval a b)  ; (0.125 . 1.25)
(div-interval a c)  ; Error! Cannot divide by zero
(div-interval a d)  ; Error! Cannot divide by zero

