#lang sicp

; Define a better version of make-rat that handles both positive and negative arguments. Make-rat
; should normalize the sign so that if the rational number is positive, both the numerator and
; denominator are positive, and if the rational number is negative, only the numerator is negative.

; ==================================================
; define basic procedure
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
(define (abs x) (if (< x 0) (- x) x))

; ==================================================
; define rational number object
(define (make-rat n d)
  (let ((g (gcd n d)))
    (if (or (and (< n 0) (< d 0))
            (and (> n 0) (> d 0)))
        (cons (/ n g) (/ d g))
        (cons (- (abs (/ n g))) (abs (/ d g))))))

(define (numer x) (car x))

(define (denom x) (cdr x))


; define rational number operation
(define (add-rat x y)
  (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (sub-rat x y)
  (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
            (* (denom x) (denom y))))

(define (mul-rat x y)
  (make-rat (* (numer x) (numer y))
            (* (denom x) (denom y))))

(define (div-rat x y)
  (make-rat (* (numer x) (denom y))
            (* (denom x) (numer x))))

(define (equal-rat? x y)
  (= (* (numer x) (denom y))
     (* (numer y) (denom x))))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

; ==================================================
; print negative rational num and perform operation

(define minus-one-half (make-rat 1 -2))
(define minus-one-third (make-rat -1 3))
(define one-half (make-rat 1 2))

(print-rat minus-one-half)      ; -1/2
(print-rat minus-one-third)     ; -1/3
(print-rat one-half)            ; 1/2 

(print-rat (add-rat minus-one-half minus-one-third))    ; -5/6
(print-rat (mul-rat minus-one-half minus-one-third))    ; 1/6 
(print-rat (add-rat minus-one-third minus-one-third))   ; -2/3

