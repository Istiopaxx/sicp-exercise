#lang sicp


(define (make-from-real-imag x y)
  (define (square x) (* x x))
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x) (square y))))
          ((eq? op 'angle) (atan y x))
          (else
           (error "Unknown op -- MAKE-FROM-REAL-IMAG" op))))
  dispatch)


; ==================================================
; Exercise 2.75.

; Implement the constructor make-from-mag-ang in message-passing style. This procedure should be
; analogous to the make-from-real-imag procedure given above.



(define (make-from-mag-ang r a)
  (define (dispatch op)
    (cond ((eq? op 'real-part) (* r (cos a)))
          ((eq? op 'imag-part) (* r (sin a)))
          ((eq? op 'magnitude) r)
          ((eq? op 'angle) a)
          (else
           (error "Unknown op -- MAKE-FROM-MAG-ANG" op))))
  dispatch)

(define (apply-generic op arg) (arg op))

; ==================================================
; Test.
(define polar (make-from-mag-ang 10 3))

(apply-generic 'real-part polar)
; -9.899924966004454
(apply-generic 'imag-part polar)
; 1.4112000805986722
(apply-generic 'magnitude polar)
; 10
(apply-generic 'angle polar)
; 3