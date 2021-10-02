#lang sicp

(define (square x) (* x x))

(define (cont-frac n d k)
  (define (f count result)
    (if (< count 1)
        result
        (f (- count 1) (/ (n count) (+ (d count) result)))))
  (f k 0))

(define (tan-cf x k)
  (/ (cont-frac (lambda (i)
                  (- (square x)))
                (lambda (i)
                  (- (* 2.0 i) 1))
                k)
     (- x)))

(tan-cf 10 25)
; 0.6483608274590856   <- 계산값
; 0.64836082745...     <- 실제 tan(10)
