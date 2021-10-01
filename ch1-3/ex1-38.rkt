#lang sicp

(define (average a b c) (/ (+ a b c) 3.0))

(define (cont-frac n d k)
  (define (f count result)
    (if (< count 1)
        result
        (f (- count 1) (/ (n count) (+ (d count) result)))))
  (f k 0))


(+ 2 (cont-frac (lambda (i) 1.0)
                (lambda (i)
                  (if (= (remainder i 3) 2)
                      (* 2 (/ (+ i 1) 3))
                      1))
                30))
; 2.7182818284590455

