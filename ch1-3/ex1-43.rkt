#lang sicp

(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter count result)
    (if (< count 2)
        result
        (iter (- count 1) (compose f result))))
  (iter n f))

(define (square x) (* x x))

((repeated square 1) 5) ; 25      -> 5^2
((repeated square 2) 5) ; 625     -> 5^4
((repeated square 3) 5) ; 390625  -> 5^8
