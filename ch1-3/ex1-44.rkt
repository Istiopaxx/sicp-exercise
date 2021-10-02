#lang sicp

(define dx 0.01)
(define (average a b c) (/ (+ a b c) 3.0))

(define (smooth f)
  (lambda (x)
    (average (f (+ x dx))
             (f (- x dx))
             (f x))))


(define (abs x) (if (< x 0) (- x ) x))
((smooth abs) 0.0001)     ; 0.0067
(abs 0.0001)              ; 0.0001



(define (compose f g)
  (lambda (x)
    (f (g x))))

(define (repeated f n)
  (define (iter count result)
    (if (< count 2)
        result
        (iter (- count 1) (compose f result))))
  (iter n f))

(define (smooth-n f n)
  (repeated (smooth f) n))

((smooth-n abs 10) 0.0001)  ; 0.009999832342630697
(abs 0.0001)                ; 0.0001