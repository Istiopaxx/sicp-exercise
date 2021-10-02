#lang sicp


(define tolerance 0.00001)
(define (abs x) (if (< x 0) (- x) x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)
(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))
(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

(define (cubic a b c)
  (define (cube x) (* x x x))
  (define (square x) (* x x))
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))

(newton-method (cubic 2 3 4) 1)
; -1.6506291914330982   <- 결과 값
; −1.65062919143939...  <- 계산기로 계산해본 값