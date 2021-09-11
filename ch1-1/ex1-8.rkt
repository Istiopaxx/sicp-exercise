#lang sicp

;; 세제곱근을 구하는 뉴튼 메소드는 x의 세제곱근에 가까운 값을 near라고 할 때,
;; 다음 식에 따라 다음 가까운 값을 계산하는 방법이다.
;; (/ (+ (* 2 near) (/ x (* near near))) 3)
;; 세제곱근 프로시저를 짜보자.

(define (abs x)
  (if (< x 0) (- x) x))
(define (square x) (* x x))

(define (sqrt x)
  (define (good-enough? guess improved)
    (< (abs (- guess improved)) 0.000001))
  (define (improve guess)
    (/ (+ (/ x (square guess))
          (* 2 guess))
       3))
  (define (sqrt-iter guess)
    (if (good-enough? guess (improve guess))
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(sqrt 27) ;; 3.0000005410641766