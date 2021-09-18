#lang sicp

;; 뉴튼 메소드로 제곱근을 구해 보자.
;; x의 제곱근에 가까운 값 near가 있을 때 near와 x / near의 평균을 구하여
;; 제곱근에 가까워지는 방식이다. 즉 계속해서 평균을 구하면 구할수록 그 값은
;; 제곱근의 정확한 값에 가까워지게 된다.

(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x) (* x x))

(define (average a b)
  (/ (+ a b) 2))


(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))


(define (improve guess x)
  (average guess (/ x guess)))


(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))


(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))
