#lang sicp

;; 세 숫자를 인자로 받아 그 가운데 큰 숫자 두 개를 제곱한 다음,
;; 그 두 값을 덧셈하여 내놓는 프로시저를 정의하라.

(define (monkeyNandayoJoJo a b c)
  (define (square x) (* x x))
  (define (findNingen x y z)
    (cond ((and (> x y) (> y z)) x)
          ((and (> x z) (> z y)) x)
          ((and (> y z) (> z x)) y)
          ((and (> y x) (> x z)) y)
          ((and (> z x) (> x y)) z)
          ((and (> z y) (> y x)) z)))
  (define (findMonkey x y z)
    (cond ((and (> y x) (> x z)) x)
          ((and (> z x) (> x y)) x)
          ((and (> x y) (> y z)) y)
          ((and (> z y) (> y x)) y)
          ((and (> x z) (> z y)) z)
          ((and (> y z) (> z x)) z)))
  (+ (square (findNingen a b c)) (square (findMonkey a b c))))

(monkeyNandayoJoJo 1 2 3)
          