#lang sicp


(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))
; 이 프로시저는 선형 재귀 프로세스를 만든다. 다음처럼 선형 반복 프로세스로 바꿔보자.

(define (sum-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))

(define (inc n) (+ n 1))
(define (cube x) (* x x x))

(sum cube 1 inc 10)        ; 3025
(sum-iter cube 1 inc 10)   ; 3025