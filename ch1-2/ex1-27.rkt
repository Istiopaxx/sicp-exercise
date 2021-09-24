#lang sicp


(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (define (test-iter count)
    (cond ((= count 0) #t)
          ((try-it count) (test-iter (- count 1)))
          (else #f)))
  (test-iter (- n 1)))


; 카마이클 수란, 페르마 검사를 통과하지만 소수가 아닌 수를 말한다. 가장 작은 카마이클 수 중 몇개는
; 561, 1105, 1729, 2465, 2821, 6601 같은 수들이 있다. 진짜로 페르마 검사를 통과하는지 확인해보자.
; 자연수 n을 입력받아 a<n인 모든 자연수 a에 대해 a^n mod n이 a와 같은지 확인하는 프로시저를 만들고,
; 카마이클 수를 넣어보자.

(fermat-test 561)  ; #t
(fermat-test 1105) ; #t
(fermat-test 1729) ; #t
(fermat-test 2465) ; #t
(fermat-test 2821) ; #t
(fermat-test 6601) ; #t


(define (prime?-slow n)
  (define (smallest-divisor n)
    (define (divides? a b) (= (remainder b a) 0))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= (smallest-divisor n) n))

(prime?-slow 561)  ; #f
(prime?-slow 1105) ; #f
(prime?-slow 1729) ; #f
(prime?-slow 2465) ; #f
(prime?-slow 2821) ; #f
(prime?-slow 6601) ; #f