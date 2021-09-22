#lang sicp


(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (smallest-divisor n)
  (define (divides? a b) (= (remainder b a) 0))
  (define (find-divisor n test-divisor)
    (cond ((> (square test-divisor) n) n)
          ((divides? test-divisor n) test-divisor)
          (else (find-divisor n (+ test-divisor 1)))))
  (find-divisor n 2))

(define (prime? n)
  (= (smallest-divisor n) n))



(define (timed-prime-test n)
  (define (report-prime elapsed-time)
    (newline)
    (display n)
    (display " *** ")
    (display elapsed-time))
  (define (start-prime-test n start-time)
    (if (prime? n)
        (report-prime (- (runtime) start-time))
        #f))
  (start-prime-test n (runtime)))


(define (search-for-primes n counter)
  (define (s-f-p n counter)
    (if (> counter 0)
        (if (timed-prime-test n)
            (s-f-p (+ n 2) (- counter 1))
            (s-f-p (+ n 2) counter))))
  (if (even? n)
      (s-f-p (+ n 1) counter)
      (s-f-p n counter)))


(search-for-primes 10000 3)
(search-for-primes 100000 3)
(search-for-primes 1000000 3)

; 10007 *** 1
; 10009 *** 4
; 10037 *** 1
; 100003 *** 5
; 100019 *** 5
; 100043 *** 5
; 1000003 *** 15
; 1000033 *** 18
; 1000037 *** 15


; 커뮤니티 솔루션을 조금 참고하였다. 10000, 100000, 1000000보다 큰 처음나오는 세 소수를 찾으라고 하여
; 머리를 싸고 생각해봤지만 명쾌하게 떠오르지 않고 오히려 두통이 생겨서 솔루션을 보았다.
; start-prime-test가 if 프로시저가 거짓일때 #f를 반환하여 s-f-p 프로시저에서 if 프로시저를 깔끔하게 만든게
; 인상적이다.
; 어쨌든 결과는 sqrt(10) ≒ 3이므로 거의 3배씩 늘어나는 것 같다. 즉 prime? 프로시저는 sqrt(n)의 자람차수를
; 가지는 것이 실험결과와 일치한다.