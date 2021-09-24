#lang sicp


(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))

(define (prime?-slow n)
  (define (smallest-divisor n)
    (define (divides? a b) (= (remainder b a) 0))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (+ test-divisor 1)))))
    (find-divisor n 2))
  (= (smallest-divisor n) n))

; 앞에서 해본 smallest-divisor는 2,3,4,5,6,7,8,9,... 순으로 소수인지 확인하는데, 2로 나누어 떨어지는 수는
; 굳이 확인할 필요가 없다. 따라서 4, 6, 8, ...는 보지 않도록 바꿔보자. smallest-divisor 프로시저가
; (+ test-divisor 1) eotls (next test-divisor)를 쓰도록 next 프로시저를 정의하자.

(define (prime?-fast n)
  (define (smallest-divisor n)
    (define (next n)
      (if (= n 2)
          3
          (+ n 2)))
    (define (divides? a b) (= (remainder b a) 0))
    (define (find-divisor n test-divisor)
      (cond ((> (square test-divisor) n) n)
            ((divides? test-divisor n) test-divisor)
            (else (find-divisor n (next test-divisor)))))
    (find-divisor n 2))
  (= (smallest-divisor n) n))

; 이제 1-22 예제에서 했었던 작업을 다시 해보자. 방금 한 최적화가 계산 단계를 반으로 줄이므로, 2배 빨라질거라
; 예상할 수 있다. 실제로 2배 빨라지는지, 아니라면 빨라진 비율이 계산하고 비율이 왜 2배가 아닌지 설명해보자.

(define (search-for-primes n counter prime?)
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
  (define (s-f-p n counter)
    (if (> counter 0)
        (if (timed-prime-test n)
            (s-f-p (+ n 2) (- counter 1))
            (s-f-p (+ n 2) counter))))
  (if (even? n)
      (s-f-p (+ n 1) counter)
      (s-f-p n counter)))

(display " prime?-slow ")
(search-for-primes 100000000 3 prime?-slow)
(search-for-primes 1000000000 3 prime?-slow)
(search-for-primes 10000000000 3 prime?-slow)
(newline)
(display " =========================== ")
(newline)
(display " prime?-fast ")
(search-for-primes 100000000 3 prime?-fast)
(search-for-primes 1000000000 3 prime?-fast)
(search-for-primes 10000000000 3 prime?-fast)

; 결과
; prime?-slow
; 100000007 *** 503
; 100000037 *** 518
; 100000039 *** 524
; 1000000007 *** 1620
; 1000000009 *** 1613
; 1000000021 *** 1640
; 10000000019 *** 5977
; 10000000033 *** 5362
; 10000000061 *** 5919
; ===========================
; prime?-fast
; 100000007 *** 419
; 100000037 *** 260
; 100000039 *** 266
; 1000000007 *** 898
; 1000000009 *** 865
; 1000000021 *** 856
; 10000000019 *** 2873
; 10000000033 *** 2962
; 10000000061 *** 3203


; 거의 일관되게 2배는 안나오고, 좀 들쭉날쭉하다. 커뮤니티 솔루션을 참고해보니 대략 1.5배가 나오고 그 이유는
; next 프로시저에서 if 연산때문이라고 하는데, 내 결과는 몇번을 시도해봐도 일관되게 1.5배가 나오지 않는다.
; 아마도 최적화가 적용되어 거의 2배로 나오는 것이 아닐까 생각해본다. 