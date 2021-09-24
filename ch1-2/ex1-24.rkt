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

; 1-22 예제에서 했던 것을 페르마 검사를 이용하여 비교해보자. fast-prime? 프로시저를 이용하면 된다.

(define (prime?-fast n)
  (define (fast-prime? n times)
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
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0 ) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))
  (fast-prime? n 30))



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
(search-for-primes 10000000 3 prime?-slow)
(search-for-primes 100000000 3 prime?-slow)
(search-for-primes 1000000000 3 prime?-slow)
(newline)
(display " =========================== ")
(newline)
(display " prime?-fast ")
(search-for-primes 10000000 3 prime?-fast)
(search-for-primes 100000000 3 prime?-fast)
(search-for-primes 1000000000 3 prime?-fast)
(newline)
(display " =========================== ")
(newline)
(display " prime?-fast 1000 & 1000000 ")
(search-for-primes 1000000000 1 prime?-fast)
(search-for-primes 1000000 1 prime?-fast)

; prime?-slow
; 10000019 *** 159
; 10000079 *** 161
; 10000103 *** 249
; 100000007 *** 534
; 100000037 *** 522
; 100000039 *** 858
; 1000000007 *** 2678
; 1000000009 *** 2675
; 1000000021 *** 1942
; ===========================
; prime?-fast
; 10000019 *** 189
; 10000079 *** 158
; 10000103 *** 221
; 100000007 *** 187
; 100000037 *** 172
; 100000039 *** 198
; 1000000007 *** 208
; 1000000009 *** 260
; 1000000021 *** 365
; ===========================
; prime?-fast 1000000 & 1000000000
; 1000000007 *** 229
; 1000003 *** 174

; prime?-slow는 n이 10배 증가할때마다 sqrt(10n) => sqrt(n) * sqrt(10)≒3배정도씩 증가하고 있지만,
; prime?-fast는 n이 10배 증가할 때마다 log(10n) => log(n) + log(10)≒1(밑이 10이라고 하자)므로 상수시간
; 만큼이 더해지고 있다. 따라서 prime?-fast는 n이 10배 증가해도 차이가 거의 동일하게 상수이다.
; prime?-fast의 1000000 과 1000000000 사이의 차이도 상수시간이 3번 더해진 것과 비슷하다. 대략
; 10배 증가할때 20~40ms가 증가하는 것 같고, 1000배 증가했으므로 60~120ms가 증가했다고 보면 예상대로다.
