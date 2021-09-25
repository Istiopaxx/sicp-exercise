#lang sicp

(define (square x) (* x x))

(define (prime? n)
  (define (fast-prime? n times)
    (define (nontrivial-sqrt? a n)
      (if (and (and (not (= a 1))
                    (not (= a (- n 1))))
               (= (remainder (square a) n) 1))
          0
          (remainder (square a) n)))
    (define (expmod base exp m)
      (cond ((= exp 0) 1)
            ((even? exp)
             (nontrivial-sqrt? (expmod base (/ exp 2) m) m))
            (else
             (remainder (* base (expmod base (- exp 1) m))
                        m))))
    (define (miller-rabin-test n)
      (define (try-it a)
        (= (expmod a (- n 1) n) 1))
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0 ) #t)
          ((miller-rabin-test n) (fast-prime? n (- times 1)))
          (else #f)))
  (fast-prime? n 30))

(prime? 334153)     ; #f
; 카마이클 수로, 소수가 아니지만 페르마 검사를 통과한다. 밀러-라빈 검사는 통과하지 못한다.
(prime? 100000007)  ; #t 