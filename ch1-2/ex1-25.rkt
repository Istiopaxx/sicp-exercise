#lang sicp

(define (square x) (* x x))
(define (even? x) (= (remainder x 2) 0))
(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* b a)))))
  (fast-expt-iter b n 1))

; Alyssa P.Hacker는 expmod를 쓰느라 손이 아프다고 한다. 그냥 밑에처럼 쓰면 안되냐?
; 밑에가 과연 맞을까? 이전에 쓰던거처럼 같이 잘 동작할까? 설명해보자. 

(define (prime?-fast n)
  (define (fast-prime? n times)
    (define (expmod base exp m)
      (remainder (fast-expt base exp) m))
    (define (fermat-test n)
      (define (try-it a)
        (= (expmod a n n) a))
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0 ) #t)
          ((fermat-test n) (fast-prime? n (- times 1)))
          (else #f)))
  (fast-prime? n 30))

(prime?-fast 10007) ; 2초정도 걸려서 #t
(prime?-fast 1000000007) ; 10분째 기다려도 안끝나는중;;;;;;

; 결론 : 큰 수에서는 잘 동작하지 않는다. 
; 왜인지 설명하자면, 스킴은 부동소수점을 쓰기 때문에 매우 큰 수도 가능하긴 하다. 그럼 매우 큰 수에 대해 
; remainder 연산은 오래 걸리는 것 같다.그게 아니면 30번 틀리면 혹은 그 전에라도 틀리면 바로 나와야 되는데
; 안나오는 중. 