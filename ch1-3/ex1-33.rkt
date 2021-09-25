#lang sicp

; filter라는 개념을 넣어서 accumulate의 쓰임을 넓히자. 어떤 조건을 만족하는 값만 묶고 나머지는 걸러낸다는
; 뜻이다. 이 프로시저를 filtered-accumulate라고 하자. 이 프로시저는 accumulate와 같은 인자를 받으며,
; filter로 쓸 술어 프로시저 predicate를 받아야 한다.

; 반복 프로세스로 짜보자.
(define (filtered-accumulate predicate combiner null-value term a next b)
  (define (iter a result)
    (cond ((> a b) result)
          ((predicate a) (iter (next a)
                               (combiner (term a) result)))
          (else (iter (next a) result))))
  (iter a null-value))

; 되도는 프로세스이다.
(define (filtered-accumulate-recur predicate combiner null-value term a next b)
  (cond ((> a b) null-value)
        ((predicate a) (combiner (term a) (filtered-accumulate-recur predicate combiner null-value
                                                                     term (next a) next b)))
        (else (filtered-accumulate-recur predicate combiner null-value term (next a) next b))))




; (prime? 프로시저가 있다 치고)a에서 b사이에 있는 모든 소수를 제곱하여 더해보자.
(define (prime? n)
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
  (if (= n 1)
      #f
      (= (smallest-divisor n) n)))
(define (square x) (* x x))
(define (inc n) (+ n 1))

(define (solve-a a b)
  (filtered-accumulate prime? + 0 square a inc b))

(solve-a 1 10) ; 87
; 1부터 10사이에 있는 수 중 소수를 제곱하여 더하면 2^2 + 3^2 + 5^2 + 7^2 = 87





; n과 서로소인 수, 즉 i<n이고 0보다 큰 정수 i가 gcd(i,n)=1이 되는 n보다 작고 0보다 큰 모든 자연수를
; 곱해보자.
(define (identity x) x)
(define (solve-b n)
  (define (coprime? i)
    (define (gcd x y)
      (if (= y 0)
          x
          (gcd y (remainder x y))))
    (= (gcd i n) 1))
  (filtered-accumulate coprime? * 1 identity 0 inc n))

(solve-b 24) ; 37182145
; 5*7*11*13*17*19*23
