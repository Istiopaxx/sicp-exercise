#lang sicp

; 아래처럼 a에서 b 사이에 있는 어떤 수열을 묶어가는 개념, 곧 accumulate가 있다고 할 때, sum과 1-31예제에서
; 만든 product가 이 개념을 응용한 보기임을 보이자.
; (accumulate combiner null-value term a next b)
; accumulate는 이어지는 두 값을 묶는 프로시저 combiner와, 계산할 값이 없을 때 쓰는 null-value  인자를
; 받고, 나머지 인자는 이전 예시와 같다.

; 되도는 프로세스
(define (accumulate-recur combiner null-value term a next b)
  (if (> a b)
      null-value
      (combiner (term a)
                (accumulate-recur combiner null-value term (next a) next b))))

; 반복 프로세스
(define (accumulate-iter combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (combiner result (term a)))))
  (iter a null-value))


;  sum과 product를 accumulate를 불러 쓰는 프로시저로 바꾸자.
(define (sum f a next b)
  (accumulate-iter + 0 f a next b))

(define (product f a next b)
  (accumulate-iter * 1 f a next b))

(define (identity x) x)
(define (inc n) (+ n 1))

(sum identity 1 inc 10)      ; 55
(product identity 1 inc 10)  ; 3628800