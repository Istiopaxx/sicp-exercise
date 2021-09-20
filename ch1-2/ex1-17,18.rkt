#lang sicp

; 거듭제곱은 곱셈을 여러번 하는 거라면, 곱셈은 덧셈을 여러번 하는 것이다. 곱셈을 덧셈으로 표현해보았다.

(define (multiply a b)
  (if (= b 0)
      0
      (+ a (* a (- b 1)))))

; 이 알고리즘의 계산 단계는 선형 비례로 자란다. 정수 값을 두배로 하거나 반으로 나누는 프로시저 double과
; halve가 있다고 하자. 이 프로시저를 써서 fast-expt처럼 계산단계가 로그 비례인 곱셈 프로시저를 짜보자.

(define (even? n) (= (remainder n 2) 0))
(define (double n) (* n 2))
(define (halve n) (/ n 2))

(define (fast-mul-recursive a b)
  (cond ((= b 0) 0)
        ((even? b) (double (fast-mul-recursive a (halve b))))
        (else (+ a (fast-mul-recursive a (- b 1))))))

(fast-mul-recursive 2 20) ; 40

; 위 프로시저는 되도는 프로세스이다 .반복 프로세스로 짜보자.

(define (fast-mul a b)
  (define (fast-mul-iter a b tmp)
    (cond ((= b 0) tmp)
          ((even? b) (fast-mul-iter (double a) (halve b) tmp))
          (else (fast-mul-iter a (- b 1) (+ tmp a)))))
  (fast-mul-iter a b 0))

(fast-mul 2 20)

