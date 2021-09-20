#lang sicp

(define (even? n) (= (remainder n 2) 0))

(define (square n) (* n n))


; 거듭제곱 b^n은 *b 를 n번 반복한 것이다. 따라서 반복되는 프로세스로 구할 수 있다. 근데 n이 매우 크면, 조금
; 비효율적이다. n이 매우 클 때 (b^2)^(n/2) = b^n 을 이용하여 n이 2로 나누어떨어지면 나누면서 계산 단계를 줄일
; 수 있다.


(define (fast-recursive-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-recursive-expt b (/ n 2))))
        (else (* b (fast-recursive-expt b (- n 1))))))

(fast-recursive-expt 2 31)


; 위의 프로세스는 되도는 프로세스이다. n, b, a를 가지고 a*b^n을 일정하게 유지하면서 반복하는 프로세스로 짜보자.
; a는 프로세스가 시작할 때는 1이고, 끝날 때는 결과가 들어있다. (b^2)^(n/2) = (b^(n/2))^2 를 활용해보자.
; fast-expt-iter 프로시저는 b, n, a를 입력으로 받는다. a*b^n은 항상 일정하다.
; n이 짝수일때, b^n = (b^2)^(n/2)를 이용하여 b <- b^2, n <- n/2로 바꾸어 다음 단계로 넘어간다. 
; n이 홀수일때, a <- a * b, n <- n-1로 바꾸어 다음 단계로 넘어간다.
; 능지가 딸려서 20분정도 고민하다가 커뮤니티 솔루션을 보았다. 책에서 힌트는 다 주었는데, 
; 아무래도 반복하는 프로세스를 짤 때에는 타 언어의 반복문처럼 값들이 단계를 넘어갈 때마다 어떻게 바뀌는지
; 규칙을 정해야 좋을 것 같다. 다음부터는 종이와 펜으로 loop variant를 써보자.

(define (fast-expt b n)
  (define (fast-expt-iter b n a)
    (cond ((= n 0) a)
          ((even? n) (fast-expt-iter (square b) (/ n 2) a))
          (else (fast-expt-iter b (- n 1) (* b a)))))
  (fast-expt-iter b n 1))

(fast-expt 2 31)

