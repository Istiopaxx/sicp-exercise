#lang sicp

; 피보나치 수열을 구하는 계산 단계를 로그 차수로 하는 알고리즘을 만들어 보자.
; a <- a+b, b <- a의 규칙을 T라고 할 때, a와 b의 첫 값을 각각 1과 0으로 놓고 T를 n번 하면 Fib(n+1)과 Fib(n)
; 을 얻을 수 있다. 즉 T^n, (1, 0)에서 T를 n번 거듭곱하면 피보나치 수열이 나온다. T_pq가 (a, b)를 두고
; a <- bq + aq + ap, b <- bp + aq라는 규칙을 나타낸다고 하면, T는 T_pq에서 p=0, q=1인 경우라고 볼 수 있다.
; T_pq를 두번 거듭하면 T_p`q`을 한번 한것과 같다는 것을 밝히고, p와 q로 p`와 q`를 계산하는 식도 만들자.
; 이래야 T_pq를 제곱하는 방법을 얻어 T^n을 계산하는데 계속 제곱하는 방법을 쓸 수 있다. 아래의 빈칸을 채우자.

(define (fib n)
  (define (even? n) (= (remainder n 2) 0))
  (define (next-p p q) (+ (* p p) (* q q)))
  (define (next-q p q) (+ (* 2 p q) (* q q)))
  (define (fib-iter a b p q count)
    (cond ((= count 0) b)
          ((even? count)
           (fib-iter a
                     b
                     (next-p p q) ; p`를 채워보자.
                     (next-q p q) ; q`를 채워보자.
                     (/ count 2)))
          (else (fib-iter (+ (* b q) (* a q) (* a p))
                          (+ (* b p) (* a q))
                          p
                          q
                          (- count 1)))))
  (fib-iter 1 0 0 1 n))

(fib 45) ; 1134903170

; ┌ □ □ ┐┌a┐ = ┌ ap+aq+bg ┐ 라고 할 때, □ 에 들어갈 것을 구하면
; └ □ □ ┘└b┘   └   aq+bp  ┘
; ┌ p+q q ┐┌a┐ = ┌ ap+aq+bg ┐ 가 된다.
; └  q  p ┘└b┘   └   aq+bp  ┘
; ┌ p+q q ┐ <= 이것이 T_pq가 뜻하는 선형 변환이라고 할 수 있다. 그럼 T_pq를 두번 한것(T_p`q`)을 구해보자.
; └  q  p ┘
; ┌ p+q q ┐┌ p+q q ┐ = ┌ p^2+2pq+2q^2  2pq+q^2 ┐
; └  q  p ┘└  q  p ┘   └   2pq+q^2     p^2+q^2 ┘
; 따라서 T_p`q`의 p`은 p^2+q^2이고 q`는 2pq+q^2인 것을 알 수 있다.

