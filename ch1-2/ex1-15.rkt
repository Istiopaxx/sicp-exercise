#lang sicp

(define (cube x) (* x x x))

(define (p x) (- (* 3 x) (* 4 (cube x))))

(define (abs x) (if (< x 0) (- x) x))

(define (sine angle)
  (if (not (> (abs angle) 0.1))
      angle
      (p (sine (/ angle 3.0)))))

(sine 12.15)

#|
라디안으로 나타낸 각 x가 있을때, sin(x)를 구한다고 하자. x가 충분히 작으면 sin(x) ≒ x 이고, 그렇지 않을때는
어떤 공식(p로 나타냄)을 적용하면 구할 수 있다. 충분히 작은 각이란, 0.1rad보다 크지 않은 각을 말한다.
1. (sine 12.15)의 값을 구할때, p 프로시저를 몇번 불러 쓰는가?
(sine 12.15) => (p (sine 4.05)) => (p (p (sine 1.35))) => (p (p (p (sine 0.45))))
=> (p (p (p (p (sine 0.15))))) => (p (p (p (p (p (sine 0.05)))))) => 5번

2. (sine a) 값을 계산할때 sine 프로시저가 만들어내는 프로세스에서 기억공간과 계산 단계의 자람차수를
a의 함수로 나타내면?
a의 값이 0.1이하가 될 때까지 a를 3으로 나눠서 되돌기 부름하므로, 되도는 프로세스이고 a는 단계마다
 1/3로 줄어든다. 즉 계산 단계는 log(a), 기억공간도 log(a)이다. 
 |#