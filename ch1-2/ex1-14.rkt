#lang sicp

(define (first-denominateion kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (= kinds-of-coins 0)) 0)
        (else (+ (cc amount
                     (- kinds-of-coins 1))
                 (cc (- amount
                        (first-denominateion kinds-of-coins))
                     kinds-of-coins)))))

(define (count-change amount)
  (cc amount 5))

#|
나무꼴을 다 그리는 건 스킵하고, 프로세스가 거치는 단계 수와 기억공간의 크기는
어떤 자람차수를 보이는가?

1. 단계 수(동작 시간)
n이 amount이고, k가 현재 가지고있는 동전 종류라고 할 때, cc의 동작시간을 T(n, k)이라는 함수로
생각하면, 그외의 경우 T(n, k) = T(n-현재동전값, k) + T(n, k-1)이라고 할 수 있다.
T는 각 단계에서 2번의 선택지가 있으므로 2배씩 계산 단계가 커진다. 근데, 선택지가 2개인 단계의 최대 횟수는
동전의 종류의 갯수와 n에서 동전 중 최소로 나눈 몫 중 더 큰 값이다.
현재는 k=5이고 동전 중 최소는 1이므로 5와 n 중 더 큰 값이 된다.
T(n, 5) = ┌ O(2^(n+1)) #if n >= 5
          └ O(2^5) #otherwise

2. 기억공간의 크기
각 cc는 되돌기 부름될 때 반복 프로세스가 아니므로, 기억공간은 상수가 아니다. 최대로 쓰는 기억공간은
프로세스 나무꼴의 깊이에 해당하고, 나무꼴의 최대 깊이는 현재 동전 중 최소가 1이므로 n이 된다.
S(n, 5) = O(n)
 |#





