#lang sicp

; Louis Reasoner는 1-24 예제를 푸는게 너무 어려웠다. 그의 fast-prime? 프로시저는 원래 prime? 프로시저보다
; 느렸다? 그는 친구 Eva Lu Ator를 불러서 도움을 요청했다. 루이스의 코드를 쭉 보다가, 그들은 expmod
; 프로시저가 square 프로시저를 안쓰고 곱셈을 명시적을 하는 것을 발견했다. 

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (* (expmod base (/ exp 2) m)
                       (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))
; 루이스는 뭐가 잘못인지 몰랐지만, 똑똑이 에바는 이렇게 하면 O(logN)의 코드가 O(N)코드가 된다고 설명하였다.
; 왜인지 한번 설명해보자.

; 위처럼 하면 인자 먼저 계산법을 따르는 실행기는 (expmod base (/ exp 2) m)가 두개의 인자에 각각 들어가
; 있으므로, 두번 계산하게 된다. O(logN)이 되는 원리는 계산단계가 square(거듭제곱)을 활용해 2배씩 줄어드는
; 원리인데, 위처럼 하면 인자먼저계산법에 의해 square 프로시저를 이용하여 한번만 계산해도 되는 
; (expmod base (/ exp 2) m)를 두번 계산하기 때문에 계산 가짓수는 유지된다.