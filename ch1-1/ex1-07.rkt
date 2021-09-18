#lang sicp

(define (abs x)
  (if (< x 0) (- x) x))
(define (square x) (* x x))
(define (average a b)
  (/ (+ a b) 2))

(define (sqrt x)
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.00001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess)
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

;; 위에서 만든 good-enough?는 아주 작은 수와 아주 큰 수의 제곱근을 같이 구하는데에는
;; 알맞지 않다. 아주 작은 수를 구하려면 오차 범위를 아주 작은 0.000000000001정도로
;; 해야하고, 유효숫자 이슈때문에 아주 큰수에서는 오차범위를 높게 잡아야 한다. 예시를 한번 보자.

(sqrt 245625626544325134124123412542352345234523423577)
;; 실행이 끝나지 않는다. 유효숫자를 넘어선 크기라서 한 틱 단위가 0.00001보다 커졌기 때문에,
;; good-enough? 프로시저가 참이 되는 경우는 없다. 
(sqrt 0.00000004) ;; expected: 0.0002 // real : 0.001959946873290669
;; 유효숫자가 너무 크기 때문에 정확한 계산이 되지 않는다. 보다 정확한 값으로 가기 전에,
;; good-enough? 프로시저가 참이 되기 때문에 계산이 멈춘다.


;; 즉, 현재는 아주 큰 수와 아주 작은 수를 동시에 정확하게 구할 수는 없다.
;; 더 좋은 방법은 어림잡은 값을 고쳐나가면서 예전 값에 비교하여 고친 값이 별로 나아지지
;; 않을 때까지 계산을 이어나가는 것이다. 그렇게 한번 바꾸어 보자.


(define (good-sqrt x)
  (define (good-enough? guess improved)
    (< (abs (- guess improved)) 0.00001))
  (define (improve guess)
    (average guess (/ x guess)))
  (define (sqrt-iter guess)
    (if (good-enough? guess (improve guess))
        guess
        (sqrt-iter (improve guess))))
  (sqrt-iter 1.0))

(good-sqrt 245625626544325134124123412542352345234523423577) ;; 4.956063221391804e+23
;; 오차 범위를 작은 값으로 잡았지만 아주 큰 수에서도 잘 동작한다. 
(good-sqrt 0.00000004) ;; 0.00020057080390751842
;; 오차 범위 값이 위의 것과 똑같지만 더 정확하게 나온다. 