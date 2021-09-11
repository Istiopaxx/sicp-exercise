#lang sicp

;; Alyssa P.Hacker의 호기심 천국: cond를 써서 if를 보통 프로시저처럼 정의해볼까?

(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(new-if (= 2 3) 0 5) ;; 5

(new-if (= 1 1) 0 5) ;; 0

;; 오 잘된다~!! 이제 sqrt 프로시저를 new-if를 이용해서 정의해보자.
(define (abs x)
  (if (< x 0)
      (- x)
      x))

(define (square x) (* x x))

(define (average a b)
  (/ (+ a b) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))
(define (improve guess x)
  (average guess (/ x guess)))

(define (sqrt-iter-new-if guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter-new-if (improve guess x)
                            x)))
(define (sqrt x)
  (sqrt-iter-new-if 1.0 x))

;; 이제 실행해보자.
(sqrt 9) ;; 절대 끝나지 않는다;,,,;;????!!!?!?!?! 왜 그런지 설명해보자.

;; Scheme 인터프리터는 applicative order evaluation(인자 먼저 계산법)을 따른다.
;; 인자먼저계산법은 기본 프로시저가 아닌 사용자 정의 프로시저에 대해 인자를 먼저 계산한다.
;; 한번 계산 과정을 인자먼저계산법으로 따라가보자.
;; 1. (sqrt 9) 문제 없다 다음으로
;; 2. (sqrt-iter-new-if 1.0 9) 노 프라블럼 다음으로
;; 3. (new-if (good-enough? 1.0 9)
;;            1.0
;;            (sqrt-iter-new-if (improve 1.0 9)
;;                              9)
;;    여기에 문제가 있다. new-if 프로시저는 if 프로시저와 달리 사용자 정의 프로시저이기 때문에, 
;;    인자먼저계산법에 의해 (good-enough? 1.0 9)를 문제없이 계산하여 #f(즉 false)를 받고,
;;    (sqrt-iter-new-if (improve 1.0 9) 9)가 계산된다. 이때 sqrt-iter-new-if 프로시저를
;;    ,즉 자기 자신을 되돌기 부름(책표기를 지켰다)하기 때문에 절때 끝나지 않는다.


;; 이거 실행할때마다 DrRacket이 메모리 엄청 처먹더니 무반응상태 되서 매번 작업관리자로 끝냈다.
;; 이런 함정문제 좋지 않다.









