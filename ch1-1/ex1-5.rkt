#lang sicp

;; 인터프리터는 인자먼저계산법(applicative order evaluation)과
;; 정의대로계산법(normal-order evaluation)(lazy-evaluation이라고도 함) 중에 어떤 것을 따를까?
;; 아래를 통해 알아보자.

(define (p) (p))

(define (test x y)
  (if (= x 0)
      0
      y))

(test 0 (p)) ;; 현재 실행 환경(DrRacket)에서는 실행이 끝나지 않는다.

;; (p) 는 다시 자기 자신을 참조하도록 되어있는 표현식이다.
;; 따라서 (p)를 참조하려고 하면 무한 순환 참조를 하게 된다.

;; applicative order evaluation에서는 (test 0 (p))에서 (p)가
;; 표현식이므로 먼저 계산하게 된다. 따라서 무한 순환 참조에 빠지게 되고,
;; 실행이 끝나지 않는다. 즉 현재 실행환경은 applicative order evaluation을 따른다.

;; normal-order evaluation에서는 (test 0 (p))를 펼쳐서
;; (if (= 0 0) 0 (p))를 만들게 되고, 이제 기본 인자만 남았으므로 연산한다.
;; (= 0 0)은 참이므로 (if (= 0 0) 0 (p))는 0을 결과로 내뱉고 (p)식에는 도달하지 않는다.




;; 또다른 예시
(define (try a b)
  (if (= a 0) 1 b))

(try 0 (/ 1 0)) ;; 현재 실행 환경에서는 "division by zero" 에러 발생함.

;; applicative order evaluation에서는 (/ 1 0)을 먼저 계산하므로 에러가 발생한다.
;; normal-order evaluation에서는 펼쳐서 (if (= 0 0) 1 (/ 1 0))을 만든다음
;; 연산하여 1을 뱉고, (/ 1 0)에 도달하지 않는다.