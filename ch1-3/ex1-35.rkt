#lang sicp

(define tolerance 0.00001)
(define (abs x) (if (< x 0) (- x) x))
(define (square x) (* x x))

(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define golden
  (fixed-point (lambda (x) (+ 1 (/ 1 x))) 1.0))

golden ; 1.6180327868852458

; 황금비 Φ가 x=>1+1/x 함수의 고정점이라는 것을 밝히고, 이것을 바탕으로 fixed-point 프로시저로 Φ 를 찾아보자.
; Φ^2 = Φ+1 이므로 Φ = 1 + 1/Φ 이다. 따라서 위 함수의 고정점이다.
; 그럼 Φ = Φ^2 - 1이므로 이거도 고정점이 가능할까?
; 안되는데, 왜냐하면 Φ^2 - 1이 단계를 거듭할수록 Φ로 가까이 다가가야되는데 그러지 않기 때문..





