#lang sicp

(define (cube x) (* x x x))

(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; 정적분을 나이브하게 나타내 보았다.
(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b)
     dx))

; 심프슨의 규칙을 써서 더 정확하게 해보자.
(define (simpson f a b n)
  (define h (/ (- b a) n))
  (define (add-h x)
    (+ x (* 2 h)))
    (* (+ (f a)
          (f b)
          (* 4 (sum f (+ a h) add-h (- b h)))
          (* 2 (sum f (+ a h h) add-h (- b h h))))
       (/ h 3.0)))

; 나이브한 정적분 프로시저는 dx가 0.001, 즉 1/1000일때도 완전 정확하진 않지만, 심프슨 규칙을 활용한 정적분
; 프로시저는 n=100(즉 1/100)인 경우에도 정확하다. 나이브 정적분은 직사각형을 계산하고, 심프슨 규칙은 
; 사다리꼴로 계산해서 심프슨 규칙이 더 정확하게 근사한다.
(simpson cube 0 1 10)
(simpson cube 0 1 100)
(simpson cube 0 1 1000)
(simpson cube 0 1 300)
(simpson cube 0 1 900)
(integral cube 0 1 0.001)

