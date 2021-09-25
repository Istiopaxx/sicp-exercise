#lang sicp

; sum 프로시저는 차수 높은 프로시저로 간추릴 수 있는 보기 가운데 아주 단순한 것이다. sum과 비슷하게,
; 어떤 구간 속에 있는 숫자마다 정해진 함수의 값을 구하고 그 값을 모두 곱하는 프로시저 product를 짜보자.
; 그런 다음 아래 식을 바탕으로 π에 가까운 값을 얻어보자.
; π/4 = (2·4·4·6·6·8·8·10···)/(3·3·5·5·7·7·9·9···)

(define (square x) (* x x))

; 반복하는 프로세스로 해보자.
(define (product-iter term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

; 이번에는 되도는 프로세스로 해보자.
(define (product-recursive term a next b)
  (if (> a b)
      1
      (* (term a)
         (product-recursive term (next a) next b))))




; f를 (2n)*(2n+2) / (2n+1)^2 로 정의했다.
(define (next a) (+ a 1))
(define (f n)
  (/ (* 4.0 (+ (square n) n))
     (square (+ (* 2 n) 1.0))))

(define (pi-product product term a next b)
  (* 4 (product term a next b)))

(pi-product product-recursive f 1 next 10000000) ; 3.1415927321166954
(pi-product product-iter f 1 next 10000000)      ; 3.1415927321451758

; 둘다 π(3.141592...)에 꽤 근접하였다. 다만 되도는 프로세스는 시간이 약 2~3초, 반복 프로세스는 0.초에 
; 결과가 나왔다. 소숫점 아래로 꽤 내려가면 차이가 보이기도 한다. 되도는 프로세스는 연산 결과를 계속 들고
; 있다가, 나중에 곱하므로 차이가 생기는 것 같다.