#lang sicp

;; n<3일 때 f(n)=n이고, n>=3일 때 f(n)=f(n-1)+2f(n-2)+3f(n-3)으로 정의한 함수 f가 있다.
;; f의 프로시저를 되도는 프로세스, 반복 프로세스를 만드는 두가지 방법으로 작성해보자.

;; 되도는 프로세스 
(define (f-recursive n)
  (if (< n 3)
      n
      (+ (f-recursive (- n 1))
         (* 2 (f-recursive (- n 2)))
         (* 3 (f-recursive (- n 3))))))

;; 반복 프로세스
(define (f-iterative n)
  (define (calc a b c)
    (+ a
       (* 2 b)
       (* 3 c)))
  (define (f-iter a b c count)
    (if (= count 0)
        c
        (f-iter (calc a b c) a b (- count 1))))
  (f-iter 2 1 0 n))


;; 결과를 봅시다.
(f-recursive 35) ;; 4630608915694 => 거의 10초이상 걸림.
(f-iterative 35) ;; 4630608915694 => 즉시 결과나옴.