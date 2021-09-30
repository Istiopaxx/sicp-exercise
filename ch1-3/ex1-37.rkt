#lang sicp

; 되도는 프로세스
(define (cont-frac-recur n d k)
  (define (f count)
    (if (= count k)
        (/ (n count) (d count))
        (/ (n count) (+ (d count) (f (+ count 1))))))
  (f 1))

; 반복 프로세스
(define (cont-frac n d k)
  (define (f count result)
    (if (> count k)
        result
        (f (+ count 1) (/ (n count) (+ (d count) result)))))
  (f 0 0))


; 황금비는 1.618033988749895정도 된다. k값을 얼마나 주어야 1.6180까지 맞아떨어질까? 한번 찾아보자.


(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           5)
; 0.6153846153846154

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           7)
; 0.6176470588235294

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           9)
; 0.6179775280898876

(cont-frac (lambda (i) 1.0)
           (lambda (i) 1.0)
           10)
; 0.6180555555555556
; k가 10이 되면 소숫점 4자리까지 정확해진다.