#lang sicp

(define (average a b c) (/ (+ a b c) 3.0))

(define (cont-frac-iter n d k)
  (define (f count result)
    (if (> count k)
        result
        (f (+ count 1) (/ (n count) (+ (d count) result)))))
  (f 1 0))

(define (cont-frac-recur n d k)
  (define (f count)
    (if (= count k)
        (/ (n count) (d count))
        (/ (n count) (+ (d count) (f (+ count 1))))))
  (f 1))


(+ 2 (cont-frac-recur (lambda (i) 1.0)
                      (lambda (i)
                        (if (= (remainder i 3) 2)
                            (* 2 (/ (+ i 1) 3))
                            1))
                      30))
; 2.7182818284590455


(+ 2 (cont-frac-iter (lambda (i) 1.0)
                      (lambda (i)
                        (if (= (remainder i 3) 2)
                            (* 2 (/ (+ i 1) 3))
                            1))
                      30))
; 2.953516797605659
; 왜 반복 프로세스는 정확하지 않을까? 계산 방식은 바텀업 방식으로 정확히 동일한데;;;;

