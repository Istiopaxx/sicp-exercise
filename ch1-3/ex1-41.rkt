#lang sicp

(define (double f)
  (lambda (i)
    (f (f i))))

(define (inc i) (+ i 1))

((double inc) 1) ; 3
; (double inc)는 2를 더하는 프로시저이므로 결과가 3이다.

(((double (double double)) inc) 5) ; 21
; (double double) => (double (double ..))
; (double (double double)) => (double (double (double (double ...))))
; ((double (double double)) inc) => (double (double (double (double inc))))
; => inc를 더블 더블 더블 더블 했으므로 2^4번 한거고, 그럼 inc 를 16번 했다.
; 따라서 5 + 16이기 때문에 21



