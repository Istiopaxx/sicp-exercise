#lang sicp

(define (square x) (* x x))

(define (f g)
  (g 2))

(f square) ; 4

(f (lambda (z) (* z (+ z 1)))) ; 6

(f f) ; error
; application: not a procedure;
; expected a procedure that can be applied to arguments
; given: 2

; 왜 이렇게 될까?
; (f f) -> (f 2) -> (2 2) 가 되서 그렇다.