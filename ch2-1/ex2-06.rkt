#lang sicp

; Exercise 2.6.
; In case representing pairs as procedures wasn't mind-boggling enough, consider that, in a language
; that can manipulate procedures, we can get implementing 0 and the operation of adding 1 by without
; numbers (at least insofar as nonnegative integers are concerned) as below.

(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))

; This representation is known as Church numerals, after its inventor, Alonzo Church, the logician
; who invented the lambda calculus.

; ==================================================
; 1. Define one and two directly (not using of zero and add-1).
; (Hint: Use substitution to evaluate (add-1 zero)).

; one => (add-1 zero) => (lambda (f) (lambda (x) (f ((zero f) x))))
; [(zero f) :=> (lambda (x) x)] => (lambda (f) (lambda (x) (f x)))
(define one (lambda (f) (lambda (x) (f x))))

; two => (add-1 one) => (lambda (f) (lambda (x) (f ((one f) x))))
; [(one f) :=> (lambda (x) (f x))] => (lambda (f) (lambda (x) (f ((lambda (x) (f x)) x))))
; => (lambda (f) (lambda (x) (f (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))


; ==================================================
; 2. Give a direct definition of the addition procedure + .(not using repeated application of add-1)
; (add one n) => (lambda (f) (lambda (x) (f ((n f) x))))
; (add n one) => (lambda (f) (lambda (x) ((n f) (f x))))
; add-1 n => f1·fn·fn···fn·fn
;        1 <─┴┘ └───────────┴─> n
; add a b => fa·fa···fa·fa·fb·fb···fb·fb
;        a <─┴───────────┘ └───────────┴─> b
(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))


; ==================================================
; usage for common integer
(define (inc a) (+ a 1))
(define (church-numeral->int cn)
  ((cn inc) 0)) ; 0 is start point, f is inc

(church-numeral->int one)                       ; 1
(church-numeral->int two)                       ; 2
(church-numeral->int (add (add two one) two))   ; 5
