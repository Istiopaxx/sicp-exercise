#lang sicp
(#%require sicp-pict)


; Exercise 2.47.
; Here are two possible constructors for frames:

(define (make-frame-li origin edge1 edge2)
  (list origin edge1 edge2))

(define (make-frame origin edge1 edge2)
  (cons origin (cons edge1 edge2)))

; For each constructor supply the appropriate selectors to produce an implementation for frames.

(define (origin-frame-li f)
  (car f))
(define (edge1-frame-li f)
  (cadr f))
(define (edge2-frame-li f)
  (cadr (cdr f)))


(define (origin-frame f)
  (car f))
(define (edge1-frame f)
  (cadr f))
(define (edge2-frame f)
  (cdr (cdr f)))
