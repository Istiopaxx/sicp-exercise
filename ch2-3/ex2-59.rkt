#lang sicp

; Exercise 2.59.

; Implement the union-set operation for the unordered-list representation of sets.

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))

(define (adjoin-set x set)
  (if (element-of-set? x set)
      set
      (cons x set)))

(define (intersection-set set1 set2)
  (cond ((or (null? set1) (null? set2)) '())
        ((element-of-set? (car set1) set2)
         (cons (car set1)
               (intersection-set (cdr set1) set2)))
        (else (intersection-set (cdr set1) set2))))

; ==================================================
; Define union-set.
(define (union-set set1 set2)
  (define (iter set result)
    (cond ((null? set) result)
          ((element-of-set? (car set) result)
           (iter (cdr set) result))
          (else (iter (cdr set) (cons (car set) result)))))
  (iter set1 set2))

; ==================================================
; Test.
(union-set (list 1 2 3 4) (list 3 4 5 6))
; (2 1 3 4 5 6)