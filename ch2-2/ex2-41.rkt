#lang sicp

; Exercise 2.41.
; Write a procedure to find all ordered triples of distinct positive integers i, j, and k less than
; or equal to a given integer n that sum to a given integer s.


(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))

(define (make-t-pairs-sum t-pairs)
  (list (car t-pairs)
        (cadr t-pairs)
        (cadr (cdr t-pairs))
        (+ (car t-pairs) (cadr t-pairs) (cadr (cdr t-pairs)))))


; ==================================================
; Define unique-triple-pairs.
(define (unique-t-pairs n)
  (flatmap
   (lambda (i)
     (flatmap (lambda (j)
                (map (lambda (k) (list k j i))
                     (enumerate-interval 1 (- j 1))))
              (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

; Define predicate.
(define (sum-eq-s? s sum)
  (= s sum))

; Define triple-sum-pairs.
(define (triple-sum-pairs n s)
  (filter (lambda (pairs) (sum-eq-s? s (cadr (cdr (cdr pairs)))))
          (map make-t-pairs-sum (unique-t-pairs n))))

; Test.
(map make-t-pairs-sum (unique-t-pairs 4))
; ((1 2 3 6) (1 2 4 7) (1 3 4 8) (2 3 4 9))
(triple-sum-pairs 4 9)
; ((2 3 4 9))

