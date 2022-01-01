#lang sicp


; Exercise 2.66.

; Implement the lookup procedure for the case where the set of records is structured as a binary tree,
; ordered by the numerical values of the keys.

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

; Define lookup.
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      #f
      (let ((now (entry set-of-records)))
        (cond ((= given-key now) now)
              ((< given-key now)
               (lookup given-key (left-branch set-of-records)))
              ((> given-key now)
               (lookup given-key (right-branch set-of-records)))))))

; ==================================================
; Test.

(define a (list 5 (list 1 '() (list 3 '() '())) (list 9 (list 7 '() '()) (list 11 '() '()))))

(lookup 5 a)
; 5
(lookup 4 a)
; #f

