#lang sicp

(define (reverse input-list)
  (define (iter input result)
    (if (null? input)
        result
        (iter (cdr input)
              (cons (car input) result))))
  (iter input-list nil))


; Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure
; that takes a list as argument and returns as its value the list with its elements reversed and
; with all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))

; x
; ((1 2) (3 4))

; (reverse x)
; ((3 4) (1 2))


; ==================================================
; Define deep-reverse

(define (deep-reverse inp)
  (define (iter seq ret)
    (cond ((null? seq) ret)
          ((not (pair? (car seq)))
           (iter (cdr seq)
                 (cons (car seq) ret)))
          (else
           (iter (cdr seq)
                 (cons (iter (car seq) nil)
                       ret)))))
  (iter inp nil))


x ; ((1 2) (3 4))
(deep-reverse x)
; ((4 3) (2 1))

(define y (list (list 1 2 (list 3 4 5)) (list 3 4) 8 9))
y ; ((1 2 (3 4 5)) (3 4) 8 9)
(deep-reverse y)
; (9 8 (4 3) ((5 4 3) 2 1))
