#lang sicp

(define (reverse input-list)
  (define (reverse-iter input result)
    (if (null? input)
        result
        (reverse-iter (cdr input)
                      (cons (car input) result))))
  (reverse-iter input-list nil))


; Exercise 2.27.  Modify your reverse procedure of exercise 2.18 to produce a deep-reverse procedure
; that takes a list as argument and returns as its value the list with its elements reversed and
; with all sublists deep-reversed as well. For example,

(define x (list (list 1 2) (list 3 4)))

x
; ((1 2) (3 4))

(reverse x)
; ((3 4) (1 2))


; ==================================================
; Define deep-reverse

(define (deep-reverse inp)
  (define (reverse-iter seq ret)
    (cond ((null? seq) ret)
          ((pair? (car seq)) (reverse-iter (cdr seq)
                                           (cons (reverse (car seq)) ret)))
          (else (reverse-iter (cdr seq)
                              (cons (car seq) ret)))))
  (reverse-iter inp nil))

(deep-reverse x)
; ((4 3) (2 1))