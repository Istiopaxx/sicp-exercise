#lang sicp


; Exercise 2.28.  Write a procedure fringe that takes as argument a tree (represented as a list)
; and returns a list whose elements are all the leaves of the tree arranged in left-to-right order.
; For example,

(define x (list (list 1 2) (list 3 4)))

(define (reverse input-list)
  (define (iter input result)
    (if (null? input)
        result
        (iter (cdr input)
              (cons (car input) result))))
  (iter input-list nil))

(define (fringe root)
  (define (iter node result)
    (cond ((null? node) result)
          ((not (pair? (car node)))
           (iter (cdr node)
                 ; using cons directly, not append - for efficiency
                 (cons (car node) result)))
          (else
           (iter (cdr node)
                 (iter (car node) result)))))
  ; reverse once to get correct result
  (reverse (iter root nil)))

(fringe x)
; (1 2 3 4)

(fringe (list x x))
; (1 2 3 4 1 2 3 4)