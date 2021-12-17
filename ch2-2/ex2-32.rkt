#lang sicp

; Exercise 2.32.  We can represent a set as a list of distinct elements, and we can represent the
; set of all subsets of the set as a list of lists. For example, if the set is (1 2 3), then the
; set of all subsets is (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3)).

; Complete the following definition of a procedure that generates the set of subsets of a set and
; give a clear explanation of why it works:

(define (subsets s)
  (if (null? s)
      (list nil)
      (let ((rest (subsets (cdr s))))
        (append rest
                (map (lambda (item)
                       (cons (car s) item))
                     rest)))))

; This procedure works by 2 options of selection and combine it.
; A subset of any of set is choosing the set's first item and decide whether to put that
; first item to existing subset or not. So existing subset must be a subsets of remaining itmes' set.
; Above procedure implements this idea by append rest(w/o first item, making subset) and
; choosen one(w/ first item, making subset). 

(subsets (list 1 2 3))
; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))