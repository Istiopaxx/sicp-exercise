#lang sicp


(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

; ==================================================
; Exercise 2.64.


; The following procedure list->tree converts an ordered list to a balanced binary tree.
; The helper procedure partial-tree takes as arguments an integer n and list of at least n elements
; and constructs a balanced tree containing the first n elements of the list.
; The result returned by partial-tree is a pair (formed with cons) whose car is the constructed tree
; and whose cdr is the list of elements not included in the tree.

(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let ((left-size (quotient (- n 1) 2)))
        (let ((left-result (partial-tree elts left-size)))
          (let ((left-tree (car left-result))
                (non-left-elts (cdr left-result))
                (right-size (- n (+ left-size 1))))
            (let ((this-entry (car non-left-elts))
                  (right-result (partial-tree (cdr non-left-elts)
                                              right-size)))
              (let ((right-tree (car right-result))
                    (remaining-elts (cdr right-result)))
                (cons (make-tree this-entry left-tree right-tree)
                      remaining-elts))))))))


; ==================================================
; a. Write a short paragraph explaining as clearly as you can how partial-tree works.
;    Draw the tree produced by list->tree for the list (1 3 5 7 9 11).

; partial-tree procedure makes left-partial-tree recursive call for first n/2 elements,
; and among later elements, makes right-partial-tree recursive call for remaining n/2 elements
; without the first of remaining, (n+1)th element.
; And make tree with left-paritial-tree result, (n+1)th element as entry, right-partial-tree result.

; (1 3 5 7 9 11)
; (1 3 5 | 7 9 11)
; (left-partial-tree with (1 3 5) | 7 | (right-partial-tree with (9 11)))
; (...)

; Result
(list->tree (list 1 3 5 7 9 11))
; (5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

;            5
;         ┌──┴──┐
;         1     9
;         └┐   ┌┴──┐
;          3   7   11

; ==================================================
; b. What is the order of growth in the number of steps required by list->tree
;    to convert a list of n elements?

; Whole list divided to two partial sub-list, and for combining it takes a cons procedure once.
; T(n) = 2*T(n/2) + O(1)
;
; => T(n) = O(n).








