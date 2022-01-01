#lang sicp

(define (entry tree) (car tree))

(define (left-branch tree) (cadr tree))

(define (right-branch tree) (caddr tree))

(define (make-tree entry left right)
  (list entry left right))

(define (element-of-set? x set)
  (cond ((null? set) false)
        ((= x (entry set)) true)
        ((< x (entry set))
         (element-of-set? x (left-branch set)))
        ((> x (entry set))
         (element-of-set? x (right-branch set)))))

(define (adjoin-set x set)
  (cond ((null? set) (make-tree x '() '()))
        ((= x (entry set)) set)
        ((< x (entry set))
         (make-tree (entry set)
                    (adjoin-set x (left-branch set))
                    (right-branch set)))
        ((> x (entry set))
         (make-tree (entry set)
                    (left-branch set)
                    (adjoin-set x (right-branch set))))))


; ==================================================
; Exercise 2.63.

; Each of the following two procedures converts a binary tree to a list.


(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))


(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))


; ==================================================
; a. Do the two procedures produce the same result for every tree? If not, how do the results differ?
;    What lists do the two procedures produce for the trees in figure 2.16?

(tree->list-1 (make-tree 7
                         (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                         (make-tree 9 '() (make-tree 11 '() '()))))
; (1 3 5 7 9 11)
(tree->list-2 (make-tree 7
                         (make-tree 3 (make-tree 1 '() '()) (make-tree 5 '() '()))
                         (make-tree 9 '() (make-tree 11 '() '()))))
; (1 3 5 7 9 11)
(tree->list-1 (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9
                                               '()
                                               (make-tree 11 '() '())))))
; (1 3 5 7 9 11)
(tree->list-2 (make-tree 3
                         (make-tree 1 '() '())
                         (make-tree 7
                                    (make-tree 5 '() '())
                                    (make-tree 9
                                               '()
                                               (make-tree 11 '() '())))))
; (1 3 5 7 9 11)
(tree->list-1 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))
; (1 3 5 7 9 11)
(tree->list-2 (make-tree 5
                         (make-tree 3
                                    (make-tree 1 '() '())
                                    '())
                         (make-tree 9
                                    (make-tree 7 '() '())
                                    (make-tree 11 '() '()))))
; (1 3 5 7 9 11)

; It looks all same.. above two procedures are functionally identical: their implementation is not.
; First is recursive, Second is iterative form.



; ==================================================
; b. Do the two procedures have the same order of growth in the number of steps required to convert
;    a balanced tree with n elements to a list? If not, which one grows more slowly?

; ====================
; 1. tree->list-1 procedure
; T(n) = 2*T(n/2) + O(n/2)
;                      └> because of append operation which takes linear time.
; => T(n) = O(nlogn)

; ====================
; 2. tree->list-2 procedure
; T(n) = 2*T(n/2) + O(1)

; => T(n) = O(n)








