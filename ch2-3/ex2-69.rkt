#lang sicp



; ==================================================
; Define huffman tree structure.
(define (make-leaf symbol weight)
  (list 'leaf symbol weight))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))

(define (left-branch tree) (car tree))

(define (right-branch tree) (cadr tree))

(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))

(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))



; ==================================================
; Define leaf-set using ordered list implementation.
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair)    ; symbol
                               (cadr pair))  ; frequency
                    (make-leaf-set (cdr pairs))))))



; ==================================================
; Exercise 2.69.

; The following procedure takes as its argument a list of symbol-frequency pairs
; (where no symbol appears in more than one pair) and generates a Huffman encoding tree according
; to the Huffman algorithm.

(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))

; Make-leaf-set is the procedure given above that transforms the list of pairs into an ordered
; set of leaves. Successive-merge is the procedure you must write, using make-code-tree to
; successively merge the smallest-weight elements of the set until there is only one element left,
; which is the desired Huffman tree.

; (This procedure is slightly tricky, but not really complicated. If you find yourself designing a
;  complex procedure, then you are almost certainly doing something wrong. You can take significant
;  advantage of the fact that we are using an ordered set representation.)



; ==================================================
; Define successive-merge.

(define (successive-merge leaf-set)
  (if (= (length leaf-set) 1)
      (car leaf-set)
      (let ((first (car leaf-set))
            (second (cadr leaf-set)))
        (successive-merge (adjoin-set (make-code-tree first second)
                                      (cddr leaf-set))))))



; ==================================================
; Test.

(generate-huffman-tree (list '(A 4) '(B 2) '(C 1) '(D 1)))
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)
(make-code-tree (make-leaf 'A 4)
                (make-code-tree
                 (make-leaf 'B 2)
                 (make-code-tree (make-leaf 'D 1)
                                 (make-leaf 'C 1))))
; ((leaf A 4) ((leaf B 2) ((leaf D 1) (leaf C 1) (D C) 2) (B D C) 4) (A B D C) 8)


