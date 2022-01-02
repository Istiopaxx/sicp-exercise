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
; Define element-of-set? to confirm whether msg is in symbols-list.
(define (element-of-set? x set)
  (cond ((null? set) false)
        ((equal? x (car set)) true)
        (else (element-of-set? x (cdr set)))))


; ==================================================
; Exercise 2.68.

; The encode procedure takes as arguments a message and a tree and produces the list of bits
; that gives the encoded message.

(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

; Encode-symbol is a procedure, which you must write, that returns the list of bits that encodes
; a given symbol according to a given tree. You should design encode-symbol so that
; it signals an error if the symbol is not in the tree at all.

; Test your procedure by encoding the result you obtained in exercise 2.67 with the sample tree and
; seeing whether it is the same as the original sample message.


; ==================================================
; Define encode-symbol.
(define (encode-symbol msg tree)
  (define (encode-iter msg tree bits)
    (if (leaf? tree)
        bits
        (let ((left (left-branch tree))
              (right (right-branch tree)))
          (cond ((element-of-set? msg (symbols left))
                 (encode-iter msg left (cons 0 bits)))
                ((element-of-set? msg (symbols right))
                 (encode-iter msg right (cons 1 bits)))
                (else (error "bad message -- encode-iter" msg))))))
  (reverse (encode-iter msg tree nil)))


; ==================================================
; Test.
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree (make-leaf 'D 1)
                                   (make-leaf 'C 1)))))

(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))

(encode '(A D A B B C A) sample-tree)
; (0 1 1 0 0 1 0 1 0 1 1 1 0)
