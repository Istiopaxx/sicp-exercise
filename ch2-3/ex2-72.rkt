#lang sicp


; Exercise 2.72.

; Consider the encoding procedure that you designed in exercise 2.68. What is the order of growth
; in the number of steps needed to encode a symbol? Be sure to include the number of steps needed
; to search the symbol list at each node encountered. To answer this question in general is difficult.

; Consider the special case where the relative frequencies of the n symbols are as described
; in exercise 2.71, and give the order of growth (as a function of n) of the number of steps needed
; to encode the most frequent and least frequent symbols in the alphabet.


; ==================================================
; Define encode.
(define (encode message tree)
  (define (encode-symbol msg tree)
    (define (encode-iter msg tree bits)
      (if (leaf? tree)
          bits
          (let ((left (left-branch tree))
                (right (right-branch tree)))
            (cond ((memq msg (symbols left))
                   (encode-iter msg left (cons 0 bits)))
                  ((memq msg (symbols right))
                   (encode-iter msg right (cons 1 bits)))
                  (else (error "bad message -- encode-iter" msg))))))
    (reverse (encode-iter msg tree nil)))
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))

(define (leaf? object)
  (eq? (car object) 'leaf))

(define (symbol-leaf x) (cadr x))

(define (weight-leaf x) (caddr x))

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
; Consider special case of exercise 2.71.

; Assume that there are n symbols that is numbered by frequency,
; 1 is most frequent symbol, 2 is next, 3 is next.... N is least frequent symbol.
; Result huffman Tree would be like below:

;          root
;         ┌──┴──┐  // -> (1 symbol) / (n-1 symbols)
;         n     │
;              ┌┴──┐  // -> (1 symbol) / (n-2 symbols)
;             n-1  │
;                 ┌┴──┐ // -> (1 symbol) / (n-3 symbols)
;                n-2  │
;                    ...
;                    ...
;                     ┌┴──┐ // (1 symbol) / (1 symbol)
;                     2   1

; When encode procedure encounter each branch,
; encode procedure lookup the symbols list of left and right branch.


; ==================================================
; 1. Most frequent symbol encoding case :
; it maybe lookup N symbols, and find most frequent symbol at once.
; Then reverse whole bit sequence, which takes just O(1) because the bit sequence's length is just 1.
; T(n) => O(n) + O(1) = O(n)


; ==================================================
; 2. Least frequent symbol encoding case :
; it may lookup N symbols at first, (N-1) symbols next, (N-2) symbols third.. (2) symbols at last.
; Then procedure would find least frequent symbol.
; Then it reverse whole bit sequence, which takes O(n-1) because bit sequence length is n-1.

; T(n) => O(n*n) + O(n-1) = O(n^2)



