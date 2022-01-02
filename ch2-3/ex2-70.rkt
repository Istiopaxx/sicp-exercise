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
; Define generate-huffman-tree.
(define (generate-huffman-tree pairs)
  (define (successive-merge leaf-set)
    (if (= (length leaf-set) 1)
        (car leaf-set)
        (let ((first (car leaf-set))
              (second (cadr leaf-set)))
          (successive-merge (adjoin-set (make-code-tree first second)
                                        (cddr leaf-set))))))
  (successive-merge (make-leaf-set pairs)))



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



; ==================================================
; Define decode.
(define (decode bits tree)
  (define (choose-branch bit branch)
    (cond ((= bit 0) (left-branch branch))
          ((= bit 1) (right-branch branch))
          (else (error "bad bit -- CHOOSE-BRANCH" bit))))
  (define (decode-1 bits current-branch)
    (if (null? bits)
        '()
        (let ((next-branch
               (choose-branch (car bits) current-branch)))
          (if (leaf? next-branch)
              (cons (symbol-leaf next-branch)
                    (decode-1 (cdr bits) tree))
              (decode-1 (cdr bits) next-branch)))))
  (decode-1 bits tree))



; ==================================================
; Exercise 2.70.

; The following eight-symbol alphabet with associated relative frequencies was designed to
; efficiently encode the lyrics of 1950s rock songs. 
; (Note that the ``symbols'' of an ``alphabet'' need not be individual letters.)
; ┌───────────┬──────────┐
; │A    :  2  │ NA  :  16│
; │BOOM :  1  │ SHA :  3 │
; │GET  :  2  │ YIP :  9 │
; │JOB  :  2  │ WAH :  1 │
; └───────────┴──────────┘
; Use generate-huffman-tree (exercise 2.69) to generate a corresponding Huffman tree,
; and use encode (exercise 2.68) to encode the following message:

; ┌──────────────────────────────────────┐
; Get a job

; Sha na na na na na na na na

; Get a job

; Sha na na na na na na na na

; Wah yip yip yip yip yip yip yip yip yip

; Sha boom
; └──────────────────────────────────────┘

; ==================================================
; Generate huffman-tree and encode.

(define rock-tree (generate-huffman-tree (list '(a 2)
                                               '(boom 1)
                                               '(get 2)
                                               '(job 2)
                                               '(na 16)
                                               '(sha 3)
                                               '(yip 9)
                                               '(wah 1))))

(define rock-message (list 'get 'a 'job
                           'sha 'na 'na 'na 'na 'na 'na 'na 'na
                           'get 'a 'job
                           'sha 'na 'na 'na 'na 'na 'na 'na 'na
                           'wah 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip 'yip
                           'sha 'boom))

(define sample-code (encode rock-message rock-tree))
sample-code
; (1 1 1 1 1 1 1 0 0 1 1 1 1 0 1 1 1 0 0 0
;  0 0 0 0 0 0 1 1 1 1 1 1 1 0 0 1 1 1 1 0
;  1 1 1 0 0 0 0 0 0 0 0 0 1 1 0 1 0 1 0 1
;  0 1 0 1 0 1 0 1 0 1 0 1 0 1 0 1 1 1 0 1
;  1 0 1 1)
(length sample-code)
; 84

; How many bits are required for the encoding?
; => 84 bits.



; ==================================================
; What is the smallest number of bits that would be needed to encode this song if we used a
; fixed-length code for the eight-symbol alphabet?

; 8 symbols -> 2^3 = 8, so it needs 3 bits to encode.
; fixed-length(3 bits) code for a symbol, and the song has 36 symbols.
; => 36 * 3 = 108 bits.

; Approximately 22% decreased bits length for huffman-coding, compared to fixed-length coding.