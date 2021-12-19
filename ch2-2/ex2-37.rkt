#lang sicp

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      nil
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))


; Exercise 2.37.
; Suppose we represent vectors v = (v_i) as sequences of numbers,
; and matrices m = (m_ij) as sequences of vectors (the rows of the matrix).
; For example, the matrix

; ┌1 2 3 4┐
; │4 5 6 6│
; └6 7 8 9┘

; is represented as the sequence ((1 2 3 4) (4 5 6 6) (6 7 8 9)). With this representation,
; we can use sequence operations to concisely express the basic matrix and vector operations.
; These operations (which are described in any book on matrix algebra) are the following:

; (dot-product v w)         // returns the sum Sigma(i) v_i*w_i
; (matrix-*-vector m v)     // returns the vector t, where t_i = Sigma(j) m_ij*v_j
; (matrix-*-matrix m n)     // returns the matrix p, where p_ij = Sigma(k) m_ik*n_kj
; (transpose m)             // returns the matrix n, where n_ij = m_ji

; We can define the dot product as

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

; Fill in the missing expressions in the following procedures for computing
; the other matrix operations. (The procedure accumulate-n is defined in exercise 2.36.)

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product v w)) m))
(define (transpose mat)
  (accumulate-n cons nil mat))
(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (r)
           (accumulate cons nil (map
                                 (lambda (w) (dot-product r w))
                                 cols)))
         m)))


; Test
(define m (list (list 1 2 3 4) (list 4 5 6 6) (list 6 7 8 9)))
; ┌1 2 3 4┐
; │4 5 6 6│
; └6 7 8 9┘
(define n (list (list 1 2 3) (list 1 2 3) (list 1 2 3) (list 1 2 3)))
; ┌1 2 3┐
; │1 2 3│
; │1 2 3│
; └1 2 3┘


(matrix-*-vector m (list 1 2 3 4))
; (30 56 80)
; ┌1 2 3 4┐ ┌1┐    ┌30┐
; │4 5 6 6│*│2│ => │56│
; └6 7 8 9┘ │3│    └80┘
;           └4┘

(transpose m)
; ((1 4 6) (2 5 7) (3 6 8) (4 6 9))
; ┌1 2 3 4┐      ┌1 4 6┐
; │4 5 6 6│ =>*T │2 5 7│
; └6 7 8 9┘      │3 6 8│
;                └4 6 9┘

(matrix-*-matrix m n)
; ((10 20 30) (21 42 63) (30 60 90))
; ┌1 2 3 4┐┌1 2 3┐    ┌10 20 30┐
; │4 5 6 6││1 2 3│ => │21 42 63│
; └6 7 8 9┘│1 2 3│    └30 60 90┘
;          └1 2 3┘




