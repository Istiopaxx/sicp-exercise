#lang sicp


; Exercise 2.38.
; The accumulate procedure is also known as fold-right, because it combines the first element of 
; the sequence with the result of combining all the elements to the right. There is also a fold-left,
; which is similar to fold-right, except that it combines elements working in the opposite direction:

(define (fold-right op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (fold-right op initial (cdr sequence)))))

(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

; What are the values of

(fold-right / 1.0 (list 1 2 3))     ; 1.5
(fold-left / 1.0 (list 1 2 3))      ; 0.16666666666666666
(fold-right list nil (list 1 2 3))  ; (1 (2 (3 ())))     
(fold-left list nil (list 1 2 3))   ; (((() 1) 2) 3)   

; Give a property that op should satisfy to guarantee that fold-right and fold-left will produce 
; the same values for any sequence.

(fold-right + 0 (list 1 2 3))   ; 6
(fold-left + 0 (list 1 2 3))    ; 6
(fold-right * 0 (list 1 2 3))   ; 0
(fold-left * 0 (list 1 2 3))    ; 0
; op should satisfy commutative property to produce same result with fold-right and -left.