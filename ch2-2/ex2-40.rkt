#lang sicp

; Exercise 2.40.
; Define a procedure unique-pairs that, given an integer n, generates the sequence of pairs (i,j)
; with 1< j< i< n. Use unique-pairs to simplify the definition of prime-sum-pairs given above.

(define (square x) (* x x))
(define (prime? n)
  (define (fast-prime? n times)
    (define (nontrivial-sqrt? a n)
      (if (and (and (not (= a 1))
                    (not (= a (- n 1))))
               (= (remainder (square a) n) 1))
          0
          (remainder (square a) n)))
    (define (expmod base exp m)
      (cond ((= exp 0) 1)
            ((even? exp)
             (nontrivial-sqrt? (expmod base (/ exp 2) m) m))
            (else
             (remainder (* base (expmod base (- exp 1) m))
                        m))))
    (define (miller-rabin-test n)
      (define (try-it a)
        (= (expmod a (- n 1) n) 1))
      (try-it (+ 1 (random (- n 1)))))
    (cond ((= times 0 ) #t)
          ((miller-rabin-test n) (fast-prime? n (- times 1)))
          (else #f)))
  (fast-prime? n 30))

(define (filter predicate sequence)
  (cond ((null? sequence) nil)
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      nil
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append nil (map proc seq)))
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))


; ==================================================
; Define unique-pairs.
(define (unique-pairs n)
  (flatmap
   (lambda (i)
     (map (lambda (j) (list i j))
          (enumerate-interval 1 (- i 1))))
   (enumerate-interval 1 n)))

; Redefine prime-sum-pairs using unique-pairs.
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? (unique-pairs n))))

(prime-sum-pairs 6)
; ((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7) (6 1 7) (6 5 11))



