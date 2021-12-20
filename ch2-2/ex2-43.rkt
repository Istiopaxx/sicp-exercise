#lang sicp


; Exercise 2.43.

; Louis Reasoner is having a terrible time doing exercise 2.42. His queens procedure seems to work,
; but it runs extremely slowly.
; (Louis never does manage to wait long enough for it to solve even the 6× 6 case.)
; When Louis asks Eva Lu Ator for help, she points out that he has interchanged the order of the
; nested mappings in the flatmap, writing it as below.

(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))

; Explain why this interchange makes the program run slowly.
; Estimate how long it will take Louis's program to solve the eight-queens puzzle,
; assuming that the program in exercise 2.42 solves the puzzle in time T.


; ==================================================
; Explanation.

; Original version computes enumerate-interval for (queen-cols (- k 1)) times.
; So, original compute queen-cols only once at one step.
; Assuming T(N) as the computing time when input is N.

; Above version computes queen-cols for (enumerate-interval 1 n)(= n) times.
; So, this version computes (n-1) queen-cols more than original at one step.
; Each step(recursion) is only once, step is n steps, so computes n*(n-1) queen-cols more.
; Assuming T_s(N) as the computing time when input is N.

; => T_s(N) is approximately N*N times slower than Original T(N).
; See the test results below.


; ==================================================
; Implementation.
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
(define (make-position x y) (list x y))
(define (get-x p) (car p))
(define (get-y p) (cadr p))
(define empty-board nil)
(define (horizontal-unsafe? p1 p2)
  (= (get-y p1) (get-y p2)))
(define (vertical-unsafe? p1 p2)
  (= (get-x p1) (get-x p2)))
(define (diagonal-unsafe? p1 p2)
  (= (abs (- (get-x p1) (get-x p2))) (abs (- (get-y p1) (get-y p2)))))
(define (adjoin-position new-row col positions)
  (cons (make-position col new-row) positions))
(define (safe? k positions)
  (let ((now (car positions))
        (rest (cdr positions)))
    (accumulate (lambda (a b) (and a b))
                #t
                (map (lambda (p)
                       (not (or (horizontal-unsafe? now p)
                                (vertical-unsafe? now p)
                                (diagonal-unsafe? now p))))
                     rest))))


(define (queens-slow board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (new-row)
            (map (lambda (rest-of-queens)
                   (adjoin-position new-row k rest-of-queens))
                 (queen-cols (- k 1))))
          (enumerate-interval 1 board-size)))))
  (queen-cols board-size))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter
         (lambda (positions) (safe? k positions))
         (flatmap
          (lambda (rest-of-queens)
            (map (lambda (new-row)
                   (adjoin-position new-row k rest-of-queens))
                 (enumerate-interval 1 board-size)))
          (queen-cols (- k 1))))))
  (queen-cols board-size))



; ==================================================
; Test.
(define t1 (runtime))
(queens 6)
(display (- (runtime) t1))
(newline)
; time : 582

(define t2 (runtime))
(queens-slow 6)
(display (- (runtime) t2))
(newline)
; time : 17350 ~= 582 * (6-1)*5

