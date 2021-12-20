#lang sicp


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


; Exercise 2.42.

; ┌─┐┌─┐┌─┐┌─┐┌─┐┼┼┼┌─┐┌─┐
; └─┘└─┘└─┘└─┘└─┘┼┼┼└─┘└─┘
; ┌─┐┌─┐┼┼┼┌─┐┌─┐┌─┐┌─┐┌─┐
; └─┘└─┘┼┼┼└─┘└─┘└─┘└─┘└─┘
; ┼┼┼┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐
; ┼┼┼└─┘└─┘└─┘└─┘└─┘└─┘└─┘
; ┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐┼┼┼┌─┐
; └─┘└─┘└─┘└─┘└─┘└─┘┼┼┼└─┘
; ┌─┐┌─┐┌─┐┌─┐┼┼┼┌─┐┌─┐┌─┐
; └─┘└─┘└─┘└─┘┼┼┼└─┘└─┘└─┘
; ┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐┼┼┼
; └─┘└─┘└─┘└─┘└─┘└─┘└─┘┼┼┼
; ┌─┐┼┼┼┌─┐┌─┐┌─┐┌─┐┌─┐┌─┐
; └─┘┼┼┼└─┘└─┘└─┘└─┘└─┘└─┘
; ┌─┐┌─┐┌─┐┼┼┼┌─┐┌─┐┌─┐┌─┐
; └─┘└─┘└─┘┼┼┼└─┘└─┘└─┘└─┘
; Figure 2.8:  A solution to the eight-queens puzzle.

; The ``eight-queens puzzle'' asks how to place eight queens on a chessboard so that no queen is in
; check from any other (i.e., no two queens are in the same row, column, or diagonal).
; One possible solution is shown in figure 2.8. One way to solve the puzzle is to work across
; the board, placing a queen in each column.
; Once we have placed k - 1 queens, we must place the k_th queen in a position where it does not
; check any of the queens already on the board.
; We can formulate this approach recursively: Assume that we have already generated the sequence
; of all possible ways to place k - 1 queens in the first k - 1 columns of the board.
; For each of these ways, generate an extended set of positions by placing a queen
; in each row of the kth column. Now filter these, keeping only the positions for which the queen
; in the kth column is safe with respect to the other queens.
; This produces the sequence of all ways to place k queens in the first k columns.
; By continuing this process, we will produce not only one solution, but all solutions to the puzzle.

; We implement this solution as a procedure queens, which returns a sequence of all solutions
; to the problem of placing n queens on an n x n chessboard.
; Queens has an internal procedure queen-cols that returns the sequence of all ways to place queens
; in the first k columns of the board.

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


; In this procedure rest-of-queens is a way to place k - 1 queens in the first k - 1 columns,
; and new-row is a proposed row in which to place the queen for the kth column.
; Complete the program by implementing the representation for sets of board positions, including
; the procedure adjoin-position, which adjoins a new row-column position to a set of positions,
; and empty-board, which represents an empty set of positions.

; You must also write the procedure safe?, which determines for a set of positions, whether the queen
; in the kth column is safe with respect to the others.
; (Note that we need only check whether the new queen is safe -- the other queens are already
; guaranteed safe with respect to each other.)

; ==================================================
; Define representation of board position(x/y pair) as a list.
(define (make-position x y) (list x y))
(define (get-x p) (car p))
(define (get-y p) (cadr p))

; Define sets of board positions as a list.
; Define empty set to nil
(define empty-board nil)

; ==================================================
; Define board position operator.
(define (horizontal-unsafe? p1 p2)
  (= (get-y p1) (get-y p2)))
(define (vertical-unsafe? p1 p2)
  (= (get-x p1) (get-x p2)))
(define (diagonal-unsafe? p1 p2)
  (= (abs (- (get-x p1) (get-x p2))) (abs (- (get-y p1) (get-y p2)))))

; Define adjoin-position by cons: => it makes easier to implement safe? procedure.
(define (adjoin-position new-row col positions)
  (cons (make-position col new-row) positions))

; Define safe? => not actually used k: because k-th queen's position is on first of queens' set.
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


; ==================================================
; Test.
(queens 4)
; ( // queens' location list
;  ((4 3) (3 1) (2 4) (1 2))
;  ((4 2) (3 4) (2 1) (1 3))
; )
(queens 5)
; ( // queens' location list
;  ((5 4) (4 2) (3 5) (2 3) (1 1))
;  ((5 3) (4 5) (3 2) (2 4) (1 1))
;  ((5 5) (4 3) (3 1) (2 4) (1 2))
;  ((5 4) (4 1) (3 3) (2 5) (1 2))
;  ((5 5) (4 2) (3 4) (2 1) (1 3))
;  ((5 1) (4 4) (3 2) (2 5) (1 3))
;  ((5 2) (4 5) (3 3) (2 1) (1 4))
;  ((5 1) (4 3) (3 5) (2 2) (1 4))
;  ((5 3) (4 1) (3 4) (2 2) (1 5))
;  ((5 2) (4 4) (3 1) (2 3) (1 5))
; )