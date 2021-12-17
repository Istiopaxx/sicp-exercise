#lang sicp

; Exercise 2.29.  A binary mobile consists of two branches, a left branch and a right branch. Each
; branch is a rod of a certain length, from which hangs either a weight or another binary mobile.
; We can represent a binary mobile using compound data by constructing it from two branches
; (for example, using list):

(define (make-mobile left right)
  (list left right))

; A branch is constructed from a length (which must be a number) together with a structure,
; which may be either a number (representing a simple weight) or another mobile:

(define (make-branch length structure)
  (list length structure))



; ==================================================
; a.  Write the corresponding selectors left-branch and right-branch, which return the branches of
;     a mobile, and branch-length and branch-structure, which return the components of a branch.

(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (car (cdr mobile)))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (car (cdr branch)))
(define (mobile? mobile)
  (and (pair? mobile) (= (length mobile) 2)))



; ==================================================
; b.  Using your selectors, define a procedure total-weight that returns the
;     total weight of a mobile.

(define (total-weight mobile-or-struct)
  (cond ((null? mobile-or-struct) 0)
        ((not (mobile? mobile-or-struct)) mobile-or-struct)
        (else (+ (total-weight (branch-structure (left-branch mobile-or-struct)))
                 (total-weight (branch-structure (right-branch mobile-or-struct)))))))

; Test.
(define m1 (make-mobile
            (make-branch 4 6)
            (make-branch 5
                         (make-mobile
                          (make-branch 3 7)
                          (make-branch 9 8)))))

;;          4  |  5
;;        +----+-----+
;;        6        3 |     9
;;               +---+---------+
;;               7             8

(total-weight m1)
; 21



; ==================================================
; c.  A mobile is said to be balanced if the torque applied by its top-left branch is equal to that
;     applied by its top-right branch (that is, if the length of the left rod multiplied by the
;     weight hanging from that rod is equal to the corresponding product for the right side) and
;     if each of the submobiles hanging off its branches is balanced. Design a predicate that tests
;     whether a binary mobile is balanced.

(define (torque branch)
  (* (branch-length branch)
     (total-weight (branch-structure branch))))
(define (balanced? mobile)
  (cond ((null? mobile) #f)
        ((not (mobile? mobile)) #t)
        (else (if (and (= (torque (left-branch mobile)) (torque (right-branch mobile)))
                       (and (balanced? (branch-structure (left-branch mobile)))
                            (balanced? (branch-structure (right-branch mobile)))))
                  #t
                  #f))))


(define m2 (make-mobile
            (make-branch 5 10)
            (make-branch 10 5)))
;;          5  |     10
;;       +-----+----------+
;;      10                5
(balanced? m2) ; #t


(define m3 (make-mobile
            (make-branch 5 10)
            (make-branch 5
                         (make-mobile
                          (make-branch 3 7)
                          (make-branch 7 3)))))

;;          5  |  5
;;       +-----+-----+
;;      10         3 |   7
;;               +---+-------+
;;               7           3
(balanced? m3) ; #t


(define m4 (make-mobile
            (make-branch 5 10)
            (make-branch 5
                         (make-mobile
                          (make-branch 3 6)
                          (make-branch 7 4)))))

;;          5  |  5
;;       +-----+-----+
;;      10         3 |   7          // not balanced sub-mobile
;;               +---+-------+
;;               6           4
(balanced? m4) ; #f



; ==================================================
; d.  Suppose we change the representation of mobiles so that the constructors are

(define (make-mobile2 left right)
  (cons left right))
(define (make-branch2 length structure)
  (cons length structure))

; How much do you need to change your programs to convert to the new representation?

; BELOW IS NEED TO BE CHANGED.. but not all the programs.
; ==> this is boundary of abstraction.

; (define (left-branch mobile)
;   (car mobile))
; (define (right-branch mobile)
;   (car (cdr mobile)))
; (define (branch-length branch)
;   (car branch))
; (define (branch-structure branch)
;   (car (cdr branch)))
; (define (mobile? mobile)
;   (and (pair? mobile) (= (length mobile) 2)))