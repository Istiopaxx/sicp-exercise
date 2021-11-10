#lang sicp

; Exercise 2.11.  In passing, Ben also cryptically comments: "By testing the signs of the endpoints
; of the intervals, it is possible to break mul-interval into nine cases, only one of which requires
; more than two multiplications." Rewrite this procedure using Ben's suggestion.


(define (make-interval a b) (cons a b))
(define (lower-bound i) (car i))
(define (upper-bound i) (cdr i))


(define (mul-interval x y)
  (define (endpoint-sign i)
    ; both positive -> 1
    ; both negetive -> -1
    ; oposite endpoint -> 0
    (cond ((and (>= (upper-bound i) 0)
                (>= (lower-bound i) 0))
           1)
          ((and (< (upper-bound i) 0)
                (< (lower-bound i) 0))
           -1)
          (else 0)))
  (let ((flag-x (endpoint-sign x))
        (flag-y (endpoint-sign y))
        (xlyl (* (lower-bound x) (lower-bound y)))
        (xlyu (* (lower-bound x) (upper-bound y)))
        (xuyl (* (upper-bound x) (lower-bound y)))
        (xuyu (* (upper-bound x) (upper-bound y))))
    (cond ((> flag-x 0) (cond ((> flag-y 0) (make-interval xlyl xuyu))
                              ((< flag-y 0) (make-interval xuyl xlyu))
                              (else (make-interval xuyl xuyu))))
          ((< flag-x 0) (cond ((> flag-y 0) (make-interval xlyu xuyl))
                              ((< flag-y 0) (make-interval xuyu xlyl))
                              (else (make-interval xlyu xlyl))))
          (else (cond ((> flag-y 0) (make-interval xlyu xuyu))
                      ((< flag-y 0) (make-interval xuyl xlyl))
                      ; Compare values
                      (else (make-interval (min xuyl xlyu) (max xlyl xuyu))))))))




; ==================================================
; test

(define (old-mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define a (make-interval 1 5))
(define b (make-interval -5 -2))
(define c (make-interval -4 7))

; case 1
(mul-interval a a)      ; (1 . 25)
(old-mul-interval a a)  ; (1 . 25)  
; case 2
(mul-interval a b)      ; (-25 . -2)
(old-mul-interval a b)  ; (-25 . -2)
; case 3
(mul-interval a c)      ; (-20 . 35)
(old-mul-interval a c)  ; (-20 . 35)
; case 4
(mul-interval b a)      ; (-25 . -2)
(old-mul-interval b a)  ; (-25 . -2)
; case 5
(mul-interval b b)      ; (4 . 25)
(old-mul-interval b b)  ; (4 . 25)
; case 6
(mul-interval b c)      ; (-35 . 20)
(old-mul-interval b c)  ; (-35 . 20)
; case 7
(mul-interval c a)      ; (-20 . 35)
(old-mul-interval c a)  ; (-20 . 35)
; case 8
(mul-interval c b)      ; (-35 . 20)
(old-mul-interval c b)  ; (-35 . 20)
; case 9 -> check value
(mul-interval c c)      ; (-28 . 49)
(old-mul-interval c c)  ; (-28 . 49)