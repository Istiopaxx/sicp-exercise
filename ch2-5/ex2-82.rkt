#lang sicp




; ==================================================
; Define put/get, put/get-coercion, tag system, apply-generic.
(define table (list))

(define (put op type proc)
  (set! table (append table (list (list op type proc)))))

(define (get op type)
  (define (search op type t)
    (cond ((null? t) #f)  ; swallow error and get back #f
          ; (error "unknown type in table -- GET")
          ((and (equal? (caar t) op) (equal? (cadar t) type))
           (caddar t))
          (else (search op type (cdr t)))))
  (search op type table))

(define (put-coercion t1 t2 proc)
  (set! table (append table (list (list t1 t2 proc)))))

(define (get-coercion t1 t2)
  (define (search t1 t2 table)
    (cond ((null? table) #f) ;  swallow error and get back #f
          ; (error "unknown type coercion in table -- GET-COERCION")
          ((and (equal? (caar table) t1) (equal? (cadar table) t2))
           (caddar table))
          (else (search t1 t2 (cdr table)))))
  (search t1 t2 table))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define (apply-generic op . args)
  (define (iter type-tags target-list args)
    (if (pair? target-list)
        (let ((target-type (car target-list)))
          (let ((coercions (map (lambda (x) (get-coercion x target-type)) type-tags)))
            (if (exist-check coercions)
                (let ((coerced-args (map (lambda (x y) (apply x (list y))) coercions args)))
                  (apply apply-generic (append (list op) coerced-args)))
                (iter type-tags (cdr target-list) args))))
        (error "No method for these types -- not exist coercion strategy" (list op type-tags))))
  (define (exist-check proc-list)
    (cond ((null? proc-list) #t)
          ((car proc-list) (exist-check (cdr proc-list)))
          (else #f)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (iter type-tags type-tags args)))))



; ==================================================
; Define scheme natural number system.

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number) (lambda (x y) (+ x y)))
  (put 'add3 '(scheme-number scheme-number scheme-number) (lambda (x y z) (+ x y z)))
  (put 'add4 '(scheme-number scheme-number scheme-number scheme-number)
       (lambda (a b c d) (+ a b c d)))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (/ x y)))
  (put 'equ? '(scheme-number scheme-number) (lambda (x y) (= x y)))
  (put '=zero? '(scheme-number) (lambda (x) (= x 0)))
  'done)

(install-scheme-number-package)



; ==================================================
; Define rational number system.

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (equ? x y)
    (let ((nx (numer x))
          (dx (denom x))
          (ny (numer y))
          (dy (denom y)))
      (and (and (< 0 (* nx ny dx dy)))
           (and (= (abs nx) (abs ny))
                (= (abs dx) (abs dy))))))
  (define (=zero? x)
    (= (numer x) 0))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'add3 '(rational rational rational)
       (lambda (x y z) (tag (add-rat (add-rat x y) z))))
  (put 'add4 '(rational rational rational rational)
       (lambda (a b c d) (tag (add-rat (add-rat (add-rat a b) c) d))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'equ? '(rational rational) equ?)
  (put '=zero? '(rational) =zero?)
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))



; ==================================================
; Exercise 2.82.

; Show how to generalize apply-generic to handle coercion in the general case of multiple arguments.
; One strategy is to attempt to coerce all the arguments to the type of the first argument,
; then to the type of the second argument, and so on.

; Give an example of a situation where this strategy (and likewise the two-argument version given
; above) is not sufficiently general. (Hint: Consider the case where there are some suitable
; mixed-type operations present in the table that will not be tried.)


; ==================================================
; Install Coercions.
(define (install-coercion-package)
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (define (scheme-number->scheme-number n) n)
  (define (rational->rational z) z)
  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'scheme-number
                scheme-number->scheme-number)
  (put-coercion 'rational 'rational rational->rational)
  'done)

(install-coercion-package)



; ==================================================
; Define generic operations.
(define (add3 a b c)
  (apply-generic 'add3 a b c))
(define (add4 a b c d)
  (apply-generic 'add4 a b c d))

(define r1 (make-rational 3 4))
(define r2 (make-rational 8 3))



; ==================================================
; Test.
(add3 r1 10 r2)
; (rational 161 . 12)
(add3 1 2 3)
; 6
(add3 r1 r1 r2)
; (rational 25 . 6)

(add4 5 r1 8 r2)
; (rational 197 . 12)
(add4 1 2 3 4)
; 10
(add4 r1 r1 r1 r2)
; (rational 59 . 12)



; ==================================================
; This strategy is not sufficient in below situation.

; Ex)
; types: A B C
; registered op: (op some-A some-B some-B)
; registered coercion: A->B, C->B
; Situation: Evaluating (apply-generic op A B C) will only try (op A B C), (op B B B) and fail
;            while we can just coerce C to B to evaluate (op A B B) instead

; I just didn't deriv this situation, so this is from community solution..
; Reference : http://community.schemewiki.org/?sicp-ex-2.82
