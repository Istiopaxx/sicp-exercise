#lang sicp


; Exercise 2.78.
; The internal procedures in the scheme-number package are essentially nothing more than calls to
; the primitive procedures +, -, etc. It was not possible to use the primitives of the language
; directly because our type-tag system requires that each data object have a type attached to it.

; In fact, however, all Lisp implementations do have a type system, which they use internally.
; Primitive predicates such as symbol? and number? determine whether data objects have particular
; types. Modify the definitions of type-tag, contents, and attach-tag from section 2.4.2 so that our
; generic system takes advantage of Scheme's internal type system. That is to say, the system should
; work as before except that ordinary numbers should be represented simply as Scheme numbers rather
; than as pairs whose car is the symbol scheme-number.


; ==================================================
; Define put/get, tag system, apply-generic.
(define table (list))

(define (put op type proc)
  (set! table (append table (list (list op type proc)))))

(define (get op type)
  (define (search op type t)
    (cond ((null? t) (error "unknown type in table -- GET"))
          ((and (equal? (caar t) op) (equal? (cadar t) type))
           (caddar t))
          (else (search op type (cdr t)))))
  (search op type table))

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
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (error
           "No method for these types -- APPLY-GENERIC"
           (list op type-tags))))))




; ==================================================
; Define scheme natural number system.

(define (install-scheme-number-package)
  (put 'add '(scheme-number scheme-number) (lambda (x y) (+ x y)))
  (put 'sub '(scheme-number scheme-number) (lambda (x y) (- x y)))
  (put 'mul '(scheme-number scheme-number) (lambda (x y) (* x y)))
  (put 'div '(scheme-number scheme-number) (lambda (x y) (/ x y)))
  'done)

(install-scheme-number-package)




; ==================================================
; Define generic operations.

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))



; ==================================================
; Test.

(add 5 4)
; 9
(sub 10 8.4)
; 1.5999999999999996
(mul 4 5.5)
; 22.0
(div 7 4.0)
; 1.75

