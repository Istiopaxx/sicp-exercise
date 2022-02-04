#lang sicp



; ==================================================
; Define put/get, put/get-coercion, tag system.
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
        (else (error "Bad tagged datum -- TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        (else (error "Bad tagged datum -- CONTENTS" datum))))

(define type-hierarchy
  (list 'complex 'real 'rational 'integer))

(define (high? target criteria)
  (define (iter t c type-hierarchy)
    (cond ((equal? t c) #f)
          ((null? type-hierarchy) (error "unknown types -- high?" (list t c)))
          ((equal? t (car type-hierarchy)) #t)
          ((equal? c (car type-hierarchy)) #f)
          (else (iter t c (cdr type-hierarchy)))))
  (iter target criteria type-hierarchy))



; ==================================================
; Exercise 2.84.

; Using the raise operation of exercise 2.83, modify the apply-generic procedure so that it coerces
; its arguments to have the same type by the method of successive raising, as discussed in this
; section. You will need to devise a way to test which of two types is higher in the tower.

; Do this in a manner that is "compatible" with the rest of the system and will not lead to
; problems in adding new levels to the tower.



; Define apply-generic and raise.
(define (apply-generic op . args)
  ; Define generic raise operation.
  (define (raise x)
    ((get 'raise (list (type-tag x))) (contents x)))
  (define (raise-until-same x target-type)
    (if (high? target-type (type-tag x))
        (raise-until-same (raise x) target-type)
        x))
  (define (iter type-tags target-list args)
    (if (pair? target-list)
        (let ((target-type (car target-list)))
          (let ((raised-args (map (lambda (x) (raise-until-same x target-type)) args)))
            (if (equal-check target-type (map type-tag raised-args))
                (apply apply-generic (append (list op) raised-args))
                (iter type-tags (cdr target-list) args))))
        (error "No method for these types -- not exist coercion strategy" (list op type-tags))))
  (define (equal-check target type-list)
    (cond ((null? type-list) #t)
          ((equal? target (car type-list))
           (equal-check target (cdr type-list)))
          (else #f)))
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (iter type-tags type-tags args)))))



; ==================================================
; Define integer number system.

(define (install-integer-package)
  ;; internal procedures
  (define (make-int n)
    (floor n))
  (define (add-int x y) (+ x y))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer)
       (lambda (x y) (tag (add-int x y))))
  (put 'make 'integer
       (lambda (x) (tag (make-int x))))
  'done)

(install-integer-package)

(define (make-integer n)
  ((get 'make 'integer) n))



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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  'done)

(install-rational-package)

(define (make-rational n d)
  ((get 'make 'rational) n d))



; ==================================================
; Define real number system.

(define (install-real-package)
  ;; internal procedures
  (define (make-real n)
    n)
  (define (add-real x y) (+ x y))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real)
       (lambda (x y) (tag (add-real x y))))
  (put 'make 'real
       (lambda (n) (tag (make-real n))))
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))



; ==================================================
; Define complex number system.

; Define rectangular complex system.
(define (square x) (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(install-rectangular-package)

(define (install-complex-package)
  ;; imported procedures from rectangular package
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))



; ==================================================
; Define raise(coercion) system.

(define (install-coercion-package)
  ; Define utils.
  (define (numer r)
    ((get 'numer 'rational) r))
  (define (denom r)
    ((get 'denom 'rational) r))
  ; Define raise in each case.
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real n)
    (make-real (/ (numer n) (denom n))))
  (define (real->complex n)
    (make-complex-from-real-imag n 0))
  ; interface to the rest of the system
  (put 'raise '(integer) integer->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real) real->complex)
  'done)

(install-coercion-package)




; ==================================================
; Test.

(define (add x y) (apply-generic 'add x y))

(define z (make-integer 5))
(define q (make-rational 3 4))
(define r (make-real 2.5))
(define c (make-complex-from-real-imag 2 3))



(add z q)
; (rational 23 . 4)
(add z r)
; (real . 7.5)
(add z c)
; (complex rectangular 7 . 3)
(add q r)
; (real . 3.25)
(add q c)
; (complex rectangular 11/4 . 3)
(add r c)
; (complex rectangular 4.5 . 3)

