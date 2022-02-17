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
; Exercise 2.85.

; This section mentioned a method for "simplifying" a data object by lowering it in the tower of
; types as far as possible. Design a procedure drop that accomplishes this for the tower described
; in exercise 2.83. The key is to decide, in some general way, whether an object can be lowered.

; For example, the complex number 1.5 + 0i can be lowered as far as real, the complex number 1 + 0i
; can be lowered as far as integer, and the complex number 2 + 3i cannot be lowered at all.
; Here is a plan for determining whether an object can be lowered: Begin by defining a generic
; operation project that "pushes" an object down in the tower.

; For example, projecting a complex number would involve throwing away the imaginary part. Then a
; number can be dropped if, when we project it and raise the result back to the type we started with,
; we end up with something equal to what we started with. Show how to implement this idea in detail,
; by writing a drop procedure that drops an object as far as possible.

; You will need to design the various projection operations and install project as a generic
; operation in the system. You will also need to make use of a generic equality predicate, such as
; described in exercise 2.79. Finally, use drop to rewrite apply-generic from exercise 2.84 so that
; it "simplifies" its answers.




; Define apply-generic
(define (apply-generic op . args)
  ; Define raise operation.
  (define (raise x)
    ((get 'raise (list (type-tag x))) (contents x)))
  (define (raise-until-same x target-type)
    (if (high? target-type (type-tag x))
        (raise-until-same (raise x) target-type)
        x))
  ; Define drop operation.
  (define (drop-until-impossible x)
    (let ((drop (get 'drop (list (type-tag x)))))
      (if drop
          (let ((dropped (drop (contents x))))
            (if dropped
                (drop-until-impossible dropped)
                x))
          x)))
  ; Define apply-generic operation.
  (define (iter type-tags target-list args)
    (if (pair? target-list)
        (let ((target-type (car target-list)))
          (let ((raised-args (map (lambda (x) (raise-until-same x target-type)) args)))
            (if (equal-check target-type (map type-tag raised-args))
                (drop-until-impossible (apply apply-generic (append (list op) raised-args)))
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
    (inexact->exact (floor n)))
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
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))



; ==================================================
; Define raise, drop(coercion) system.

(define (install-coercion-package)
  ; Define utils.
  (define (numer r)
    ((get 'numer 'rational) r))
  (define (denom r)
    ((get 'denom 'rational) r))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  ; Define raise in each case.
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real n)
    (make-real (/ (numer n) (denom n))))
  (define (real->complex n)
    (make-complex-from-real-imag n 0))
  ; Define drop in each case.
  (define (complex->real n)
    (if (= 0 (imag-part n))
        (make-real (real-part n))
        #f))
  (define (real->integer n)
    (if (= (floor n) n)
        (make-integer n)
        #f))
  (define (rational->integer n)
    (if (= (denom n) 1)
        (make-integer (numer n))
        #f))
  ; interface to the rest of the system
  (put 'raise '(integer) integer->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real) real->complex)
  (put 'drop '(complex) complex->real)
  (put 'drop '(real) real->integer)
  (put 'drop '(rational) rational->integer)
  'done)

(install-coercion-package)




; ==================================================
; Test.

(define (add x y) (apply-generic 'add x y))

(define z (make-integer 5))
(define q (make-rational 1 2))
(define r1 (make-real 2.0))
(define r2 (make-real 2.3))
(define c1 (make-complex-from-real-imag 2.5 0))
(define c2 (make-complex-from-real-imag 2 0))


(add z r1) ; add integer and real
; (integer . 7)

(add r2 c1) ; add real and complex
; (real . 4.8)

(add r1 c2) ; add real and complex
; (integer . 4)

(add q q) ; add rational and rational
; (rational 1 . 1)
