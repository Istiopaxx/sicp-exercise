#lang sicp



; Exercise 2.83.


; Suppose you are designing a generic arithmetic system for dealing with the tower of types shown
; in figure 2.25: integer, rational, real, complex. For each type (except complex), design a
; procedure that raises objects of that type one level in the tower.

; Show how to install a generic raise operation that will work for each type (except complex).


;       Complex   // High
;          │
;         Real
;          │
;       Rational
;          │
;       Integer   // Low

; Figure 2.25: A tower of types.



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
; Define integer number system.

(define (install-integer-package)
  ;; internal procedures
  (define (make-int n)
    (floor n))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'integer x))
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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
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
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'real x))
  (put 'make 'real
       (lambda (n) (tag (make-real n))))
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))



; ==================================================
; Define complex number system.

; Define rectangular and polar complex system.
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

(define (install-polar-package)
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-polar-package)
(install-rectangular-package)

(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  ;; internal procedures : None of this is used in here..
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle)
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))



; ==================================================
; Define make operation and utils.

(define (numer r)
  ((get 'numer 'rational) r))
(define (denom r)
  ((get 'denom 'rational) r))



; ==================================================
; Define raise(coercion) system.

(define (install-coercion-package)
  (define (integer->rational n)
    (make-rational (contents n) 1))
  (define (rational->real n)
    (make-real (/ (numer n) (denom n))))
  (define (real->complex n)
    (make-complex-from-real-imag n 0))
  (put 'raise '(integer) integer->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real) real->complex)
  'done)

(install-coercion-package)

; Define generic raise operation.
(define (raise x)
  (apply-generic 'raise x))


; Test.
(define z (make-integer 3))
(define q (make-rational 3 4.0))
(define r (make-real 3.4))

(raise z)
; (rational 3 . 1)
(raise q)
; (real . 0.75)
(raise r)
; (complex rectangular 3.4 . 0)

