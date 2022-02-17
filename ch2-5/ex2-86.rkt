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
        ; (error "Bad tagged datum -- TYPE-TAG" datum)
        (else #f)))

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
    (make-complex-from-real-imag (make-real n) (make-real 0)))
  ; Define drop in each case.
  (define (=zero? x)
    (apply-generic '=zero? x))
  (define (complex->real n)
    (if (=zero? (imag-part n))
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
    (let ((type (type-tag x)))
      (if type
          (let ((drop (get 'drop (list type))))
            (if drop
                (let ((dropped (drop (contents x))))
                  (if dropped
                      (drop-until-impossible dropped)
                      x))
                x))
          x)))
  ; Define apply-generic operation.
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
          (drop-until-impossible (apply proc (map contents args)))
          (iter type-tags type-tags args)))))



; ==================================================
; Define integer number system.

(define (install-integer-package)
  ;; internal procedures
  (define (make-int n)
    (inexact->exact (floor n)))
  (define (add-int x y) (+ x y))
  (define (mul-int x y) (* x y))
  (define (cosine x) (cos x))
  (define (sine x) (sin x))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'integer x))
  (put 'cosine '(integer)
       (lambda (x) (tag (cosine x))))
  (put 'sine '(integer)
       (lambda (x) (tag (sine x))))
  (put 'add '(integer integer)
       (lambda (x y) (tag (add-int x y))))
  (put 'mul '(integer integer)
       (lambda (x y) (tag (mul-int x y))))
  (put 'make 'integer
       (lambda (x) (tag (make-int x))))
  (put '=zero? '(integer) (lambda (x) (= x 0)))
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
  (define (=zero? x)
    (= (numer x) 0))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'numer 'rational numer)
  (put 'denom 'rational denom)
  (put '=zero? '(rational) =zero?)
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
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
  (define (mul-real x y) (* x y))
  (define (cosine x) (cos x))
  (define (sine x) (sin x))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'real x))
  (put 'sine '(real)
       (lambda (x) (tag (sine x))))
  (put 'cosine '(real)
       (lambda (x) (tag (cosine x))))
  (put 'add '(real real)
       (lambda (x y) (tag (add-real x y))))
  (put 'mul '(real real)
       (lambda (x y) (tag (mul-real x y))))
  (put 'make 'real
       (lambda (n) (tag (make-real n))))
  (put '=zero? '(real) (lambda (x) (= x 0)))
  'done)

(install-real-package)

(define (make-real n)
  ((get 'make 'real) n))




; ==================================================
; Exercise 2.86.

; Suppose we want to handle complex numbers whose real parts, imaginary parts, magnitudes, and angles
; can be either ordinary numbers, rational numbers, or other numbers we might wish to add
; to the system.

; Describe and implement the changes to the system needed to accommodate this.
; You will have to define operations such as sine and cosine that are generic over ordinary numbers
; and rational numbers.


; First, rectangular and polar representation must have the two values of the real/imaginary and 
; magnitude/angle parts separately. So cons is not enough: we must make two types divided.

; Second, magnitude/angle must can be converted to real/imaginary parts, so generic sine and cosine
; is needed. Also, generic multiplication needed.


; ==================================================
; Define complex number system.

; Define rectangular complex system.
(define (square x) (* x x))

(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cadr z))
  (define (make-from-real-imag x y) (list x y))
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
  (define (angle z) (cadr z))
  (define (make-from-mag-ang r a) (list r a))
  (define (mul x y)
    (apply-generic 'mul x y))
  (define (cosine x)
    (apply-generic 'cosine x))
  (define (sine x)
    (apply-generic 'sine x))
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
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
  ;; imported procedures from rectangular package
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang x y)
    ((get 'make-from-mag-ang 'polar) x y))
  (define (real-part z)
    (apply-generic 'real-part z))
  (define (imag-part z)
    (apply-generic 'imag-part z))
  (define (magnitude z)
    (apply-generic 'magnitude z))
  (define (angle z)
    (apply-generic 'angle z))
  (define (add x y) (apply-generic 'add x y))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
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
  (put 'make-from-mag-ang 'complex
       (lambda (x y) (tag (make-from-mag-ang x y))))
  'done)

(install-complex-package)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-from-mag-ang x y)
  ((get 'make-from-mag-ang 'complex) x y))




; ==================================================
; Test.

(define (add x y) (apply-generic 'add x y))

(define q (make-rational 1 2))
(define r (make-real 60.5))
(define c1 (make-complex-from-real-imag q r))
(define c2 (make-from-mag-ang q r))


c2
;(complex polar (rational 1 . 2) (real . 60.5))

(add r c2)
;(complex rectangular (real . 60.15515648643192) (real . -0.3620537959337248))

(add q c2)
;(complex rectangular (real . 0.15515648643191682) (real . -0.3620537959337248))

(add c1 c2)
;(complex rectangular (real . 0.15515648643191682) (real . 60.137946204066274))
