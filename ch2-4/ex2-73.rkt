#lang sicp



; ==================================================
; Define deriv - data abstraction.
(define (variable? x) (symbol? x))

(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation base exponent)
  (cond ((=number? exponent 0) 1)
        ((=number? exponent 1) base)
        ((and (number? base) (number? exponent)) (exp base exponent))
        (else (list '** base exponent))))

(define (sum? x)
  (and (pair? x) (eq? (car x) '+)))

(define (addend s) (cadr s))

(define (augend s) (caddr s))

(define (product? x)
  (and (pair? x) (eq? (car x) '*)))

(define (multiplier p) (cadr p))

(define (multiplicand p) (caddr p))

(define (exponentiation? x)
  (and (pair? x) (eq? (car x) '**)))

(define (base e) (cadr e))

(define (exponent e) (caddr e))

(define (=number? exp num)
  (and (number? exp) (= exp num)))


; ==================================================
; Define put/get.
(define table (list))

(define (put op type proc)
  (set! table (append table (list (list op type proc)))))

(define (get op type)
  (define (search op type t)
    (cond ((null? t) (error "unknown type in table -- GET"))
          ((and (eqv? (caar t) op) (eqv? (cadar t) type))
           (caddar t))
          (else (search op type (cdr t)))))
  (search op type table))




; ==================================================
; Exercise 2.73.

; Section 2.3.2 described a program that performs symbolic differentiation:

(define (deriv exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        ((sum? exp)
         (make-sum (deriv (addend exp) var)
                   (deriv (augend exp) var)))
        ((product? exp)
         (make-sum
          (make-product (multiplier exp)
                        (deriv (multiplicand exp) var))
          (make-product (deriv (multiplier exp) var)
                        (multiplicand exp))))
        ; <more rules can be added here>
        (else (error "unknown expression type -- DERIV" exp))))

; We can regard this program as performing a dispatch on the type of the expression to be
; differentiated. In this situation the ``type tag'' of the datum is the
; algebraic operator symbol (such as +) and the operation being performed is deriv. We can transform
; this program into data-directed style by rewriting the basic derivative procedure as

(define (deriv2 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get 'deriv (operator exp)) (operands exp)
                                           var))))
(define (operator exp) (car exp))
(define (operands exp) (cdr exp))




; ==================================================
; a.  Explain what was done above. Why can't we assimilate the predicates number? and same-variable?
;     into the data-directed dispatch?

; deriv2 procedure get deriv procedure for proper type of operator, and apply gained procedure to
; operands and var.

; number? and same-variable? procedure is for constant number or one polynomial of var.
; Just a constant or one polynomial of var doesn't have operator, so it can't be assimilate to 
; data-directed dispatch.




; ==================================================
; b.  Write the procedures for derivatives of sums and products, and the auxiliary code required to
;     install them in the table used by the program above.

(define (install-sum-package)
  ; internal procedures
  (define (deriv-sum operands var)
    (let ((augend (car operands))
          (addend (cadr operands)))
      (make-sum (deriv addend var)
                (deriv augend var))))
  ; interface to the rest of the system
  (put 'deriv '+ deriv-sum))

(define (install-product-package)
  ; internal procedures
  (define (deriv-product operands var)
    (let ((multiplicand (car operands))
          (multiplier (cadr operands)))
      (make-sum
       (make-product multiplier
                     (deriv multiplicand var))
       (make-product (deriv multiplier var)
                     multiplicand))))
  ; interface to the rest of the system
  (put 'deriv '* deriv-product))


(install-sum-package)
(install-product-package)

(deriv2 '(* (* x y) (+ x 3)) 'x)
; (+ (* (+ x 3) y) (* x y))



; ==================================================
; c.  Choose any additional differentiation rule that you like, such as the one for exponents
;     (exercise 2.56), and install it in this data-directed system.

(define (install-exponentiation-package)
  ; internal procedures
  (define (deriv-exponentiation operands var)
    (let ((base (car operands))
          (exponent (cadr operands)))
      (make-product
       (make-product exponent
                     (make-exponentiation base (make-sum exponent -1)))
       (deriv base var))))
  ; interface to the rest of the system
  (put 'deriv '** deriv-exponentiation))

(install-exponentiation-package)

(deriv2 '(** x (+ y 3)) 'x)
; (* (+ y 3) (** x (+ (+ y 3) -1)))



; ==================================================
; d.  In this simple algebraic manipulator the type of an expression is the algebraic operator that
;     binds it together. Suppose, however, we indexed the procedures in the opposite way, so that
;     the dispatch line in deriv looked like

; ((get (operator exp) 'deriv) (operands exp) var)

; What corresponding changes to the derivative system are required?

(define (deriv3 exp var)
  (cond ((number? exp) 0)
        ((variable? exp) (if (same-variable? exp var) 1 0))
        (else ((get (operator exp) 'deriv) (operands exp)
                                           var))))

(define (install-sum-package2)
  ; internal procedures
  (define (deriv-sum operands var)
    (let ((augend (car operands))
          (addend (cadr operands)))
      (make-sum (deriv addend var)
                (deriv augend var))))
  ; interface to the rest of the system
  (put '+ 'deriv deriv-sum))

(define (install-product-package2)
  ; internal procedures
  (define (deriv-product operands var)
    (let ((multiplicand (car operands))
          (multiplier (cadr operands)))
      (make-sum
       (make-product multiplier
                     (deriv multiplicand var))
       (make-product (deriv multiplier var)
                     multiplicand))))
  ; interface to the rest of the system
  (put '* 'deriv deriv-product))

(define (install-exponentiation-package2)
  ; internal procedures
  (define (deriv-exponentiation operands var)
    (let ((base (car operands))
          (exponent (cadr operands)))
      (make-product
       (make-product exponent
                     (make-exponentiation base (make-sum exponent -1)))
       (deriv base var))))
  ; interface to the rest of the system
  (put '** 'deriv deriv-exponentiation))


; ==================================================
; Install operation packages.`
(install-sum-package2)
(install-product-package2)
(install-exponentiation-package2)

; Test.
(deriv3 '(* (* x y) (+ x 3)) 'x)
; (+ (* (+ x 3) y) (* x y))
(deriv3 '(** x (+ y 3)) 'x)
; (* (+ y 3) (** x (+ (+ y 3) -1)))

; Changes are just procedures in packages are put by their type is deriv: their op is the operation.
