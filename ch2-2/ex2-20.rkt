#lang sicp

(define <body> 0)

; Exercise 2.20.  The procedures +, *, and list take arbitrary numbers of arguments. One way to
; define such procedures is to use define with dotted-tail notation. In a procedure definition,
; a parameter list that has a dot before the last parameter name indicates that, when the procedure
; is called, the initial parameters (if any) will have as values the initial arguments, as usual,
; but the final parameter's value will be a list of any remaining arguments.

; For instance, given the definition
(define (f x y . z) <body>)
; the procedure f can be called with two or more arguments.

; If we evaluate
(f 1 2 3 4 5 6)
; then in the body of f, x will be 1, y will be 2, and z will be the list (3 4 5 6).

; Given the definition
(define (g . w) <body>)
; the procedure g can be called with zero or more arguments.

; If we evaluate
(g 1 2 3 4 5 6)
; then in the body of g, w will be the list (1 2 3 4 5 6).11


; ==================================================
; Use this notation to write a procedure same-parity that takes one or more integers and returns
; a list of all the arguments that have the same even-odd parity as the first argument.



(define (same-parity f . seq)
  (define (append list1 item)
    (if (null? list1)
        (cons item nil)
        (cons (car list1) (append (cdr list1) item))))
  (define (filter proc items)
    (define (filter-iter inp ret)
      (cond ((null? inp) ret)
            ((proc (car inp)) (filter-iter (cdr inp) (append ret (car inp))))
            (else (filter-iter (cdr inp) ret))))
    (filter-iter items nil))
  (define (even? x) (= (remainder x 2) 0))
  (define (odd? x) (= (remainder x 2) 1))
  (if (even? f)
      (filter even? (cons f seq))
      (filter odd? (cons f seq))))

(same-parity 1 2 3 4 5 6 7)
; (1 3 5 7)
(same-parity 2 3 4 5 6 7)
; (2 4 6)



; ==================================================
; alternative solution - append is expensive, just create new list in reverse order
; and reverse whole list again after the end of iteration.

(define (same-parity-alter f . seq)
  (define (reverse input-list)
    (define (reverse-iter input result)
      (if (null? input)
          result
          (reverse-iter (cdr input)
                        (cons (car input) result))))
    (reverse-iter input-list nil))
  (define (filter proc items)
    (define (filter-iter inp ret)
      (cond ((null? inp) ret)
            ((proc (car inp)) (filter-iter (cdr inp) (cons (car inp) ret)))
            (else (filter-iter (cdr inp) ret))))
    (reverse (filter-iter items nil)))
  (if (even? f)
      (filter even? (cons f seq))
      (filter odd? (cons f seq))))

(same-parity-alter 1 2 3 4 5 6 7)
; (1 3 5 7)
(same-parity-alter 2 3 4 5 6 7)
; (2 4 6)  
