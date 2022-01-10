#lang sicp



; ==================================================
; Define put/get, attach-tag.
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

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (type-tag datum)
  (if (pair? datum)
      (car datum)
      (error "Bad tagged datum -- TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
      (cdr datum)
      (error "Bad tagged datum -- CONTENTS" datum)))




; ==================================================
; Define division and file implementation: two division's record file is implemented differently.

; Below is for implementation detail:
; 1. For easy implementation, the employee name is unique integer.
; 2. For easy use, get-record and insert-record just swallow the error.


; Developer division: the personnel file is implemented by unordered list of records.
(define (install-develop-division-package)
  ; internal procedure
  (define (make-record name salary)
    (list name salary))
  (define (get-name record)
    (car record))
  (define (get-salary record)
    (cadr record))
  (define (insert-record record file)
    (if (null? file)
        (cons record nil)
        (if (equal? (get-name (car file)) (get-name record))
            file ; Throwing error can be a choice.
            ; (error "same name employee -- insert-record" record)
            (cons (car file) (insert-record record (cdr file))))))
  (define (get-record name file)
    (if (null? file)
        ; or.. can choose throwing (error "no name in this file -- get-record" name).
        #f
        (if (equal? (get-name (car file)) name)
            (car file)
            (get-record name (cdr file)))))
  ; interface to the rest of the system
  ; 1. type-checking for inputs
  ; 2. append type tag for output
  (define (tag x) (attach-tag 'develop x))
  (define (type-check x) (equal? (type-tag x) 'develop))
  (put 'make-record 'develop (lambda (n s) (tag (make-record n s))))
  (put 'insert-record 'develop (lambda (r f)
                                 (if (and (type-check r) (type-check f))
                                     (tag (insert-record (contents r) (contents f)))
                                     (error "invalid type -- expected develop in this"))))
  (put 'get-record 'develop (lambda (n f)
                              (if (type-check f)
                                  (tag (get-record n (contents f)))
                                  (error "invalid type -- expected develop in this"))))
  (put 'get-salary 'develop (lambda (r)
                              (if (type-check r)
                                  (get-salary (contents r))
                                  (error "invalid type -- expected develop in this")))))


; Designer division: the personnel file is implemented by ordered(increasing order) list of records.
(define (install-design-division-package)
  ; internal procedure
  (define (make-record name salary)
    (list name salary))
  (define (get-name record)
    (car record))
  (define (get-salary record)
    (cadr record))
  (define (insert-record record file)
    (if (null? file)
        (cons record nil)
        ; For implementation convenience, name(id) would be unique interger in this system.
        (let ((now-name (get-name (car file)))
              (insert-name (get-name record))
              (next (cdr file)))
          (cond ((< now-name insert-name)
                 (cons (car file) (insert-record record next)))
                ((> now-name insert-name)
                 (cons record file))
                ; Throwing error can be a choice.
                ; (error "same name employee -- insert-record" record)
                (else file)))))
  (define (get-record name file)
    (if (null? file)
        ; or.. can choose throwing (error "no name in this file -- get-record" name).
        #f
        (let ((now (get-name (car file))))
          (cond ((= name now) (car file))
                ((< name now) #f)
                (else (get-record name (cdr file)))))))
  ; interface to the rest of the system
  ; 1. type-checking for inputs
  ; 2. append type tag for output
  (define (tag x) (attach-tag 'design x))
  (define (type-check x) (equal? (type-tag x) 'design))
  (put 'make-record 'design (lambda (n s) (tag (make-record n s))))
  (put 'insert-record 'design (lambda (r f)
                                (if (and (type-check r) (type-check f))
                                    (tag (insert-record (contents r) (contents f)))
                                    (error "invalid type -- expected design in this"))))
  (put 'get-record 'design (lambda (n f)
                             (if (type-check f)
                                 (tag (get-record n (contents f)))
                                 (error "invalid type -- expected design in this"))))
  (put 'get-salary 'design (lambda (r)
                             (if (type-check r)
                                 (get-salary (contents r))
                                 (error "invalid type -- expected design in this")))))


; Install.
(install-develop-division-package)
(install-design-division-package)


; ==================================================
; Make each division's file randomly.
(define (iter-dev n result)
  (if (= n 0)
      result
      (let ((make-record (get 'make-record 'develop))
            (insert-record (get 'insert-record 'develop))
            (random-name (random 10))
            (random-salary (+ 3000 (* (random 100) 100))))
        (iter-dev (- n 1)
                  (insert-record (make-record random-name random-salary)
                                 result)))))
(define develop-division-file (iter-dev 30 (attach-tag 'develop nil)))

(define (iter-des n result)
  (if (= n 0)
      result
      (let ((make-record (get 'make-record 'design))
            (insert-record (get 'insert-record 'design))
            (random-name (+ (random 10) 10))
            (random-salary (+ 3000 (* (random 100) 100))))
        (iter-des (- n 1)
                  (insert-record (make-record random-name random-salary)
                                 result)))))
(define design-division-file (iter-des 30 (attach-tag 'design nil)))

; Test.
develop-division-file
; (develop (5 9400) (7 8500) (1 9900) (9 7300) (3 11900)
;          (6 5100) (4 11400) (8 8700) (2 9400) (0 12600))
design-division-file
; (design (10 12800) (12 8000) (14 9100) (15 9000)
;         (16 6100) (17 5500) (18 11000) (19 3300))



; ==================================================
; Exercise 2.74.

; Insatiable Enterprises, Inc., is a highly decentralized conglomerate company consisting of a large
; number of independent divisions located all over the world. The company's computer facilities have
; just been interconnected by means of a clever network-interfacing scheme that makes the entire
; network appear to any user to be a single computer.

; Insatiable's president, in her first attempt to exploit the ability of the network to extract
; administrative information from division files, is dismayed to discover that, although all the
; division files have been implemented as data structures in Scheme, the particular data structure
; used varies from division to division.

; A meeting of division managers is hastily called to search for a strategy to integrate the files
; that will satisfy headquarters' needs while preserving the existing autonomy of the divisions.

; Show how such a strategy can be implemented with data-directed programming. As an example, suppose
; that each division's personnel records consist of a single file, which contains a set of records
; keyed on employees' names. The structure of the set varies from division to division.
; Furthermore, each employee's record is itself a set (structured differently from division
; to division) that contains information keyed under identifiers such as address and salary.




; ==================================================
; a.  Implement for headquarters a get-record procedure that retrieves a specified employee's record
;     from a specified personnel file. The procedure should be applicable to any division's file.
;     Explain how the individual divisions' files should be structured. In particular, what type
;     information must be supplied?

(define (get-record employee-name file)
  (let ((division (type-tag file)))
    ((get 'get-record division) employee-name file)))

; Test.
(define develop-employee-2-record
  (get-record 2 develop-division-file))
(define design-employee-15-record
  (get-record 15 design-division-file))

develop-employee-2-record
; (develop 2 9400)
design-employee-15-record
; (design 15 9000)




; ==================================================
; b.  Implement for headquarters a get-salary procedure that returns the salary information from a
;     given employee's record from any division's personnel file. How should the record be structured
;     in order to make this operation work?

(define (get-salary employee-record)
  (let ((division (type-tag employee-record)))
    ((get 'get-salary division) employee-record)))


; Test.
(get-salary develop-employee-2-record)
; 9400
(get-salary design-employee-15-record)
; 9000




; ==================================================
; c.  Implement for headquarters a find-employee-record procedure. This should search all the
;     divisions' files for the record of a given employee and return the record. Assume that this
;     procedure takes as arguments an employee's name and a list of all the divisions' files.

(define all-division-files (list develop-division-file
                                 design-division-file))

(define (find-employee-record employee-name file-list)
  (if (null? file-list)
      ; or... can choose throwing error.
      #f
      (let ((division (type-tag (car file-list)))
            (file (car file-list))
            (next-list (cdr file-list)))
        (let ((result ((get 'get-record division) employee-name file)))
          (if (pair? (contents result))
              result
              (find-employee-record employee-name next-list))))))


; Test.
(find-employee-record 2 all-division-files)
; (2 9400)
(find-employee-record 15 all-division-files)
; (design 15 9000)
(find-employee-record 23 all-division-files)
; #f


; ==================================================
; d.  When Insatiable takes over a new company, what changes must be made in order to incorporate
;     the new personnel information into the central system?

; It must be installed for new division's personnel information system:
; In particular, it must have insert-record, get-record, make-record, get-salary interfaces.
; And it's file or division must be dealed with proper type(division) tag when it is used in
; central system, to distinguish with other division's file or record.


; Test for not suitable type operation.
(define (wrong-get-record name file)
  (let ((division (type-tag file)))
    ((get 'get-record 'develop) name file)))

(wrong-get-record 2 develop-division-file)
; (develop 2 8300)
(wrong-get-record 15 design-division-file)
; invalid type -- expected develop in this
;   context...:
;    body of "~\SicpExercise\ch2-4\ex2-74.rkt"

