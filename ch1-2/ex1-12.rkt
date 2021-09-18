#lang sicp

;; 파스칼의 세모꼴 수를 만드는 프로시저를 되도는 프로세스로 짜보자.

(define (pascalSemo row col)
  (cond ((= col 1) 1)
        ((= col row) 1)
        ((+ (pascalSemo (- row 1) (- col 1))
           (pascalSemo (- row 1) col)))))

;;             1
;;            1 1
;;           1 2 1
;;          1 3 3 1
;;         1 4 6 4 1
;;            ...

(pascalSemo 3 2) ;; 2
(pascalSemo 4 3) ;; 3
(pascalSemo 5 3) ;; 6