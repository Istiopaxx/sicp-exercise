#lang sicp

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

; (gcd 206 40)
; (if (= 40 0) ...)

; (gcd 40 (remainder 206 40))
; (if (= (remainder 206 40) 0) ...)
; (if (= 6 0) ...)

; (gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
; (if (= (remainder 40 (remainder 206 40)) 0) ...)
; (if (= 4 0) ...)

; (gcd (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
; (if (= (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) 0) ...)
; (if (= 2 0) ...)

; (gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40))) (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
; (if (= (remainder (remainder 40 (remainder 206 40)) (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))) 0) ...)
; (if (= 0 0) ...)

; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))

; 정의대로 계산법은 기본 프로시저가 남을때까지 식을 펼친 후, 기본 프로시저를 연산하여 다시 위로 올라간다.
; 이때 if는 기본 프로시저이므로, 펼침은 if에서 끝난다. (= b 0)을 계산하기 위해 b를 평가해야 한다.
; 따라서 (if (= b 0) ...)에서 b 자리에 (remainder a b)가 들어가게 되면 계산하게 된다.
; if 에서 평가하는데 1단계에서 1번, 2단계에서 2번, 3단계에서 4번, 4단계에서 7번 쓴다.
; 각 계산단계에서 (= b 0)을 만족하면 a를 평가하고, 만족하지 않으면 (gcd (...) (...))를 다시 부른다.
; 이때 정의대로 계산법이므로 인자의 (remainder (...) (...))는 평가하지 않는다.
; 4번째 단계에서 if프로시저의 (= b 0)을 만족했으므로 a를 평가한다. 이때 a는 
; (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))이고, a를 계산하기 위해 remainder가 
; 4번 쓰인다. 따라서 총 쓰인 횟수는 1+2+4+7+4 = 18번이다.



; (gcd 206 40)
; (gcd 40 (remainder 206 40))
; (gcd 40 6)
; (gcd 6 (remainder 40 6))
; (gcd 6 4)
; (gcd 4 (remainder 6 4))
; (gcd 4 2)
; (gcd 2 (remainder 4 2))
; (gcd 2 0)

; 인자 먼저 계산법에서는 인자를 먼저 계산하는데, 따라서 remainder는 4번만 계산된다.