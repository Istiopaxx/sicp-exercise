#lang sicp


; Exercise 2.55.
; Eva Lu Ator types to the interpreter the expression

(car ''abracadabra)

; To her surprise, the interpreter prints back quote.
; ==================================================
; Explain.

; After ', the interpreter interprets all characters as itself.
; ' is quote, so it evaluate to quote.
