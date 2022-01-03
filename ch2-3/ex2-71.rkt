#lang sicp


; Exercise 2.71.

; Suppose we have a Huffman tree for an alphabet of n symbols, and that the relative frequencies of
; the symbols are 1, 2, 4, ..., 2^n-1. Sketch the tree for n=5; for n=10.

; In such a tree (for general n) how many bits are required to encode the most frequent symbol and
; the least frequent symbol?


; ==================================================
; Assume that there are n symbols that is numbered by frequency,
; 1 is most frequent symbol, 2 is next, 3 is next.... N is least frequent symbol.
; Result huffman Tree would be like below:

;          root
;         ┌──┴──┐
;         n     │
;              ┌┴──┐
;             n-1  │
;                 ┌┴──┐
;                n-2  │
;                    ...
;                    ...
;                     ┌┴──┐
;                     2   1

; Most frequent symbol  : 1   bit  => (0)
; Least frequent symbol : n-1 bits => (1111....1111)

