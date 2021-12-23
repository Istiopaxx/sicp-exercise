#lang sicp
(#%require sicp-pict)


; Exercise 2.49.
; Use segments->painter to define the following primitive painters:


(define (bl f) (frame-origin f))
(define (br f) (vector-add (frame-origin f) (frame-edge1 f)))
(define (tl f) (vector-add (frame-origin f) (frame-edge2 f)))
(define (tr f) (vector-add (frame-origin f) (vector-add (frame-edge1 f) (frame-edge2 f))))

; ==================================================
; a.  The painter that draws the outline of the designated frame.

(define (outline f)
  (let ((bl (bl f))
        (br (br f))
        (tl (tl f))
        (tr (tr f)))
    (segments->painter (list (make-segment bl br)
                             (make-segment bl tl)
                             (make-segment br tr)
                             (make-segment tl tr)))))

; ==================================================
; b.  The painter that draws an 'X' by connecting opposite corners of the frame.

(define (draw-X f)
  (let ((bl (bl f))
        (br (br f))
        (tl (tl f))
        (tr (tr f)))
    (segments->painter (list (make-segment bl tr)
                             (make-segment br tl)))))


; ==================================================
; c.  The painter that draws a diamond shape by connecting the midpoints of the sides of the frame.

(define (draw-diamond f)
  (let ((bm (vector-scale (/ 1 2.0) (vector-add (bl f) (br f))))
        (tm (vector-scale (/ 1 2.0) (vector-add (tl f) (tr f))))
        (lm (vector-scale (/ 1 2.0) (vector-add (bl f) (tl f))))
        (rm (vector-scale (/ 1 2.0) (vector-add (br f) (tr f)))))
    (segments->painter (list (make-segment bm rm)
                             (make-segment rm tm)
                             (make-segment tm lm)
                             (make-segment lm bm)))))

; ==================================================
; d.  The wave painter.

; --> do yourself


; ==================================================
; Test.

(define full-frame
  (let ((origin (make-vect 0.0 0.0))
        (edge1 (make-vect 1.0 0.0))
        (edge2 (make-vect 0.0 1.0)))
    (make-frame origin edge1 edge2)))

(paint (outline full-frame))

(paint (draw-X full-frame))

(paint (draw-diamond full-frame))
