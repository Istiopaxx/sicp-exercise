#lang sicp


; Exercise 2.76.

; As a large system with generic operations evolves, new types of data objects or new operations may
; be needed. For each of the three strategies -- generic operations with explicit dispatch,
; data-directed style, and message-passing-style -- describe the changes that must be made to a
; system in order to add new types or new operations. Which organization would be most appropriate
; for a system in which new types must often be added? Which would be most appropriate for a system
; in which new operations must often be added?


; o                            Types
; p                   polar      │    rectangular
; e            ┌─────────────────┼───────────────────────
; r  real-part │ real-part-polar │ real-part-rectangular
; a  imag-part │ imag-part-polar │ imag-part-rectangular
; t  magnitude │ magnitude-polar │ magnitude-rectangular
; i     angle  │ angle-polar     │ angle-rectangular  
; o
; n
; s


; 1. Generic operations with Explicit dispatch    ->  grouping types for operations. Row Grouping.
; 2. data-directed style / message-passing-style  ->  grouping operations for types. Column Grouping.

; ==================================================
; New types added case: 

; It would be good to use generic operations with explicit dispatch.

; ==================================================
; New operations added case:

; It would be good to use data-directed or message-passing style.

