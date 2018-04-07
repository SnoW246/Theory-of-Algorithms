#lang racket

; Q2. Write, from scratch, a function in Racket that takes a positive
;     integer n0 as input and returns a list by recursively applying
;     the following operation, starting with the input number.
;            { 3ni+1 }  if n1 is odd
;     ni+1 = {       }
;            {  ni÷2 }  otherwise
;     End the recursion when (or if) the number becomes 1. Call the
;     function collatz-list. So, collatz-list should return a list
;     whose first element is n0, the second element is n1, and so on.
;     For example:
;     > (collatz-list 5)
;     '(5 16 8 4 2 1)
;     > (collatz-list 9)
;     '(9 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
;     > (collatz-list 2)
;     '(2 1)

; References: [1] Collatz Conjecture -
;                 https://en.wikipedia.org/wiki/Collatz_conjecture

(define (collatz-list x)
  (cond
    ; When number x is equal to 1 stop the recursion 
    ((= x 1) '(1))
    ; When number x is an even number...
    ((= (modulo x 2) 0)
     ; Add result of x/2 to the list
     (cons x (collatz-list (/ x 2))))
    ; When number x is an odd number...
    ((= (modulo x 2) 1)
     ; Add result of x*3+1 to the list 
     (cons x (collatz-list (+ (* 3 x) 1))))))

'Collatz-List==>x=1
(collatz-list 1)
; Expected output: (1)
'Collatz-List==>x=2
(collatz-list 2)
; Expected output: (2 1)
'Collatz-List==>x=5
(collatz-list 5)
; Expected output: (5 16 8 4 2 1)
'Collatz-List==>x=6
(collatz-list 6)
; Expected output: (6 3 10 5 16 8 4 2 1)
'Collatz-List==>x=9
(collatz-list 9)
; Expected output: (9 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
'Collatz-List==>x=24
(collatz-list 24)
; Expected output: (24 12 6 3 10 5 16 8 4 2 1)
'Collatz-List==>x=1992
(collatz-list 1992)
; Expected output: (1992 996 498 249 748 374 187 562 281 844 422 211 634
;                    317 952 476 238 119 358 179 538 269 808 404 202 101
;                    304 152  76  38  19  58  29  88  44  22  11  34  17
;                     52  26  13  40  20  10   5  16   8   4   2   1)
