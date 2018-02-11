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

(define (collatz-list x))

(collatz-list 5)