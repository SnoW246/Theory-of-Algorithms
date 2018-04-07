#lang racket

; Module: Theory of Algorithms
; Problem Sheet: CA - Prime Number (2018)
; Author: Adrian Golias

; CA Q1. Write, from scratch, a function in Racket that uses
;        a brute-force algorithm that takes a single positive
;        integer and return true if the number is a prime and
;        false otherwise. Call the function decide-prime.

; Prime Number: A number that is divisible only by itself and 1. 

; References: [1] PLT Inc. Racket – A programmable programming language.
;             [2] Output was checked against Prime Number List available
;                 here: https://brilliant.org/wiki/prime-numbers/

(define (decide-prime p)
  ; If number is less than 2 it cannot be a prime number
  ; & negative numbers cannot be prime numbers
  (if (< p 2)
      #f
      (recursion p 2)))

(define (recursion n m)
  ; Brute Force 
  (if (= m n)
      #t
      (if (= (modulo n m) 0)
          #f
          (recursion n (+ m 1)))))

'1: (decide-prime 1) ;#f
'2: (decide-prime 2) ;#t
'3: (decide-prime 3) ;#t
'4: (decide-prime 4) ;#f
'5: (decide-prime 5) ;#t
'6: (decide-prime 6) ;#f
'7: (decide-prime 7) ;#t
'8: (decide-prime 8) ;#f
'9: (decide-prime 9) ;#f
'10: (decide-prime 10) ;#f
'11: (decide-prime 11) ;#t
'17: (decide-prime 17) ;#t
'55: (decide-prime 55) ;#f
'113: (decide-prime 113) ;#t
'141: (decide-prime 141) ;#f
'179827: (decide-prime 179827) ;#t