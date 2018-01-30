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

(define (decide-prime x))
 

;  (filter (lambda (i) (= 0 (modulo x i))) (range 2 (floor (sqrt x)))))

;(decide-prime 1)