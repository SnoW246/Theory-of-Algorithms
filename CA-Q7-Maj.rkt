#lang racket

; Q7. Write a function maj in Racket that takes three
;     lists x, y and z of equal length and containing
;     only 0’s and 1’s. It should return a list
;     containing a 1 where two or more of x, y and z
;     contain 1’s, and 0 otherwise.
;     For example:
;     > (maj (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
;     '(0 0 0 1 0 1 1 1)

;     References: [1] - https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29

; Function that itterates through the lists and generates an
; output list based on comparisson
(define (itterate list)
  ; If list is empty
  (if (null? list)
      ; Output empty list
      '()
      ; If the output is greater than 1, than element occurs more than once
      (if (> (car list) 1)
          ; Recursively run remaining elements of the list through the
          ; function and generate 1's when element occurs more than once
          (cons 1 (itterate (cdr list)))
          ; Recursively run remaining elements of the list through the
          ; function and generate 0's when element occurs less than or just once
          (cons 0 (itterate (cdr list))))))

; Function that takes three lists as an input and maps elements
; of each list to corresponding elements, checks how many times the
; number/element occurs throughout the map and outputs a list 
(define (maj x y z)
  ; Pass the mapped list to itterate function defined previously
  ; The output of map function can be checked with the following line of code
  ;(map (lambda (x y z) (+ x y z)) x y z))
  ; Expected output: '(0 1 1 2 1 2 2 3)
  (itterate (map (lambda (x y z) (+ x y z)) x y z)))

'(maj (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
(maj (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
; Expected output: '(0 0 0 1 0 1 1 1)

'(maj (list 1 0 1 0 1 0 1 0) (list 0 1 1 0 1 1 0 1) (list 1 1 0 0 1 1 0 0))
(maj (list 1 0 1 0 1 0 1 0) (list 0 1 1 0 1 1 0 1) (list 1 1 0 0 1 1 0 0))
; Expected output: (1 1 1 0 1 1 0 0)