#lang racket

; Q9. Write a function sod2 in Racket that takes three
;     lists x, y and z of equal length and containing
;     only 0’s and 1’s. It should return a list containing
;     a 1 where the number of 1’s in a given position in
;     x, y and z contains an odd nubmer of 1’s, and 0 otherwise.
;     For example:
;     > (sod2 (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
;     '(0 1 1 0 1 0 0 1)

;     References: [1] - https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29

; Function that takes three lists as an input, maps every
; element of each list to its corresponding element every
; other list, and itterate through to check occurances
(define (sod2 x y z)
  ; Itterate through the lambda map list created
  (itterate (map (lambda (x y z) (+ x y z)) x y z)))

; Function that takes a list as an input
(define (itterate list)
  ; If list is empty
  (if (null? list)
      ; Output empty list
      '()
      ; If the modulus of the first element is qual to 1 then it
      ; occurs odd number of times
      (if (= 1 (modulo (car list) 2))
          ; If modulus is odd construct a list with 1 and
          ; itterate through remaining elements of that list
          (cons 1 (itterate (cdr list)))
          ; Else modulus is even :. construct a list with 0 instead
          ; and itterate through remaining elements of that list
          (cons 0 (itterate (cdr list))))))

'(sod2 (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
(sod2 (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
; Expected output: '(0 1 1 0 1 0 0 1)

'(sod2 (list 1 0 1 1 0 0 1 1 1 1) (list 1 1 0 1 0 0 1 0 1 1) (list 1 1 1 1 0 0 1 1 0 1))
(sod2 (list 1 0 1 1 0 0 1 1 1 1) (list 1 1 0 1 0 0 1 0 1 1) (list 1 1 1 1 0 0 1 1 0 1))
; Expected output: '(1 0 0 1 0 0 1 0 0 1)