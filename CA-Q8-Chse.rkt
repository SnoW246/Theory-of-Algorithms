#lang racket

; Q8. Write a function chse in Racket that takes three lists
;     x, y and z of equal length and containing only 0’s and 1’s.
;     It should return a list containing the elements of y in
;     the positions where x is 1 and the elements of z otherwise.
;     For example:
;     > (chse (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
;     '(0 1 0 1 0 0 1 1)

;     References: [1] - https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Fprivate%2Fmap..rkt%29._map%29%29

; Function that takes 3 lists as an input and returns another
; list containing elements from lists x, y or z based on the
; circumstances described in the question above
(define (chse x y z)
  ; Mapping each element of every list to it's
  ; corresponding element in every other list,
  ; itterating through the lists
  (map (lambda (x y z)
         ; If the element of x is equal to 1,
         ; replace the element of list x with the corresponding
         ; element from list y, else replace the element of list
         ; x with the corresponding element from list z
         (if (= x 1) y z))
       ; Output new list
       x y z))

'(chse (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
(chse (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
; Expected output: '(0 1 0 1 0 0 1 1)

'(chse (list 1 0 1 0 1 0 1 0) (list 1 0 1 0 0 1 0 1) (list 0 1 0 1 0 0 0 0))
(chse (list 1 0 1 0 1 0 1 0) (list 1 0 1 0 0 1 0 1) (list 0 1 0 1 0 0 0 0))
; Expected output: '(1 1 1 1 0 0 0 0)