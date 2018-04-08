#lang racket

; Q10. Write a function lstq in Racket that takes as
;      arguments two lists l and m of equal length and
;      containing numbers. It should return d, the
;      distance given by the sum of the square residuals
;      between the numbers in the lists.
;      This means take the ith element of m from the ith
;      element of l and square the result for all i.
;      Then add all of those to get d.
;      For example:
;      > (lstq (list 4.5 5.1 6.2 7.8) (list 1.1 -0.1 6.1 3.8))
;      54.61

;      References [1] - https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._combinations%29%29

; Function to take two lists as input and output the single
; distance value given by the sum of the square residuals
; between the numbers in the lists
(define (lstq l m)
  (if (null? l)
      0
      ;(+ (lstq-squared (- (car l) (car m))) (lstq (cdr l) (cdr m)))))
      (+ (lstq-squared (car (map (lambda (l m) (- l m)) l m))) (lstq (cdr l) (cdr m)))))
  
  ; If list is empty
  ;(if (null? l)
      ; Output 0 /do nothing
      ;0
      ; Else runt the list through another function to square
      ; the corresponding elements & recursively run through
      ; remaining element of that list while adding the output
      ; value returned by the square function
      ;(+ (lstq-squared l m) (lstq (cdr l) (cdr m))))

; Function that takes two lists as input and outputs the square
; of the first elements of those lists
(define (lstq-squared n)
  (* n n))

'(lstq (list 4.5 5.1 6.2 7.8) (list 1.1 -0.1 6.1 3.8))
(lstq (list 4.5 5.1 6.2 7.8) (list 1.1 -0.1 6.1 3.8))
; Expected output: 54.61

'(lstq (list 1 1.1 1.2 1.3) (list 1.0 -0.1 1.9 1.7))
(lstq (list 1 -7.7 -10 -3.4) (list 11.0 2.3 10 6.6))
; Expected output: 700.0

'(lstq (list -5 -7.7 6 -9) (list 5 2.3 -4.0 1.0))
(lstq (list -5 -7.7 6 -9) (list 5 2.3 -4.0 1.0))
; Expected output: 400.0