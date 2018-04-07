#lang racket

; Q4. Write a function sublsum in Racket that takes a list
;     (of integers) as input and returns a list of sublists
;     of it that sum to zero. For this problem, you can use
;     the combinations built-in function. Note the order
;     of the sublists and their elements doesn’t matter.
;     For example:
;     > (sublsum (list 1 2 3 4 -5))
;     '((2 3 -5) (-5 1 4))
;     > (sublsum (list 1 2 3 4 5))
;     '()

;     References: [1] - https://codereview.stackexchange.com/questions/87058/splitting-a-list-in-racket
;                 [2] - https://docs.racket-lang.org/reference/pairs.html#%28def._%28%28lib._racket%2Flist..rkt%29._combinations%29%29

; Function that takes list input and sums up the
; elements of that list together
(define (sum list)
  ; If list is not null or 0
  (if (null? list) 0
      ; Add first element of the list to the sum of the rest of the list
      (+ (car list) (sum (cdr list)))))

; Uncomment bellow to check if the sum fnction works correctly
;'(sum '(1 2 3 4 5))
;(sum '(1 2 3 4 5))
; Expected ouput: 15

; Function that takes list combination as an input and calculates the
; sum of that sub list under one condition using sum function defined before. 
(define (get-sum list)
  ; If list is empty
  (if (null? list)
      ; Output empty list
      '()
      ; If the sum of the first element of the combination is 0 and is not null
      (if (and (= (sum (car list)) 0) (not (null? (car list))))
          ; Construct the new list with first element and a recursively pass
          ; the rest of the elements of that combination list through the function
          (cons (car list) (get-sum (cdr list)))
          ; Else recursively pass the rest of the element of that combination
          ; list through the function
          (get-sum (cdr list)))))
          
; Function that takes list input and gets the sum of all
; possible combinations of that list
(define (sublsum list)
  ; If list is empty
  (if (null? list)
      ; Output empty list
      '()
      ; Else get the sum of all possible combinations
      (get-sum (combinations list))))

'(sublsum '(1 2 3 4 -5))
(sublsum '(1 2 3 4 -5))
; Expected output: '((2 3 -5) (1 4 -5))
'(sublsum '(1 2 3 4 5))
(sublsum '(1 2 3 4 5))
; Expected output: '()