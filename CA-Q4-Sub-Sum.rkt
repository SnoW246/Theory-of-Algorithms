#lang racket

; Q3. Write a function sublsum in Racket that takes a list
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

; Function that takes list input and sums up the
; elements of that list together
(define (get-sum list)
  ; If list is not null or 0
  (if (null? list) 0
      ; Add first element of the list to the sum of the rest of the list
      (+ (car list) (get-sum (cdr list)))))

'(get-sum '(1 2 3 4 5))
(get-sum '(1 2 3 4 5))
; Expected ouput: 15

