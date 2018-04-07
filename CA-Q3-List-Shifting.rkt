#lang racket

; Q3. Write, from scratch, two functions in Racket. The first is called
;     lcycle. It takes a list as input and returns the list cyclically
;     shifted one place to the left. The second is called rcycle, and
;     it shifts the list cyclically shifted one place to the right.
;     For example:
;     > (lcycle (list 1 2 3 4 5))
;     '(2 3 4 5 1)
;     > (rcycle (list 1 2 3 4 5))
;     '(5 1 2 3 4)

;     References: [1] - https://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html
;                 [2] - https://stackoverflow.com/questions/13046017/rotate-a-list-to-the-left-in-scheme-racket
;                 [3] - https://stackoverflow.com/questions/8092878/racket-output-content-of-a-list

; Function that takes list input and shifts it to the left
(define (lcycle list)
; returns list shifted one place to the left
  ; If list is empty
  (if (null? list)
      ; Prints the empty list
      '()
      ; Else, append the the list with first element excluded
      (append (cdr list)
              ; Append the list with the fist element of the original list
              ; which adds it to the end of the list
              (cons (car list)
                    '()))))

'(lcycle '(1))
(lcycle '(1))
; Expected output: '(1)
'(lcycle '(1 2))
(lcycle '(1 2))
; Expected output: '(2 1)
'(lcycle '(1 2 3))
(lcycle '(1 2 3))
; Expected output: '(2 3 1)
'(lcycle '(1 2 3 4))
(lcycle '(1 2 3 4))
; Expected output: '(2 3 4 1)
'(lcycle '(1 2 3 4 5))
(lcycle '(1 2 3 4 5))
; Expected output: '(2 3 4 5 1)
'(lcycle '(5 4 3 2 1))
(lcycle '(5 4 3 2 1))
; Expected output: '(4 3 2 1 5)

; Function that takes list input and outputs another list,
; that excludes the last element of the input list
(define (all-but-last list)
  ; If list is empty
  (if (null? (cdr list))
      ; Output empty list
      '()
      ; Else, construct a new list consisting of the first
      ; element of the input list
      (cons (car list)
            ; Keep adding elements to that list until the last
            ; element is encountered & excluded
            (all-but-last (cdr list)))))

; Function that takes list input and outputs the last element of that list 
(define (last-element list)
  ; If the rest of the list is not equal to the first of the list
  ; meaning it's the exact same element
  (cond ((null? (cdr list)) (car list))
        ; Exclude the first element and loop through the rest of
        ; the list again until last emelent is found
        (else (last-element (cdr list)))))

; Function that takes list input and shifts it to the right
(define (rcycle list)
  ; Construct a new list and pass the input list to last-element function
  ; that returns the lest element which will become the very first
  ; element in this new list 
  (cons (last-element list)
        ; Passing the input list into all-but-last function that returns
        ; the list without the last element. This list is added to the
        ; end of newly constructed list
        (all-but-last list)))

'(rcycle '(1))
(rcycle '(1))
; Expected output: '(1)
'(rcycle '(1 2))
(rcycle '(1 2))
; Expected output: '(2 1)
'(rcycle '(1 2 3))
(rcycle '(1 2 3))
; Expected output: '(3 1 2)
'(rcycle '(1 2 3 4))
(rcycle '(1 2 3 4))
; Expected output: '(4 1 2 3)
'(rcycle '(1 2 3 4 5))
(rcycle '(1 2 3 4 5))
; Expected output: '(5 1 2 3 4)
'(rcycle '(5 4 3 2 1))
(rcycle '(5 4 3 2 1))
; Expected output: '(1 5 4 3 2)