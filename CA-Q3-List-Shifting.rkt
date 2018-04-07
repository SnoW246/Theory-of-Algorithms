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

;     References: [1] 
;     https://docs.racket-lang.org/guide/Lists__Iteration__and_Recursion.html

(define (lcycle list)
; Takes list input
; returns list shifted one place to the left
  (if (null? list)
      '()
      (append (cdr list)
              (cons (car list)
                    '()))))

(lcycle '(1 2 3 4 5))


;(define rcicle)