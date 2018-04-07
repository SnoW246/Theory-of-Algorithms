#lang racket

; Q5. Write a function hamming-weight in Racket that takes
;     a list l as input and returns the number of non-zero
;     elements in it.
;     For example:
;     > (hamming-weight (list 1 0 1 0 1 1 1 0))
;     5

;     References: [1] - https://stackoverflow.com/questions/32854125/racket-how-to-count-the-number-of-elements-in-a-list

; Function that take list input and returns the number
; of non-zero elements of that list
(define (hamming-weight l)
  ; Lambda expression to create function in its simplest case,
  ; to count every element/ number of the list that is not equal to 0
  (count (lambda (n) (not (zero? n)))
         l))

; Function that take list input and returns the number
; of non-zero elements of that list
(define (hamming-weight2 l)
  ; If the list is empty
  (if (null? l)
      ; Do nothing
      0
      ; If the first element of the list is qual to 0
      (if (= (car l) 0)
          ; Recursively loop through remaining elements of the list
          (hamming-weight2 (cdr l))
          ; Add 1 to the counter if the element is not equal to 0
          (+ 1 (hamming-weight (cdr l))))))

; Function that take list input and returns the number
; of non-zero elements of that list
(define (hamming-weight3 l)
  ; Condition, if the list is empty return 0
  (cond ((null? l) 0)
        ; If the current element is not equal to 0
        ((not (= (car l) 0))
         ; Add 1 to the counter and recursively run remaining 
         ; elements of that list through the function
         (+ 1 (hamming-weight3 (cdr l))))
        ; Else...
        (else
         ;Skip to the next element & recursively run remaining
         ; elements of that list through the function
         (hamming-weight3 (cdr l)))))
'(hamming-weight (list 1 0 1 0 1 1 1 0))
(hamming-weight (list 1 0 1 0 1 1 1 0))
;Expected output: 5
'(hamming-weight2 (list 1 0 1 0 1 1 1 0))
(hamming-weight2 (list 1 0 1 0 1 1 1 0))
;Expected output: 5
'(hamming-weight3 (list 1 0 1 0 1 1 1 0))
(hamming-weight3 (list 1 0 1 0 1 1 1 0))
;Expected output: 5
'(hamming-weight (list 0 0 0 0 0 1 1 1))
(hamming-weight (list 0 0 0 0 0 1 1 1))
'(hamming-weight2 (list 1 0 1 0 1 0 1 0))
(hamming-weight2 (list 1 0 1 0 1 0 1 0))
'(hamming-weight3 (list 1 1 1 0 0 1 1 1))
(hamming-weight3 (list 1 1 1 0 0 1 1 1))