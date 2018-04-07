#lang racket

; Q6. Write a function hamming-distance in Racket that takes
;     two lists and returns the number of positions in which
;     they differ.
;     For example:
;     > (hamming-distance (list 1 0 1 0 1 1 1 0) (list 1 1 1 1 0 0 0 0))
;     5

;     References: [1] - https://stackoverflow.com/questions/33427777/comparing-two-lists-in-scheme
;                 [2] - https://www.student.cs.uwaterloo.ca/~cs135/handouts/06-recursion-post.pdf

; Count function to count the number of elemebts in the input list
(define (count l)
  ; If the list is empty
  (if (null? l)
      ; Return 0 / Do nothing
      0
      ; Else add one to the count and recursively run the remaining
      ; elements of the list through the function 
      (+ 1 (count (cdr l)))))

; Compare function that compares two lists against each other
(define (compare l m)
  ; If list is empty
  (if (null? l)
      ; Output 0/ do nothing
      0
      ; If first element of both lists are equal, it's a match!
      (if (= (car l) (car m))
          ; Else recursively run the remaining elements of
          ; both lists through the function
          (compare (cdr l) (cdr m))
          ; Add one to the counter when match was
          ; not found between the lists
          (+ 1 (compare (cdr l) (cdr m))))))

; Function that takes two input lists and returns the
; number of positions in which they differ
(define (hamming-distance l m)
  ; If the number of elements of list l are qual to number
  ; of elements of list m, there is a possibility
  ; they might be the same :. allow to check
  (if (= (count l) (count m))
      ; Run the lists throught compare function defined previously
      (compare (cdr l) (cdr m))
      ; Else display error message of invalid input
      (displayln "       Invalid Input!!! The length of each list differ in size!
       Lists must have the same amount of elements to be compared!")))

'(hamming-distance (list 1 0 1 0 1 1 1 0) (list 1 1 1 1 0 0 0 0))
(hamming-distance (list 1 0 1 0 1 1 1 0) (list 1 1 1 1 0 0 0 0))
;Expected output: 5

'(hamming-distance (list 1 0 1 0 1 1 1 0) (list 1 1 1 1 ))
(hamming-distance (list 1 0 1 0 1 1 1 0) (list 1 1 1 1 ))
; Expected output:        Invalid Input!!! The length of each list differ in size!
;                         Lists must have the same amount of elements to be compared!

'(hamming-distance (list 1 0 1 0 1 1 1 0 2 3 4 0 6) (list 1 1 1 1 0 0 0 0 2 3 0 1 6))
(hamming-distance (list 1 0 1 0 1 1 1 0 2 3 4 0 6) (list 1 1 1 1 0 0 0 0 2 3 0 1 6))
; Expected output: 7