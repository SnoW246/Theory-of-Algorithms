#lang racket

; Module: Theory of Algorithms
; Problem Sheet: CA - Prime Number (2018)
; Author: Adrian Golias

; CA Q1. Write, from scratch, a function in Racket that uses
;        a brute-force algorithm that takes a single positive
;        integer and return true if the number is a prime and
;        false otherwise. Call the function decide-prime.

; Prime Number: A number that is divisible only by itself and 1. 

; References: [1] PLT Inc. Racket – A programmable programming language.
;             [2] Output was checked against Prime Number List available
;                 here: https://brilliant.org/wiki/prime-numbers/

(define (decide-prime p)
  ; If number is less than 2 it cannot be a prime number
  ; & negative numbers cannot be prime numbers
  (if (< p 2)
      #f
      (recursion p 2)))

(define (recursion n m)
  ; Brute Force 
  (if (= m n)
      #t
      (if (= (modulo n m) 0)
          #f
          (recursion n (+ m 1)))))

'1: (decide-prime 1) ;#f
'2: (decide-prime 2) ;#t
'3: (decide-prime 3) ;#t
'4: (decide-prime 4) ;#f
'5: (decide-prime 5) ;#t
'6: (decide-prime 6) ;#f
'7: (decide-prime 7) ;#t
'8: (decide-prime 8) ;#f
'9: (decide-prime 9) ;#f
'10: (decide-prime 10) ;#f
'11: (decide-prime 11) ;#t
'17: (decide-prime 17) ;#t
'55: (decide-prime 55) ;#f
'113: (decide-prime 113) ;#t
'141: (decide-prime 141) ;#f
'179827: (decide-prime 179827) ;#t

;===============================================================================

; Q2. Write, from scratch, a function in Racket that takes a positive
;     integer n0 as input and returns a list by recursively applying
;     the following operation, starting with the input number.
;            { 3ni+1 }  if n1 is odd
;     ni+1 = {       }
;            {  ni÷2 }  otherwise
;     End the recursion when (or if) the number becomes 1. Call the
;     function collatz-list. So, collatz-list should return a list
;     whose first element is n0, the second element is n1, and so on.
;     For example:
;     > (collatz-list 5)
;     '(5 16 8 4 2 1)
;     > (collatz-list 9)
;     '(9 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
;     > (collatz-list 2)
;     '(2 1)

; References: [1] Collatz Conjecture -
;                 https://en.wikipedia.org/wiki/Collatz_conjecture

(define (collatz-list x)
  (cond
    ; When number x is equal to 1 stop the recursion 
    ((= x 1) '(1))
    ; When number x is an even number...
    ((= (modulo x 2) 0)
     ; Add result of x/2 to the list
     (cons x (collatz-list (/ x 2))))
    ; When number x is an odd number...
    ((= (modulo x 2) 1)
     ; Add result of x*3+1 to the list 
     (cons x (collatz-list (+ (* 3 x) 1))))))

'Collatz-List==>x=1
(collatz-list 1)
; Expected output: (1)
'Collatz-List==>x=2
(collatz-list 2)
; Expected output: (2 1)
'Collatz-List==>x=5
(collatz-list 5)
; Expected output: (5 16 8 4 2 1)
'Collatz-List==>x=6
(collatz-list 6)
; Expected output: (6 3 10 5 16 8 4 2 1)
'Collatz-List==>x=9
(collatz-list 9)
; Expected output: (9 28 14 7 22 11 34 17 52 26 13 40 20 10 5 16 8 4 2 1)
'Collatz-List==>x=24
(collatz-list 24)
; Expected output: (24 12 6 3 10 5 16 8 4 2 1)
'Collatz-List==>x=1992
(collatz-list 1992)
; Expected output: (1992 996 498 249 748 374 187 562 281 844 422 211 634
;                    317 952 476 238 119 358 179 538 269 808 404 202 101
;                    304 152  76  38  19  58  29  88  44  22  11  34  17
;                     52  26  13  40  20  10   5  16   8   4   2   1)

;===============================================================================

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

;===============================================================================

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

;===============================================================================

; Q5. Write a function hamming-weight in Racket that takes
;     a list l as input and returns the number of non-zero
;     elements in it.
;     For example:
;     > (hamming-weight (list 1 0 1 0 1 1 1 0))
;     5

;     References: [1] - https://stackoverflow.com/questions/32854125/racket-how-to-count-the-number-of-elements-in-a-list
;                 [2] - https://stackoverflow.com/questions/36595710/counting-non-zero-values-from-a-list-with-scheme

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

;===============================================================================

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

;===============================================================================

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

;===============================================================================

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

;===============================================================================

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
  (itterate2 (map (lambda (x y z) (+ x y z)) x y z)))

; Function that takes a list as an input
(define (itterate2 list)
  ; If list is empty
  (if (null? list)
      ; Output empty list
      '()
      ; If the modulus of the first element is qual to 1 then it
      ; occurs odd number of times
      (if (= 1 (modulo (car list) 2))
          ; If modulus is odd construct a list with 1 and
          ; itterate through remaining elements of that list
          (cons 1 (itterate2 (cdr list)))
          ; Else modulus is even :. construct a list with 0 instead
          ; and itterate through remaining elements of that list
          (cons 0 (itterate2 (cdr list))))))

'(sod2 (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
(sod2 (list 0 0 0 0 1 1 1 1) (list 0 0 1 1 0 0 1 1) (list 0 1 0 1 0 1 0 1))
; Expected output: '(0 1 1 0 1 0 0 1)

'(sod2 (list 1 0 1 1 0 0 1 1 1 1) (list 1 1 0 1 0 0 1 0 1 1) (list 1 1 1 1 0 0 1 1 0 1))
(sod2 (list 1 0 1 1 0 0 1 1 1 1) (list 1 1 0 1 0 0 1 0 1 1) (list 1 1 1 1 0 0 1 1 0 1))
; Expected output: '(1 0 0 1 0 0 1 0 0 1)

;===============================================================================

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
  ; If list is empty
  (if (null? l)
      ; Output 0 /do nothing
      0
      ; Else run the list through another function to square
      ; the corresponding elements & recursively run through
      ; remaining element of that list while adding the output
      ; value returned by the square function
      ;(+ (lstq-squared (- (car l) (car m))) (lstq (cdr l) (cdr m)))))

      ; Else map every element of each list to its corresponding
      ; element ((E li) = (E mi)) and subtract them from each other in
      ; the process ((E li) - (E mi)) which creates a new single list
      ; with mapped & subtracted values in it. Run that list through
      ; external function to square the first element of that list,
      ; and recursively pass the remaining elements of that list through
      ; the square function while adding the adding the output
      ; value returned by the square function
      (+ (lstq-squared (car (map (lambda (l m) (- l m)) l m))) (lstq (cdr l) (cdr m)))))
  
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

;===============================================================================