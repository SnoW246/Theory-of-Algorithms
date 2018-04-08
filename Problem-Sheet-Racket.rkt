#lang racket

; Module: Theory of Algorithms
; Problem Sheet: Racket
; Author: Adrian Golias

; The following exercises are related to the Racket programming language [3].

; References: [1] Project Euler. Project euler.
;             [2] Brian Harvey and Matt Wright.
;                 Simply scheme: Introducing computer science.
;             [3] PLT Inc. Racket – A programmable programming language.

; Q1. Re-write the following expressions in Racket and evaluate them
;     using a Racket interpreter/compiler.

; Q1.(a) (3 × (5 + (10 ÷ 5)))
'Q1.a: (*(+(/ 10 5) 5) 3)
; Q1.(b) (2 + 3 + 4 + 5)
'Q1.b: (+ 2 3 4 5)
; Q1.(c) (1 + (5 + (2 + (10 ÷ 3))))
'Q1.c: (+ 1 5 2 (/ 10 3))
; Q1.(d) (1 + (5 + (2 + (10 ÷ 3.0))))
'Q1.d: (+ 1 5 2 (/ 10 3.0))
; Q1.(e) (3 + 5) × (10 ÷ 2)
'Q1.e: (* (+ 3 5) (/ 10 2))
; Q1.(f) (3 + 5) × (10 ÷ 2) + (1 + (5 + (2 + (10 ÷ 3))))
'Q1.f: (+ (* (+ 3 5) (/ 10 2)) (+ 1 5 2 (/ 10 3)))

; Q2. Define a procedure discount that takes two arguments:
;     An item’s initial price and a percentage discount [2].
;     It should return the new price:

;     > (discount 10 5)
;     9.50
;     > (discount 29.90 50)
;     14.95

(define (discount x y)
  (- x (* x (/ y 100.0))))

'Q2:
(discount 10 5)
(discount 29.90 50)

; Q3. Define a function grcomdiv that takes two integers and
;     returns their greatest common divisor.
;     > (grcomdiv 10 15)
;     5
;     > (grcomdiv 64 30)
;     2

; https://rosettacode.org/wiki/Greatest_common_divisor
(define (grcomdiv x y )
  (if (= y 0)
      x
      (grcomdiv y (modulo x y))))

'Q3:
(grcomdiv 10 15)
(grcomdiv 64 30)

; Q4. Write a function called appearances that returns the
;     number of times its first argument appears as a
;     member of its second argument [2].

(define (appearances l m)
  (if (null? m)
      0
      (if (equal? l (car m))
          (+ 1 (appearances l (cdr m)))
          (appearances l (cdr m)))))

'Q4: 
'(appearances 2 (list 2 5 8 10 2 2 0 1 2))
(appearances 2 (list 2 5 8 10 2 2 0 1 2))

; Q5. Write a procedure inter that takes two lists as arguments.
;     It should return a list containing every element that
;     appears in both lists, exactly once.

(define (inter l m)
  (if (null? l)
      '()
      (if (> (appearances (car l) m) 0)
          (cons (car l) (inter (cdr l) m))
          (inter (cdr l) m))))

'Q5:
'(inter (list 1 2 3 4 5 6) (list 1 5 7 3 8 0))
(inter (list 1 2 3 4 5 6) (list 1 5 7 3 8 0))

; Q6. Write a procedure noatoms that takes a list and returns
;     the number of atoms it contains.

(define (noatoms l)
  (if (null? l)
      0
      (if (not (or (pair? (car l)) (null? (car l))))
          (+ 1 (noatoms (cdr l)))
          (noatoms (cdr l)))))

'Q6:
'(noatoms (list 1 2 3 4 5 6))
(noatoms (list 1 2 3 4 5 6))
'(noatoms (list 1 1 1))
(noatoms (list 1 1 1))

; Q7. Here is a Racket procedure that never finishes its
;     job when n is not 0:

;     (define (forever n)
;       (if (= n 0)
;            1
;            (+ 1 (forever n))))

;     Explain why it doesn’t give any result[2].

(define (forever n)
  (if (= n 0)
      1
      (+ 1 (forever n))))

'Q7:
'(forever 0)
(forever 0)
'(forever 1) (displayln "Will continue to execute until you run out of memory!")
;(forever 1)

(displayln "")
(displayln "If value passed to forefer function is equal to 0,
the function terminates with the output of 1. However, when number
passed into forever function is not equal to 0, it keeps calling itself
and continue to increase the value of n by 1, resulting in infinite loop.")

; Q8. Write a function called range that takes an integer n
;     and returns a list containing the atoms 1, 2, 3, . . . , n.

(define (range n)
  (if (= n 0)
      '()
      (append (range (- n 1)) (list n))))

'Q8:
'(range 3)
(range 3)
'(range 8)
(range 8)

; Q9. Write a function called reversel that takes a list
;     and returns it reversed.
(define (reversel l)
  (define (reversel-aux l a)
    (if (null? l)
        a
        (reversel-aux (cdr l) (cons (car l) a))))
  (reversel-aux l null))

'Q9:
'(reversel (list 1 2 3 4 5 6))
(reversel (list 1 2 3 4 5 6))

; Q10. If we list all the natural numbers below 10 that are
;      multiples of 3 or 5, we get 3, 5, 6 and 9. The sum of these
;      multiples is 23. Write a procedure to find the sum of all
;      the multiples of 3 or 5 below 1000 [1].

(define (multiples-of-3-and-5 n)
  (if (= 0 n)
      0
      (if (= 0 (modulo n 3))
          (+ n (multiples-of-3-and-5 (- n 1)))
          (if (= 0 (modulo n 5))
              (+ n (multiples-of-3-and-5 (- n 1)))
              (multiples-of-3-and-5 (- n 1))))))

'Q10:
'(multiples-of-3-and-5 999)
(multiples-of-3-and-5 999)

; Q11. Write a procedure called flatten that takes as its argument
;      a list, possibly including sublists, but whose ultimate
;      building blocks are atoms. It should return a sentence
;      containing all the atoms of the list, in the order in which
;      they appear in the original:

;      > (flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))
;      (a b c d e f g h i j k)

(define (flatten l)
  (if (null? l)
      '()
      (if (pair? (car l))
          (append (flatten (car l)) (flatten (cdr l)))
          (cons (car l) (flatten (cdr l))))))

'Q11:
'(flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))
(flatten '(((a b) c (d e)) (f g) ((((h))) (i j) k)))

; Q12. Each new term in the Fibonacci sequence is generated by
;      adding the previous two terms. By starting with 1 and 2,
;      the first 10 terms will be:

;      1, 2, 3, 5, 8, 13, 21, 34, 55, 89

;      By considering the terms in the Fibonacci sequence whose
;      values do not exceed four million, find the sum of the
;      even-valued terms [1].

(define (sumflt total)
  (define (aux n-2 n-1 maxval total)
    (if (> (+ n-2 n-1) maxval)
        total
        (if (= 0 (modulo (+ n-2 n-1) 2))
            (aux n-1 (+ n-2 n-1) maxval (+ total n-2 n-1))
            (aux n-1 (+ n-2 n-1) maxval total))))
  (aux 0 1 total 0))

'Q12:
'(sumflt 4000)
(sumflt 4000)

; Q13. Write a procedure to-binary that takes a decimal interger
;      and converts it into a list of 0’s and 1’s representing
;      the number in binary form. The least significant bit
;      should be on the right of the list.

;      > (to-binary 9)
;      1001
;      > (to-binary 23)
;      10111

(define (to-binary n)
  (if (= n 0)
      '()
      (append (to-binary (/ (- n (modulo n 2)) 2))
              (list (modulo n 2)))))

'Q13:
'(to-binary 9)
(to-binary 9)
'(to-binary 23)
(to-binary 23)

; Q14. Write a function select that takes two elements, a list and
;      a position in the list, and return the element of the list
;      in that position.

;      > (select (list 1 2 3 4 5) 1)
;      2

(define (select l x)
  (if (= 0 x)
      (car l)
      (select (cdr l) (- x 1))))

'Q14:
'(select (list 1 2 3 4 5) 1)
(select (list 1 2 3 4 5) 1)

; Q15. Write a function perms that takes a list as its only argument,
;      and returns a list containing all permutations of that list.

(define (perms l)
  (if (null? l)
      '(())
      (apply append
             (map (lambda (i)
                    (map (lambda (j)(cons i j))
                         (perms (remove i l))))
                  l))))
'Q15:
'(perms (list 1 2 3))
(perms (list 1 2 3))
'(perms (list 1 2 3 4))
(perms (list 1 2 3 4))