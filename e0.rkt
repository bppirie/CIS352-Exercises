#lang racket

; Bradley Pirie
; 12FEB2021

;; Exercise 0: complete two simple functions

(provide implies-value
         point-distance)


; Compute the truth value of the proposition "x --> y" where x and y are booleans
(define (implies-value x y)
  (if (equal? x #t)   ;Check if x is t
      (if (equal? y #t) #t #f)   ;If x is t, check if y is t. If it is return t, if not return f
      #t))   ;If x is f, return t


; Compute the distance between two (x,y) pairs of integers

(define (pwr a)
  (* a a))   ;Function to square a value

(define (point-distance x0 y0 x1 y1)
  (sqrt (+ (pwr (- x1 x0)) (pwr (- y1 y0)))))   ;Write distance between two points formula in prefix notation
