#lang racket

;; Bradley Pirie
;; 18FEB2021

;;; Exercises 1: a set of functions, each described in comments below
(provide zip
          unzip
          cars
          cdrs)

; HINT: If you have trouble inplement some functinos
; Please refer to https://docs.racket-lang.org/guide/Pairs__Lists__and_Racket_Syntax.html

; Zip: Given two lists, extract each first element and put into a pair store at the same position of a new list, if one list is empty end this operations
; For example, if given '(1 2 3) '(a b c d e), it should return '((1 . a) (2 . b) (3 . c))
(define (zip l1 l2)
  (if (empty? l1)   ;; Check if first list is empty
      '()           ;; If it is return empty list
      (if (empty? l2) ;;Otherwise check if the second list is empty
          '()       ;; If the second list is then return empty list
          (cons (cons (car l1) (car l2)) (zip (cdr l1) (cdr l2))))))
;; If neither are empty ^^^^^^^^ create a pair with the first element of each list and concat it to the result of zip with the rest of each list
      


; Cars, it will get the first elements of pairs and store in a list
; For example, if given '((1 . a) (2 . b) (3 . c)), it should return '(1 2 3) 
(define (cars lst)
  (if (empty? lst)   ;; Check if lst is empty
      '()            ;; If it is return empty list
      (cons (car (car lst)) (cars (cdr lst))))) ;;Otherwise take the first element of the first pair and concat it onto the result of cars with the rest of the pairs


;; Unzip: Given a list of pairs, split the first element of these pairs into list1, second element of these pairs into list2, then return the pair of list1 and list2
; For example, if given '((1 . a) (2 . b) (3 . c)), it should return '((1 2 3) a b c)
; HINT: (cons '(1 2 3) '(a b c)) -> '((1 2 3) a b c)
(define (unzip lst)
  (if (empty? lst)  ;; Check if list is empty
      '()           ;; If it is return the empty list
      (cons (cars lst) (cdrs lst))))  ;; Otherwise concat cars of lst with cdrs of lst


; Cdrs: Given a list, it may contain a list length iw 2, or a pair. Get the cdr element of the pairs or lists
; For example, if given '((a 1) (c . 2) (d 3)), it should return '((1) 2 (3))

(define (cdrs lst)
  (if (empty? lst)   ;; Check if lst is empty
      '()            ;; If it is return empty list
      (cons (cdr (car lst)) (cdrs (cdr lst)))))  ;;;;Otherwise take the last element of the first pair and concat it onto the result of cdrs with the rest of the pairs