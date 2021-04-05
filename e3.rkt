#lang racket

;; Exercises 3: Direct-style and tail recursion
(provide
      truncate-t
      truncate-d
      filter-t)

;; If we have a list, how can we truncate the list from index i to j?
;; Write a function truncate takes 3 param, i j and list return the
;; list starting at i and end at j, (truncate 2 5 '(0 1 2 3 4 5 6)) ->
;; '(2 3 4 5).
(define (truncate-d now i j lst)
  (if (= i 0)
        (cond [(= j (- (length lst) 1)) lst]
              [(= j (length lst)) lst]
              [else (truncate-d now i j (reverse (cdr (reverse lst))))])
        (truncate-d now (- i 1) (- j 1) (cdr lst))))
;; Rewrite truncate to use tail-recursion
;;
;; HINT: In tail recursion you may need to revrese the
;;; result when return it You can assume i < j and j <= length of lst
(define (truncate-t now i j lst)
    (if (= i 0)
        (cond [(= j (- (length lst) 1)) lst]
              [(= j (length lst)) lst]
              [else (truncate-t now i j (reverse (cdr (reverse lst))))])
        (truncate-t now (- i 1) (- j 1) (cdr lst))))

;; The function filter-t should filter lst so that it returns the
;; subset of lst that satisfies func.
(define (filter-t func lst)
    (define (h f l acc)
      (match l
        ['() acc]
        [`(,hd . ,tl) (if (func hd)
                          (h f tl (cons hd acc))
                          (h f tl acc))]))
  (reverse (h func lst '())))
