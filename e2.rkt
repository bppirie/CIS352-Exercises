#lang racket
(provide    mk-point
      point?
      point-dist
      binary-tree?
      sorted-tree?
      balanced-tree?)

; Now we assume a point is in format of '(point x y), where x and y are integers. For example, '(point 1 3) is a point at x=3, y=4

; Now we need a constructor for the point, which takes into two integers and returns a point
(define (mk-point x y)
  (cons 'point (cons x (cons y '()))))

; Now, how can we know whether a object is a point? We need a function point? takes in a parameter, checks
; 1. first element in this list is 'point
; 2. Second element in this list is number
; 3. Third element in this list is number
; 4. This list only contains these 3 elements
(define (point? x)
  (cond [(not (equal? 'point (first x))) #f]
        [(not (and (number? (second x)) (number? (third x)))) #f]
        [(> (length x) 3) #f]
        [else #t]))

; Remember that point-distance in e0? Now we want to apply that to points!
; Write a function point-dist which takes in 2 points, return the distance between these 2 points
(define (point-dist p1 p2)
  (let ([x1 (second p1)] [y1 (third p1)] [x2 (second p2)] [y2 (third p2)])
    (sqrt (+ (* (- x2 x1) (- x2 x1)) (* (- y2 y1) (- y2 y1))))))

; In the video we see how binary tree looks like, please complete the binary-tree?, which checks whether t is a binary tree
(define (binary-tree? t)
  (cond
    [(and (equal? (length t) 2) (equal? (first t) 'leaf) (integer? (second t))) #t]
    [(and (equal? (length t) 4) (equal? (first t) 'node) (integer? (second t)) (binary-tree? (third t)) (binary-tree? (fourth t))) #t]
    [else #f]))

; A sorted tree is a tree that, any node is greater than the left side, but less than the right side
; Write a function sorted-tree, checks whether a binary tree is sorted
(define (sorted-tree? t)
  (if (equal? 'leaf (first t))
      #t
      (and (< (second (third t)) (second t)) (> (second (fourth t)) (second t)) (sorted-tree? (third t)) (sorted-tree? (fourth t)))))

; A balanced tree is a binary tree that, keep its height at minimal. 
; In other words, the longest path to any leaf and the shortest path to any leaf is equal or less than 1
; Write a function is-balanced-tree?, checks whether a binary tree is balanced

(define (balanced-tree? t)
  (if (equal? 'leaf (first t))
      #t
      (<= (abs (- (height (third t)) (height (fourth t)))) 1)))

(define (height t)
  (if (equal? 'leaf (first t))
      0
      (+ (max (height (third t)) (height (fourth t))) 1)))
  