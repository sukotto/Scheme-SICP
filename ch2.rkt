#lang racket
(require "ch1.rkt")
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.1 Introduction to Data Abstraction
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;(define (make-rat n d) (cons n d))

; Exercise 2.1
(define (make-rat n d)
  (let ((g (gcd n d)))
    (cons (/ n g) (abs(/ d g)))))

(define (numer x) (car x))

(define (denom x) (cdr x))

(define (print-rat x)
  (newline)
  (display (numer x))
  (display "/")
  (display (denom x)))

(define one-half (make-rat 1 2))

(define one-third (make-rat 1 3))

; Exercise 2.2 Represent line segments in a plane
(define (make-point x y)
  (cons x y))

(define (x-point point)
  (car point))

(define (y-point point)
  (cdr point))

(define (make-segment start-point end-point)
  (cons start-point end-point))

(define (start-segment segment)
  (car segment))

(define (end-segment segment)
  (cdr segment))

(define (midpoint-segment segment)
  (let*((start (start-segment segment))
        (end (end-segment segment))
        (mid-x (/ (+ (x-point start)(x-point end))2))
        (mid-y (/ (+ (y-point start)(y-point end))2))
        (mid-point (make-point mid-x mid-y)))
    mid-point))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

; Exercise 2.3 Implement a representation for rectangles
;1st Representation
;(define (make-rectangle point width height)
;  (list point width height))
;
;(define (r-point rectangle)
;  (first rectangle))
;
;(define (r-width rectangle)
;  (second rectangle))
;
;(define (r-height rectangle)
;  (third rectangle))
;2nd Representation
(define (make-rectangle p1 p2)
  (let*((x1 (x-point p1))
       (y1 (y-point p1))
       (x2 (x-point p2))
       (y2 (y-point p2))
       (width (abs(- x2 x1)))
       (height(abs(- y2 y1))))
    (list p1 p2 width height)))

(define (r-width rectangle)
  (third rectangle))

(define (r-height rectangle)
  (fourth rectangle))


(define (r-perimeter rectangle)
  (let((w (r-width rectangle))
       (h (r-height rectangle)))
    (+ (* 2 w)(* 2 h))))

(define (r-area rectangle)
  (let((w (r-width rectangle))
       (h (r-height rectangle)))
    (* w h)))
         
;2.1.4 Interval Arithmetic
(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (let ((p1 (* (lower-bound x) (lower-bound y)))
        (p2 (* (lower-bound x) (upper-bound y)))
        (p3 (* (upper-bound x) (lower-bound y)))
        (p4 (* (upper-bound x) (upper-bound y))))
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))

(define (div-interval x y)
  (mul-interval x 
                (make-interval (/ 1.0 (upper-bound y))
                               (/ 1.0 (lower-bound y)))))

;Exercise 2.7
(define (make-interval a b) (cons a b))

(define (lower-bound interval)
  (car interval))

(define (upper-bound interval)
  (cdr interval))

;Exercise 2.8
(define (sub-interval a b)
  (make-interval (- (lower-bound a)(upper-bound b))
                 (- (upper-bound a)(lower-bound b))))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 2))
(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 2))

; Exercise 2.12
(define (make-center-percent center percent)
  (let*((bound (* center (/ percent 100.0)))
        (lower (- center bound))
       (upper (+ center bound)))
    (make-interval lower upper)))

(define (percent interval)
  (let*((width (abs(- (center interval) 
                      (lower-bound interval))))
        (percent-value (/ (* width 100.0)
                          (center interval))))
    percent-value))
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.2 Hierarchical Data and the Closure Property
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; 2.2.1 Representing Sequences
; Exercise 2.17.
(define (last-pair l)
  (if(null? (cdr l))
     l
     (last-pair (cdr l))))

; Exercise 2.18.
(define (reverse l)
  (define (aux l acc)
    (if(null? l)
       acc
       (aux (cdr l)(cons(car l)acc))))
  (aux l '()))

; Exercise 2.20.
(define (same-parity x . xs)
  (cond[(even? x)
     (cons x (filter even? xs))]
     [(odd? x)
      (cons x (filter odd? xs))]
     [else "Error: wrong input"]))

; Exercise 2.21.
;(define (square-list items)
;   (if (null? items)
;       null
;       (cons (* (car items)(car items)) 
;             (square-list (cdr items)))))

(define (square-list items)
   (map (λ(x)(* x x)) items))

; Exercise 2.23.
(define (for-each f xs)
  (if(null? xs) 
     #t
     (and (f (car xs))
              (for-each f (cdr xs)))))

;2.2.2 Hierarchical Structures
(define (count-leaves x)
  (cond ((null? x) 0)  
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

;2.28.
(define(fringe x)
  (cond[(null? x)null]
       [(not(list? x))(list x)]
       [(append (fringe (first x))
              (fringe (rest x)))]))

;Mapping over trees
;2.30.
;(define (square-tree t)
;  (cond[(null? t)null]
;       [(not(list? t))(* t t)]
;       [else (cons(square-tree (first t))
;                  (square-tree (rest t)))]))

;2.31
(define (tree-map f t)
  (cond [(null? t)null]
        [(not(list? t))(f t)]
        [else (cons (tree-map f (first t))
                    (tree-map f (rest t)))]))

(define (square-tree tree) (tree-map square tree))


;2.32.
(define (subsets s)
  (if (null? s)
      (list null)
      (let ((rest (subsets (cdr s))))
        (append rest (map (λ(x)(cons (car s) x)) rest)))))

;2.2.3 Sequences as Conventional Interfaces
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      null
      (cons low (enumerate-interval (+ low 1) high))))

;Exercise 2.33 Define some list-manipulation operations as accumulations
 (define (acc-map p sequence)
   (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
 
 (define (acc-append seq1 seq2)
   (accumulate cons seq2 seq1))
 
 (define (acc-length sequence)
   (accumulate (lambda (x y) (+ 1 y)) 0 sequence))
 
 ;Exercise 2.34
 (define (horner-eval x coefficient-sequence)
   (accumulate (lambda (this-coeff higher-terms) (+ this-coeff (* x higher-terms)))
               0
               coefficient-sequence))
 
 
 ;Exercise 2.36
 (define (accumulate-n op init seqs)
   (if (null? (car seqs))
       '()
       (cons (accumulate op init (map (lambda(x)(car x)) seqs))
             (accumulate-n op init (map(lambda(x)(cdr x)) seqs)))))
 
 ;Exercise 2.38
 (define (fold-left op initial sequence)
   (define (iter result rest)
     (if (null? rest)
         result
         (iter (op result (car rest))
               (cdr rest))))
   (iter initial sequence))
 
 (define (fold-right op initial sequence)
   (accumulate op initial sequence))
 
 ;Exercise 2.39
 (define (fold-reverse sequence)
   (fold-left (lambda(x y) (cons y x)) '() sequence))