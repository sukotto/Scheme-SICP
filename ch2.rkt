#lang racket

; 2.1 Introduction to Data Abstraction

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
         
