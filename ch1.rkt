#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.1 The Elements of Programming
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(define (square x)(* x x))

(define (sum-of-squares x y)
  (+ (square x) (square y)))

;;Exercise 1.3 Define a procedure that takes 3 numbers 
;;and returns the sum of squares of the 2 larger numbers
(define (sum-squares-larger x y z)
  (cond [(= x (min x y z)) (sum-of-squares y z)]
        [(= y (min x y z)) (sum-of-squares x z)]
        [(= z (min x y z)) (sum-of-squares x y)]
        [else "Error: wrong input for sum-squares-larger"]))

;1.1.7 Example: Square Roots by Newton's Method
(define (sqrt-iter guess x)
  (if (good-enough? guess x)
      guess
      (sqrt-iter (improve guess x)
                 x)))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess x)
  (< (abs (- (square guess) x)) 0.001))

(define (sqrt x)
  (sqrt-iter 1.0 x))

;;Exercise 1.8 Newton's method cube roots
(define (cube x)(* x x x ))

(define (cbrt-iter guess x)
  (if (cbrt-good-enough? guess x)
      guess
      (cbrt-iter (cbrt-improve guess x)
                 x)))

(define (cbrt-improve guess x)
  (/ (+ (/ x (square guess))(* 2 guess)) 3)) 

(define (cbrt-good-enough? guess x)
  (< (abs (- (cube guess) x)) 0.001))

(define (cbrt x)
  (cbrt-iter 1.0 x))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.2 Procedures and the Processes They Generate
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

; 1.2.2 Tree Recursion
; Exercise 1.12 Pascal's Triangle
; Recursive process
(define (pascal row col)
  (if (or (= 0 col)(= col row))
      1
      (+ (pascal (- row 1)(- col 1)) 
         (pascal (- row 1) col))))

; Exercise 1.16
(define (fast-expt b n)
  (fast-expt-iter b n 1))

(define (fast-expt-iter b n a)
  (cond[(= n 0) a]
       [(even? n)(fast-expt-iter (square b) (/ n 2) a)]
       [else (fast-expt-iter b (- n 1) (* b a))]))

; Exercise 1.17-1.18
; Multiplication function analogous to fast-expt
(define (double x)
  (* x 2))

(define (halve x)
  (/ x 2))

(define (fast-mult a b)
  (mult-iter a b 0))

(define (mult-iter a b acc)
  (cond[(= b 0) acc]
       [(even? b)(mult-iter (double a)(halve b) acc)]
       [else (mult-iter a (- b 1) (+ a acc))]))

; 1.2.6 Example: Testing for Primality
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))

(define (smallest-divisor n)
  (find-divisor n 2))
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
(define (divides? a b)
  (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else
         (remainder (* base (expmod base (- exp 1) m))
                    m))))        

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((fermat-test n) (fast-prime? n (- times 1)))
        (else false)))

(define runtime current-inexact-milliseconds)

; Exercise 1.22
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))
(define (start-prime-test n start-time)
  (when (prime? n)
      (report-prime (- (runtime) start-time))))
(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))

(define (search-for-primes from to)
  (for([i (in-range from to)])
    (when (odd? i)
      (timed-prime-test i))))

; Exercise 1.27. Demonstrate that the Carmichael numbers
; e.g. 561, 1105, 1729, 2465, 2821, 6601 really do fool
; the Fermat test
(define (all-congruent n)
 (let([congruent #true])
  (for([a(in-range 1 (- n 1))])
    (when(not(= (expmod a n n) a))
       (set! congruent #false)))
    congruent))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.3.Formulating Abstractions with Higher-Order Procedures
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;1.3.1 Procedures as Arguments
; (define (sum term a next b)
;    (if (> a b)
;        0
;        (+ (term a)
;           (sum term (next a) next b))))
; 
(define (inc n) (+ n 1))
(define (sum-cubes a b)
   (sum cube a inc b))

(define (identity x) x)

(define (sum-integers a b)
   (sum identity a inc b))

(define (pi-sum a b)
   (define (pi-term x)
     (/ 1.0 (* x (+ x 2))))
   (define (pi-next x)
     (+ x 4))
   (sum pi-term a pi-next b))

(define (integral f a b dx)
   (define (add-dx x) (+ x dx))
   (* (sum f (+ a (/ dx 2.0)) add-dx b)
      dx))

; Exercise 1.29.
; Simpson's Rule for numerical integration
(define (simpson f a b n)
  (let*([h (/ (- b a) n)]
        [yk (λ (k) (f (+ a (* k h))))]
        [s (+ (f a)(f b))])
    (for([i(in-range 1 n 2)])
      (set! s (+ s (* 4 (yk i)))))
    (for([i(in-range 2 (- n 1) 2)])
      (set! s (+ s (* 2 (yk i)))))
    (/ (* s h) 3)))
    
;Exercise 1.30.
; iterative version of sum
; (define (sum term a next b)
;   (define (iter a result)
;     (if(> a b)
;        result
;        (iter (next a)(+ result (term a)))))
;   (iter a 0))

; Exercise 1.31
; (define (product term a next b)
;   (if (> a b)
;       1
;       (* (term a)
;          (product term (next a) next b))))

(define (factorial n)
  (product identity 1 inc n))

; (define (product term a next b)
;   (define (iter a result)
;     (if (> a b)
;         result
;         (iter (next a) (* result (term a)))))
;   (iter a 1))

; Exercise 1.32
; Show that some and product are special cases of a more
; general notion called accumulate
; (define (accumulate combiner null-value term a next b)
;   (if (> a b)
;       null-value
;       (combiner (term a)
;         (accumulate combiner null-value term (next a)
;                     next b))))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))

(define (accumulate combiner null-value term a next b)
    (define (iter a result)
      (if(> a b)
         result
         (iter (next a)(combiner result (term a)))))
  (iter a null-value))

; Exercise 1.33.
(define (filtered-accumulate combiner null-value term a
                             next b filter)
  (define (iter a result)
    (cond[(> a b) result]
         [(filter a)(iter (next a)
                          (combiner result (term a)))]
         [else (iter (next a) result)]))
  (iter a null-value))

(define (sum-prime-squares a b)
  (filtered-accumulate + 0 square a inc b prime?))

(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))

(define (product-relative-primes n)
  (filtered-accumulate * 1 identity 1 inc n
             (λ(x)(= 1 (gcd x n)))))
                           
       
;1.3.3 Procedures as General Methods
(define tolerance 0.00001)
(define (fixed-point f first-guess)
  (define (close-enough? v1 v2)
    (< (abs (- v1 v2)) tolerance))
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
          next
          (try next))))
  (try first-guess))

(define dx 0.00001)

(define (deriv g)
  (lambda (x)
    (/ (- (g (+ x dx)) (g x))
       dx)))

(define (newton-transform g)
  (lambda (x)
    (- x (/ (g x) ((deriv g) x)))))
(define (newtons-method g guess)
  (fixed-point (newton-transform g) guess))

; Exercise 1.40.
(define (cubic a b c)
  (λ(x)(+ (cube x)(* a (square x)) (* b x) c)))

; Exercise 1.41
(define (double-f f)
  (λ(x)(f (f x))))

; Exercise 1.42.
(define (compose f g)
  (λ(x)(f(g x))))



