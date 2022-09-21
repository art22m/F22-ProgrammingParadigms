#lang slideshow

#|      Murashko Artem SD20-01     |#
#| Programming Paradigms Fall 2022 |#
#|      Homework Assignment #1     |#

; In this assignment I need to implement the tools for symbolic differentiation
; of expressions.


; Exercise 1.1
; Let me introduce helper predicates and functions.

; check whether a given expression is a variable
(define (variable? expr)
  (cond
    [(equal? expr '+) #f]
    [(equal? expr '-) #f]
    [(equal? expr '*) #f]
    [(equal? expr '/) #f]
    [(number? expr) #f]
    [(list? expr) #f]
    [(number? expr) #f]
    [(procedure? expr) #f]
    [else #t]))

; check whether a given expression is a sum
(define (sum? expr)
  (cond
    [(and (list? expr) (and (equal? (length expr) 3) (equal? (first expr) '+))) #t]
    [else #f]))

; extract first summand from a sum
(define (summand-1 expr)
  (cond
    [(sum? expr) (second expr)]
    [else (error "Given expr is not a sum:" expr)]))

; extract second summand from a sum
(define (summand-2 expr)
  (cond
    [(sum? expr) (third expr)]
    [else (error "Given expr is not a sum:" expr)]))

; check whether a given expression is a product
(define (product? expr)
   (cond
    [(and (list? expr) (and (equal? (length expr) 3) (equal? (first expr) '*))) #t]
    [else #f]))

; extract first multiplier from a product
(define (multiplier-1 expr)
  (cond
    [(product? expr) (second expr)]
    [else (error "Given expr is not a product:" expr)]))

; extract second multipler from a product
(define (multiplier-2 expr)
  (cond
    [(product? expr) (third expr)]
    [else (error "Given expr is not a product:" expr)]))


; Exercise 1.2
; Let's implement a recursive function derivative that computes a symbolic
; derivative of a given expression with respect to a given variable.

(define (derivative expr var)
  (cond
    ; Derivative for a constant
    [(number? expr) 0]

    ; Derivartive for a variable
    [(variable? expr) (if (equal? expr var) 1 0)]

    ; Derivative for a sum
    [(sum? expr) (list '+
                       (derivative (summand-1 expr) var)
                       (derivative (summand-2 expr) var))]

    ; Derivative for a product
    [(product? expr) (list '+
                           (list '*
                                 (derivative (multiplier-1 expr) var)
                                 (multiplier-2 expr))
                           (list '*
                                 (multiplier-1 expr)
                                 (derivative (multiplier-2 expr) var)))]))

; Test examples

(derivative '(+ 1 x) 'x)
; = '(+ 0 1)

(derivative '(* 2 y) 'y)
; = '(+ (* 0 y) (* 2 1))

(derivative '(* (+ x y) (+ x (+ x x))) 'x)
; = '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))


; Exercise 1.3
; Let's implement a recursive function simplify that simplifies
; an expression using the following rules:

(define (simplify expr)
  (define (simplify-at-root expr)
    (cond
      [(sum? expr)
       (cond
         [(equal? (summand-1 expr) 0) (summand-2 expr)]
         [(equal? (summand-2 expr) 0) (summand-1 expr)]
         [(and (number? (summand-1 expr)) (number? (summand-2 expr))
               (+ (summand-1 expr) (summand-2 expr)))]
         [else expr])]
      [(product? expr)
       (cond
         [(equal? (multiplier-1 expr) 1) (multiplier-2 expr)]
         [(equal? (multiplier-2 expr) 1) (multiplier-1 expr)]
         [(equal? (multiplier-1 expr) 0) 0]
         [(equal? (multiplier-2 expr) 0) 0]
         [(and (number? (multiplier-1 expr)) (number? (multiplier-2 expr))
               (* (multiplier-1 expr) (multiplier-2 expr)))]
         [else expr])]
      [else (error "Given expr is not valid:" expr)]))

  (cond
    [(sum? expr)
     (simplify-at-root (list '+
                             (simplify (summand-1 expr))
                             (simplify (summand-2 expr))))]
    [(product? expr)
     (simplify-at-root (list '*
                             (simplify (multiplier-1 expr))
                             (simplify (multiplier-2 expr))))]
    [else expr]))

; Test examples

(simplify '(+ 0 1))
; 1

(simplify '(+ (* 0 y) (* 2 1)))
; 2

(simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1)))))
; '(+ (+ x (+ x x)) (* (+ x y) 3))


; Exercise 1.5
; Let's implement a recursive function to-infix that converts
; an expression into an infix form:

(define (to-infix expr)
  (cond
    [(sum? expr)
     (list (to-infix (summand-1 expr))
           '+
           (to-infix (summand-2 expr)))]
    [(product? expr)
     (list (to-infix (multiplier-1 expr))
           '*
           (to-infix (multiplier-2 expr)))]
    [else expr]))

; Test examples

(to-infix '(+ (+ x (+ x x)) (* (+ x y) 3)))
; '((x + (x + x)) + ((x + y) * 3)

(to-infix '(+ (+ a b) (* c d)))
; '((a + b) + (c * d))