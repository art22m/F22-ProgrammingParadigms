#lang slideshow

#|      Murashko Artem SD20-01     |#
#| Programming Paradigms Fall 2022 |#
#|      Homework Assignment #1     |#

; In this assignment I need to implement the tools for symbolic differentiation
; of expressions.

; Exercise 1.2
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
    

