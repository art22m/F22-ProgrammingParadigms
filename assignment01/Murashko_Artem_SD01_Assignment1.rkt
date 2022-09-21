#lang slideshow

#|      Murashko Artem SD20-01     |#
#| Programming Paradigms Fall 2022 |#
#|      Homework Assignment #1     |#

; In this assignment I need to implement the tools for symbolic differentiation of expressions.
; Let me introduce helper predicates and functions.

; Exercise 2.1

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

