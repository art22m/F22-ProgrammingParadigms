#lang slideshow

#|      Murashko Artem SD20-01     |#
#| Programming Paradigms Fall 2022 |#
#|      Homework Assignment #1     |#

; In this assignment I need to implement the tools for symbolic differentiation
; of expressions.


; Exercise 1.1 & 1.6
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
    [(and (list? expr)
          (and (equal? (length expr) 3)
               (equal? (first expr) '+)))
     #t]
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

; check whether a given expression is a subtraction
(define (sub? expr)
  (cond
    [(and (list? expr)
          (and (equal? (length expr) 3)
               (equal? (first expr) '-)))
     #t]
    [else #f]))

; extract minuend from a subtraction
(define (minuend expr)
  (cond
    [(sub? expr) (second expr)]
    [else (error "Given expr is not a subtraction:" expr)]))

; extract second summand from a subtraction
(define (subtrahend expr)
  (cond
    [(sub? expr) (third expr)]
    [else (error "Given expr is not a subtraction:" expr)]))

; check whether a given expression is a product
(define (product? expr)
   (cond
    [(and (list? expr)
          (and (equal? (length expr) 3)
               (equal? (first expr) '*)))
     #t]
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

; check whether a given expression is an exponentiation
(define (exponentiation? expr)
   (cond
    [(and (list? expr) (and (equal? (length expr) 3) (equal? (first expr) '^))) #t]
    [else #f]))

; extract a base from an exponentiation
(define (exponentiation-base expr)
  (cond
    [(exponentiation? expr) (second expr)]
    [else (error "Given expr is not an exponentiation:" expr)]))

; extract a power from an exponentiation
(define (exponentiation-power expr)
  (cond
    [(exponentiation? expr) (third expr)]
    [else (error "Given expr is not an exponentiation:" expr)]))

; check whether a given expression is a sinus
(define (sin? expr)
   (cond
    [(and (list? expr)
          (and (equal? (length expr) 2)
               (equal? (first expr) 'sin)))
     #t]
    [else #f]))

; extract an argument from a sinus
(define (sin-arg expr)
  (cond
    [(sin? expr) (second expr)]
    [else (error "Given expr is not a sinus" expr)]))

; check whether a given expression is a cosinus
(define (cos? expr)
   (cond
    [(and (list? expr)
          (and (equal? (length expr) 2)
               (equal? (first expr) 'cos)))
     #t]
    [else #f]))

; extract an argument from a cosinus
(define (cos-arg expr)
  (cond
    [(cos? expr) (second expr)]
    [else (error "Given expr is not a cosinus" expr)]))

; check whether a given expression is a tangent
(define (tan? expr)
   (cond
    [(and (list? expr)
          (and (equal? (length expr) 2)
               (equal? (first expr) 'tan)))
     #t]
    [else #f]))

; extract an argument from a tangent
(define (tan-arg expr)
  (cond
    [(tan? expr) (second expr)]
    [else (error "Given expr is not a tangent" expr)]))

; check whether a given expression is a natural logarithm
(define (log? expr)
   (cond
    [(and (list? expr)
          (and (equal? (length expr) 2)
               (equal? (first expr) 'log)))
     #t]
    [else #f]))

; extract an argument from a natural logarithm
(define (log-arg expr)
  (cond
    [(log? expr) (second expr)]
    [else (error "Given expr is not a natural logarithm" expr)]))

; check whether a given expression is a polyvariadic sum
(define (polyvariadic-sum? expr)
  (cond
    [(and (list? expr)
          (and (> (length expr) 3)
               (equal? (first expr) '+)))
     #t]
    [else #f]))

; check whether a given expression is a polyvariadic product
(define (polyvariadic-product? expr)
   (cond
    [(and (list? expr)
          (and (> (length expr) 3)
               (equal? (first expr) '*)))
     #t]
    [else #f]))

; convert polyvariadic expression into normal prefix form
(define (polyvariadic-to-normal expr)
  (cond
    [(polyvariadic-sum? expr)
     (list '+
           (polyvariadic-to-normal (second expr))
           (polyvariadic-to-normal (cons '+ (rest (rest expr)))))]
    [(sum? expr)
     (list '+
           (polyvariadic-to-normal (second expr))
           (polyvariadic-to-normal (third expr)))]
    [(polyvariadic-product? expr)
     (list '*
           (polyvariadic-to-normal (second expr))
           (polyvariadic-to-normal (cons '* (rest (rest expr)))))]
    [(product? expr)
     (list '*
           (polyvariadic-to-normal (second expr))
           (polyvariadic-to-normal (third expr)))]
    [else expr]))
         

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

    ; Derivative for a substraction
    [(sub? expr) (list '-
                       (derivative (minuend expr) var)
                       (derivative (subtrahend expr) var))]  

    ; Derivative for a product
    [(product? expr) (list '+
                           (list '*
                                 (derivative (multiplier-1 expr) var)
                                 (multiplier-2 expr))
                           (list '*
                                 (multiplier-1 expr)
                                 (derivative (multiplier-2 expr) var)))]

    ; Derivative for a exponentiation
    ; Actually, subtraction can be replaced by a sum, where one of the terms
    ; is multiplied by -1.
    [(exponentiation? expr)
     (list '+
           (list '*
                 (list '^ (exponentiation-base expr) (exponentiation-power expr))
                 (list '*
                       (list 'log (exponentiation-base expr))
                       (derivative (exponentiation-power expr) var)))
           (list '*
                 (list '^ (exponentiation-base expr) (list '- (exponentiation-power expr) 1))
                 (list '*
                       (exponentiation-power expr)
                       (derivative (exponentiation-base expr) var))))]
    
    ; Derivative for a sinus
    [(sin? expr) (list '*
                         (list 'cos (sin-arg expr))
                         (derivative (sin-arg expr) var))]

    ; Derivative for a cosinus
    [(cos? expr) (list '*
                         -1
                         (list '*  
                               (list 'sin (cos-arg expr))
                               (derivative (cos-arg expr) var)))]

    ; Derivative for a tangent
    
    ; Division can be replaced by raising to -1 power: 1/x = x^(-1)
    [(tan? expr) (list '*
                       (list '^
                             (list '^
                                   (list 'cos (tan-arg expr))
                                   2)
                             -1)
                       (derivative (tan-arg expr) var))]

    ; Derivative for a logarithm
    [(log? expr) (list '*
                       (list '^
                             (log-arg expr)
                             -1)
                       (derivative (log-arg expr) var))]

    ; Derivative for a polyvariadic sum
    
    ; Just go through all summands and take the derivative of it.
    ; The answer will be still polyvariadic.
    [(polyvariadic-sum? expr) (cons '+
                                    (map
                                     (lambda (curr) (derivative curr var))
                                     (rest expr)))]

    ; Derivative for a polyvariadic product
    
    ; For every multiplier take derivative of it, multiply
    ; by the rest multipliers and add sum it.
    ; For example, (abc)' = a'bc + ab'c + abc'.   
    ; The answer will be still polyvardic.
    [(polyvariadic-product? expr)
     (cons '+
           (map
            (lambda (curr)
              (cons '*
                    (cons (derivative curr var)
                          (remove curr (rest expr)))))
            (rest expr)))]

    [else (error "Given expr is not valid:" expr)]))


; Test examples

(derivative '(+ 1 x) 'x)
; = '(+ 0 1)

(derivative '(* 2 y) 'y)
; = '(+ (* 0 y) (* 2 1))

(derivative '(* (+ x y) (+ x (+ x x))) 'x)
; = '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1))))

(derivative '(sin (log x)) 'x)
; (* (cos (log x)) (* (^ x -1) 1))

; Exercise 1.3
; Let's implement a recursive function simplify that simplifies
; an expression using the following rules:

(define (simplify expr)
  (define (simplify-at-root expr)
    (cond
      ; Simplify sum
      [(sum? expr)
       (cond
         [(equal? (summand-1 expr) 0) (summand-2 expr)]
         [(equal? (summand-2 expr) 0) (summand-1 expr)]
         [(and (number? (summand-1 expr))
               (number? (summand-2 expr)))
          (+ (summand-1 expr) (summand-2 expr))]
         [else expr])]

      ; Simplify substraction
      [(sub? expr)
       (cond
         [(equal? (subtrahend expr) 0) (minuend expr)]
         [(and (number? (minuend expr))
               (number? (subtrahend expr)))
          (- (minuend expr) (subtrahend expr))]
         [else expr])]

      ; Simplify product
      [(product? expr)
       (cond
         [(equal? (multiplier-1 expr) 1) (multiplier-2 expr)]
         [(equal? (multiplier-2 expr) 1) (multiplier-1 expr)]
         [(equal? (multiplier-1 expr) 0) 0]
         [(equal? (multiplier-2 expr) 0) 0]
         [(and (number? (multiplier-1 expr))
               (number? (multiplier-2 expr)))
          (* (multiplier-1 expr) (multiplier-2 expr))]
         [else expr])]

      ; Simplify exponentiation
      [(exponentiation? expr)
       (cond
         [(equal? (exponentiation-base expr) 0) 0]
         [(equal? (exponentiation-base expr) 1) 1]
         [(equal? (exponentiation-power expr) 0) 1]
         [(equal? (exponentiation-power expr) 1) (exponentiation-base expr)]
         [(and (number? (exponentiation-base expr))
               (number? (exponentiation-power expr)))
          (expt (exponentiation-base expr) (exponentiation-power expr))]
         [else expr])]

      ; Simplify sinus
      [(sin? expr)
       (cond
         [(equal? (sin-arg expr) 0) 0]
         [else expr])]

      ; Simplify cosinus
      [(cos? expr)
       (cond
         [(equal? (cos-arg expr) 0) 1]
         [else expr])]

      ; Simplify tangent
      [(tan? expr)
       (cond
         [(equal? (tan-arg expr) 0) 0]
         [else expr])]

      ; Simplify logarithm
      [(log? expr)
       (cond
         [(equal? (log-arg expr) 1) 0]
         [(equal? (log-arg expr) 'e) 1]
         [else expr])]
            
      [else (error "Given expr is not valid:" expr)]))

  (cond
    [(sum? expr)
     (simplify-at-root (list '+
                             (simplify (summand-1 expr))
                             (simplify (summand-2 expr))))]

    [(sub? expr)
     (simplify-at-root (list '-
                             (simplify (minuend expr))
                             (simplify (subtrahend expr))))]
    
    [(product? expr)
     (simplify-at-root (list '*
                             (simplify (multiplier-1 expr))
                             (simplify (multiplier-2 expr))))]
    
    [(exponentiation? expr)
     (simplify-at-root (list '^
                             (simplify (exponentiation-base expr))
                             (simplify (exponentiation-power expr))))]

    [(sin? expr)
     (simplify-at-root (list 'sin
                             (simplify (sin-arg expr))))]

    [(cos? expr)
     (simplify-at-root (list 'cos
                             (simplify (cos-arg expr))))]

    [(tan? expr)
     (simplify-at-root (list 'tan
                             (simplify (tan-arg expr))))]

    [(log? expr)
     (simplify-at-root (list 'log
                             (simplify (log-arg expr))))]

    ; If we have polyvariadic sum or product, firstly we need to
    ; convert it to normal prefix form. After that we can work as before.
    [(or (polyvariadic-sum? expr) (polyvariadic-product? expr))
     (simplify (polyvariadic-to-normal expr))]
    
    [else expr]))

; Test examples

(simplify '(+ 0 1))
; 1

(simplify '(+ (* 0 y) (* 2 1)))
; 2

(simplify '(+ (* (+ 1 0) (+ x (+ x x))) (* (+ x y) (+ 1 (+ 1 1)))))
; '(+ (+ x (+ x x)) (* (+ x y) 3))

(simplify '(+ (log e) (+ (^ x 0) (cos 0))))
; 3

(simplify (derivative '(^ x 3) 'x))
; '(* (^ x 2) 3)


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


; Exercise 1.7
; Test examples

(derivative '(+ 1 x y (* x y z)) 'x)
; '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0)))

(simplify '(+ 0 1 0 (+ (* 1 y z) (* x 0 z) (* x y 0))))
; '(+ 1 (* y z))


; Exercise 1.8
; Implement a function variables-of that returns a (sorted) list of
; distinct variables used in a given expression:

; Firtsly I flat the expression, i.e. remove the braces.
; Then I leave only variables using filter.
; After that I sort by symbols and remove duplicates.
(define (variables-of expr)
  (remove-duplicates
   (sort
    (filter variable? (flatten expr)) symbol<?)))
    
; Test examples

(variables-of '(+ 1 x y (* x y z)))
; '(x y z)

; Exercise 1.9
; Implement a function gradient that returns a gradient of a multivariable
; expression (given explicitly the list of variables).

(define (gradient expr vars)
  (map
   (lambda (var) (simplify (derivative expr var)))
   vars))

; Test examples

(gradient '(+ 1 x y (* x y z)) '(x y z))
; '((+ 1 (* y z)) (+ 1 (* x z)) (* x y))
