#lang slideshow

; 1(a)

(define (binary-to-decimal lst)
  (define (pow num power)
    (cond
      [(= power 0) 1]
      [else (* num
               (pow num (- power 1)))]))

  (define (calculate lst id)
    (cond
      [(empty? lst) 0]
      [else (+ (* (first lst) (pow 2 id))
               (calculate (rest lst) (+ id 1)))]))

  (calculate (reverse lst) 0))
               

; 1(b)

(define (count-zeros lst)
  (define (skip-leading-zeros lst)
    (cond
      [(empty? lst) lst]
      [(= (first lst) 1) lst]
      [else (skip-leading-zeros(rest lst))]))

  (define (count-all-zeros lst)
    (cond
      [(empty? lst) 0]
      [(= (first lst) 1) (count-all-zeros(rest lst))]
      [else (+ 1
               (count-all-zeros(rest lst)))]))
      
  (count-all-zeros (skip-leading-zeros lst)))