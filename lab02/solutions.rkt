#lang slideshow

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