#lang slideshow

; 2(a)

(define (alternating-sum lst)
  (cond
    [(empty? lst) 0]
    [(= (length lst) 1) (first lst)]
    [else (+ (alternating-sum (rest (rest lst))) (- (first lst) (second lst)))]))

;(alternating-sum (list 6 2 4 1 3 9))

; 2(b)

#|

(alternating-sum (list 1 2 3 4 5))
(+ (alternating-sum '(3 4 5)) (- 1 2))
(+ (alternating-sum '(3 4 5)) -1)
(+ (+ (alternating-sum '(5)) (- 3 4)) -1)
(+ (+ (alternating-sum '(5)) -1) -1)
(+ (+ 5 -1) -1)
(+ 4 -1)
3

|#

; 2(c)
; Let's solve this task using tail recursion

(define (alternating-sum-tail lst)
  (define (helper lst sum isPlus?)
    (cond
      [(empty? lst) sum]
      [isPlus? (helper (rest lst) (+ sum (first lst)) #f)]
      [else (helper (rest lst) (- sum (first lst)) #t)]))

  (helper lst 0 #t))

;(alternating-sum-tail (list 6 2 4 1 3 9))

#|

Let's analyze evaluation of tail recursion:

(alternating-sum-tail (list 1 2 3 4 5))
(helper '(1 2 3 4 5) 0 #t)
(helper '(2 3 4 5) (+ 0 1) #f)
(helper '(2 3 4 5) 1 #f)
(helper '(3 4 5) (- 1 2) #t)
(helper '(3 4 5) -1 #t)
(helper '(4 5) (+ -1 3) #f)
(helper '(4 5) 2 #f)
(helper '(5) (- 2 4) #t)
(helper '(5) -2 #t)
(helper '() (+ -2 5) #f)
(helper '() 3 #f)
3

The solution with tail recursion produces more steps,
but we use less memory since we do not store intermediate calculations in the memory.

|#

(define (dec n) (- n 1))
(define (f n)
  (cond
    [(<= n 2) (- 10 n)]
    [else (* (f (dec (dec n))) (f (dec n)))]))
  
(f 3)