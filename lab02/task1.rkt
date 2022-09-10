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
               
;(binary-to-decimal '(1 0 1 1 0)) ; ==> 22

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

;(count-zeros '(0 0 0 1 0 1 1 0)) ; ==> 2

; 1(c)

(define (encode-with-lengths lst)
  (define (skip-leading-zeros lst)
    (cond
      [(empty? lst) lst]
      [(= (first lst) 1) lst]
      [else (skip-leading-zeros(rest lst))]))

  (define (count prev cnter lst ans)
    (cond
     [(and (empty? lst) (= cnter 0)) (list)]
     [(empty? lst) (cons cnter ans)]
     [(and (empty? ans) (= cnter 0)) (count (first lst) 1 (rest lst) ans)]
     [(= (first lst) prev) (count prev (+ 1 cnter) (rest lst) ans)]
     [else (count (first lst) 1 (rest lst) (cons cnter ans))]))

  
  (reverse (count 0 0 (skip-leading-zeros lst) (list))))

;(encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0)) ; ==> '(2 1 3 2)
;(encode-with-lengths '()) ; ==> '()
;(encode-with-lengths '(0 0)) ; ==> '()

; 1(d)

(define (binary-odd? lst)
  (cond
    [(= (first (reverse lst)) 1) #t]
    [else #f]))

;(binary-odd? '(1 0 1 1 0)) ; ==> #f
;(binary-odd? '(1 0 1 1 1)) ; ==> #t

; 1(e)

(define (decrement lst)
  (define (skip-leading-zeros lst)
    (cond
      [(empty? lst) (list 0)]
      [(= (first lst) 1) lst]
      [else (skip-leading-zeros(rest lst))]))

  (define (invert-bits bits res)
    (cond
      [(empty? bits) (reverse res)]
      [(= (first bits) 1) (invert-bits (rest bits) (cons 0 res))]
      [else (invert-bits (rest bits) (cons 1 res))]))

  (define (helper temp lst)
    (cond
      [(empty? lst) (list 0)]
      [(= (first lst) 1) (append (invert-bits temp (list)) (cons 0 (rest lst)))]
      [else (helper (cons (first lst) temp) (rest lst))]))

  (skip-leading-zeros (reverse (helper (list) (reverse lst)))))
  
;(decrement '(1 0 1 1 0)) ; ==> '(1 0 1 0 1)
;(decrement '(1 0 0 0 0)) ; ==> '(1 1 1 1)
;(decrement '(0)) ; ==> '(0)

  

