#lang slideshow

#|      Murashko Artem SD20-01     |#
#| Programming Paradigms Fall 2022 |#
#|         Problem Set #4          |#

; Exercise 1

; Let me introduce helper functions:
(define (remove-first-n vals num)
  (cond
    [(empty? vals) vals]
    [(<= num 0) vals]
    [else (remove-first-n (rest vals) (sub1 num))]))

(define (get-first-n vals num ans)
  (cond
    [(empty? vals) ans]
    [(<= num 0) ans]
    [else (cons (first vals) (get-first-n (rest vals) (sub1 num) ans))]))

; 1.a

(define (replicate num val)
  (define (helper num val ans)
    (cond
      [(equal? num 0) ans]
      [else (cons val (helper (sub1 num) val ans))]))

  (helper num val '()))

(replicate 3 'a) ; '(a a a)

(replicate 3 '(1 . 2)) ; '((1 . 2) (1 . 2) (1 . 2))

; 1.b

(define (split vals num)
  (cons (get-first-n vals num '())
        (reverse (get-first-n (reverse vals) (- (length vals) num) '()))))

(split '(1 2 3 4 5) 2)
; '((1 2) 3 4 5) (equals to '((1 2) . (3 4 5)),
; since '((a b) . (c d)) renders as '((a b) c d))

(split '(a b c d) 4)
; '((a b c d))

(split '(a b c) 4)
; '((a b c))

(split '(a b c) 0)
; '(() a b c)

; 1.c

(define (chunks vals num)
  (define (helper vals num answer)
    (cond
      [(empty? vals) answer]
      [(<= num 0) vals]
      [else (cons (get-first-n vals num '())
                  (helper (remove-first-n vals num) num answer))]))
    

  (helper vals num '()))

(chunks '(1 2 3 4 5) 2)
; '((1 2) (3 4) (5))

(chunks '(a b c d e f) 3)
; '((a b c) (d e f))

; 1.d

(define (windows vals num)
  (define (helper vals num answer)
    (cond
      [(empty? vals) answer]
      [(<= (length vals) num) vals]
      [else (cons (get-first-n vals num '())
                  (helper (remove-first-n vals 1) num answer))]))
  
  (helper vals num '()))

(windows '(1 2 3 4 5) 2)
; '((1 2) (2 3) (3 4) (4 5))

(windows '(a b c d e) 3)
; '((a b c) (b c d) (c d e))

; Exercise 2

; Let me introduce helper functions:

(define (all-pairs vals)
  (define (helper)
    (map (lambda (val1)
           (map (lambda (val2)
                  (cons val1 val2)) (remove val1 vals))) vals))

   (foldl (lambda (val ans)
           (foldl (lambda (pair ans)
                    (cons pair ans)) ans val)) '() (helper)))

; 2.a

(define (pairs vals)
  (define (helper)
    (map (lambda (val1)
           (map (lambda (val2)
                  (cons val1 val2)) (remove val1 vals))) vals))

  (foldl (lambda (val ans)
           (foldl (lambda (pair ans)
                    (cons pair (remove (cons (cdr pair) (car pair)) ans))) ans val)) '() (helper)))

(pairs '(a b c d))
; '((a . b) (a . c) (a . d) (b . c) (b . d) (c . d))

; 2.b
(define (splits lst)
  (foldl (lambda (sublst n ans)
           (append (cons (split sublst n) '()) ans))
         '() (replicate (add1 (length lst)) lst) (inclusive-range 0 (length lst))))

(splits '(a b c))

; 2.c

(define (max-product vals)
  (foldl (lambda (val ans)
           (if (> (* (car val) (cdr val)) (* (car ans) (cdr ans))) val ans)) (first (all-pairs vals)) (all-pairs vals)))

(max-product '(1 2 3 4 3 2 1))
; '(3 . 4)

; 2.d

(define (max-binary-op op vals)
  (foldl (lambda (val ans)
           (if (> (op (car val) (cdr val)) (op (car ans) (cdr ans))) val ans)) (first (all-pairs vals)) (all-pairs vals)))


(max-binary-op * '(1 2 3 4 3 2 1))
; '(3 . 4)

(max-binary-op - '(1 2 3 4 3 2 1))
; '(4 . 1)



