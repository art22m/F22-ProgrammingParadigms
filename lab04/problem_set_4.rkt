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


  
