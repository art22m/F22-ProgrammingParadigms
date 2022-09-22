#lang slideshow

; Murashko Artem SD20-01
; Problem set 3

; Ex 1

; 1.a
(define (binary-to-decimal elems)
  (foldl (lambda (bit sum) (+ (* sum 2) bit)) 0 elems))

(binary-to-decimal '(1 0 1 1 0)) ; ==> 22

; 1.b
(define (remove-leading-zeros elems)
  (reverse
   (foldl (lambda (bit ans)
            (cond
              [(equal? bit 1) (cons bit ans)]
              [(cons? ans) (cons bit ans)]
              [else '()]))
          '() elems)))

(remove-leading-zeros '(0 0 0 1 0 1 1 0)) ; ==> '(1 0 1 1 0)

; 1.c
(define (count-zeros elems)
  (- (length (remove-leading-zeros elems)) (apply + (remove-leading-zeros elems))))

(count-zeros '(0 0 0 1 0 1 1 0)) ; ==> 2

; 1.d
(define (group-consecutive elems)
  (reverse (foldl (lambda (bit ans)
                    (cond
                      [(empty? ans) (cons (list bit) ans)]
                      [(equal? (first (first ans)) bit) (cons (cons bit (first ans)) (rest ans))]
                      [else (cons (list bit) ans)]))
                  '() elems)))
 
(group-consecutive '(0 0 0 1 0 1 1 0)) ; ==> '((0 0 0) (1) (0) (1 1) (0))

; 1.e
(define (encode-with-lengths elems)
  (map (lambda (lst) (length lst)) (group-consecutive (remove-leading-zeros elems))))

(encode-with-lengths '(0 0 0 1 1 0 1 1 1 0 0)) ; ==> '(2 1 3 2)

; Ex 2

; 2.a
(define employees
  '(("John" "Malkovich" . 29)
    ("Anna" "Petrova"   . 22)
    ("Ivan" "Ivanov"    . 23)
    ("Anna" "Karenina"  . 40)))

(define (fullname employee)
  (cons (car employee) (car (cdr employee))))

(fullname '("John" "Malkovich" . 29)) ; '("John" . "Malkovich")

; 2.b
(define (find-employee-anna employees)
  (filter (lambda (employee) (equal? "Anna" (car employee))) employees))

(find-employee-anna employees) ; '(("Anna" "Petrova" . 22) ("Anna" "Karenina" . 40))

; 2.c
(define (employees-over-25 employees)
  (filter (lambda (employee) (> (cdr (cdr employee)) 25)) employees))

(employees-over-25 employees) ; '(("John" . "Malkovich") ("Anna" . "Karenina"))