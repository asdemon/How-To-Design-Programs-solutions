#lang sicp

(define (member? x l)
  (cond
    ((null? l) #f)
    ((eq? x (car l)) #t)
    (else (member? x (cdr l)))))

(define (cycle? l0)
   (define (cycle?/a l acc)
     (cond
       ((null? l) #f)
       ((not (pair? l)) #f)
       ((null? (cdr l)) #f)
       ((member? l acc) #t)
       (else (cycle?/a (cdr l) (cons l acc)))))
  (cycle?/a l0 '()))
     







(define a (cons 1 2))
(define b (cons a a))
(define c (cons b b))
(cycle? a)
(cycle? b)
(cycle? c)

(define d (cons 3 a))
(cycle? d)
(set-cdr! a d)
(cycle? d)