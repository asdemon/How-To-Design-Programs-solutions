#lang sicp

(define (member? x l)
  (cond
    ((null? l) #f)
    ((eq? x (car l)) #t)
    (else (member? x (cdr l)))))


;the count is this function is useless and can be omitted
(define (count-pairs l0)
  (let ((accumulator '()))
  (define (count/a l count)
    (cond
      ((not (pair? l)) count)
      ((member? l accumulator) count)
      (else (+  (begin (set! accumulator (cons l accumulator)) (count/a (car l)  count))
                (begin (set! accumulator (cons l accumulator)) (count/a (cdr l)  count))
               1))))
    (count/a l0 0)))








(define a (cons 1 2))
(define b (cons a a))
(define c (cons b b))
(count-pairs a)
(count-pairs b)
(count-pairs c)