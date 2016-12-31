#lang sicp

(define (member? x l)
  (cond
    ((null? l) #f)
    ((eq? x (car l)) #t)
    (else (member? x (cdr l)))))

(define (count-pairs l0)
  (define (count/a l count accumulator)
    (cond
      ((not (pair? l)) count)
      ((member? l accumulator) count)
      (else (+ (count/a (car l)  count (cons l accumulator))
               (count/a (cdr l)  count (cons l accumulator))







(define a '(a))
(define b `(,a a))