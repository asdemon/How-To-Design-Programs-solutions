#lang sicp

(define (cdr/self l)
  (if (pair? l)
      (cdr l)
      #f))

(define (car/self l)
  (if (pair? l)
      (car l)
      #f))

(define (cycle? l0)
  (let* ((step1 (cdr/self l0))
         (step2 (cdr/self step1)))
   (define (cycle?/a)
     (cond
       ((or (boolean? step1) (boolean? step2)) #f)
       ((eq? step1 step2) #t)
       (else (begin (set! step1 (cdr/self step1))
                    (set! step2 (cdr/self (cdr/self step2)))
                    (cycle?/a)))))
  (cycle?/a)))
     







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