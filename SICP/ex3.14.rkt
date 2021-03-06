#lang sicp

(define (last l)
  (cond
    ((null? l) '())
    ((null? (cdr l)) (car l))
    (else (last (cdr l)))))

(define (append! x y)
  (set-cdr! (last-pair x) y)
  x)

(define (last-pair x)
  (if (null? (cdr x)) x (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)


;reverse order
(define (mystery x)
  (define (loop x y)
    (if (null? x)
        y
        (let ((temp (cdr x)))
          (set-cdr! x y)
          (loop temp x))))
  (loop x '()))




(define v (list 'a 'b 'c 'd))