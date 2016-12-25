#lang sicp
(#%require racket/base)
;(#%require lang/htdp-advanced)

(define (rand-update x)
  (modulo  (+ 111 (* 417 x)) 4747))

(define rand
  (let ((x (runtime)))
               (lambda (op)
                 (cond
                   ((eqv? op 'generate) (begin (set! x (rand-update x))
                                               x))
                   ((eqv? op 'reset) (lambda (init) (set! x init)))
                   (else (error "wrong operation"))))))