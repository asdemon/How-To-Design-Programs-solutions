#lang sicp
(#%require (only racket/base error))

(define (rand-update x)
  (modulo  (+ 111 (* 417 x)) 4747))

(define (random-steam-generator op random-init)
  (define random-numbers
    (cons-stream
     random-init
     (stream-map
      (lambda (num op)
        (cond
          ((eqv? op 'generate) (rand-update num))
          ((eqv? op 'reset) random-init)
          (else (error "op invalid" op))))
      random-numbers
      op)))
  random-numbers)