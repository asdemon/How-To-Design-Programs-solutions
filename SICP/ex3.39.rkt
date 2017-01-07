#lang sicp
(#%require (only racket/base error))
(#%require (only racket/base thread-wait thread))


(define (parallel-execute . procs)
  (map thread-wait
       (map (lambda (proc) (thread proc))
            procs)))


(define x 10)
(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (+ x 1))))


;ex3.39
;121 100 11 101

;ex3.41
;NO, read concurrency is safe.We should verify when it is written,
;it can not be read.
