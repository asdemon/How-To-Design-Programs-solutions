#lang sicp

(define (make-monitored f)
  (let ((counter 0))
    (lambda (x)
      (cond
        ((eqv? x 'how-many-calls?) counter)
        ((eqv? x 'reset-count) (set! counter 0))
        (else (begin 
                (set! counter (+ counter 1))
                (f x)))))))
