#lang sicp

(define (make-accumulator init)
  (lambda (acc)
    (begin
      (set! init (+ init acc))
      init)))

(define A (make-accumulator 0))
(define B (make-accumulator 1))
