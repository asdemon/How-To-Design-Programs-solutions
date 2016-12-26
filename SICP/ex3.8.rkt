#lang sicp
;(#%require racket/base)

(define counter 0)
(define (f x)
  (if (= 0 counter)
      (begin (set! counter (inc counter))
             x)
      0))



;(+ (f 0) (f 1))
;(+ (f 1) (f 0))
