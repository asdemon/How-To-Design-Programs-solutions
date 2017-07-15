#lang sicp
(#%require (only racket/base error))

;
;(define-namespace-anchor anc)
;(define ns (namespace-anchor->namespace anc))

(define no-operands? null?)
(define first-operand car)
(define rest-operands cdr)

(define (list-of-values-from-left-to-right exps env)
  (let ([first false]
        [rest false])
    
    (if (no-operands? exps)
        '()
        (begin
          (set! first (eval (first-operand exps) env))
          (set! rest (list-of-values-from-left-to-right (rest-operands exps) env))
          (cons first rest)))))


;; test
;;(define v 10)
(define expression '((begin (set! v (+ v 2)) v)
                     (begin (set! v (* v 2)) v)))
;(eval '(begin (set! v (+ v 2)) v) (interaction-environment))
;(list-of-values-from-left-to-right expression (interaction-environment)) ; => (12 24)

;;(set! v 10)
;(list-of-values-right-to-left expression (interaction-environment)) ; => (20 22)

(define (p f v)
    (begin
      (eval '(begin (set! v (f v 2)) v) (null-environment 5))))
