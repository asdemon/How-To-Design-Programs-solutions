;and
(define (and? exp) (tagged-list? exp 'and))
(define (operands exp) (cdr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))
(define (eval-and ops)
  (if (true? (eval (first-operand ops)))
      (eval-and (rest-operands ops))
      #f))


;and
(define (or exp) (tagged-list? exp 'or))
;(define (operands exp) (cdr exp))
;(define (no-operands? ops) (null? ops))
;(define (first-operand ops) (car ops))
;(define (rest-operands ops) (cdr ops))
(define (eval-or ops)
  (if (true? (eval (first-operand ops)))
      #t
      (eval-and (rest-operands ops))))