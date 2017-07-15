#lang sicp

; 1. because the implement of application? is pair? , it always thinks the define is an application
; so it can't define var



; 2. it is enough to change the abstact function layer
(define (application? exp) (tagged-list? exp 'call))
(define (operator exp) (cadr exp))
(define (operands exp) (cddr exp))
(define (no-operands? ops) (null? ops))
(define (first-operand ops) (car ops))
(define (rest-operands ops) (cdr ops))