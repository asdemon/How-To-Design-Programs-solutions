#lang sicp

; ex4.6
(define (let? exp) (tagged-list? exp 'let))
(define (let-parameters exp) (map car (cadr exp)))
(define (let-parameters-val exp) (map cadr (cadr exp)))
(define (let-body exp) (cddr exp))
(define (make-let parameters-and-values-list body)
  (cons 'let (cons parameters-and-values-list body)))

(define (let->combination exp)
  (cons (make-lambda (let-parameters exp) (let-body exp))
        (let-parameters-val exp)))


(define (eval exp env)
  (cond ((self-evaluating? exp) exp)
        ((variable? exp) (lookup-variable-value exp env))
        ((quoted? exp) (text-of-quotation exp))
        ((assignment? exp) (eval-assignment exp env))
        ((definition? exp) (eval-definition exp env))
        ((if? exp) (eval-if exp env))
        ((lambda? exp) (make-procedure (lambda-parameters exp)
                                       (lambda-body exp)
                                       env))
        ((begin? exp)
         (eval-sequence (begin-actions exp) env))
        ((cond? exp) (eval (cond->if exp) env))
        ((let? exp) (eval (let->combination exp)))
        ((application? exp)
         (apply (eval (operator exp) env)
                (list-of-values (operands exp) env)))
        (else
         (error "Unknown expression type: EVAL" exp))))




; ex4.7
(define (let*-parameters-and-val-list exp) (cadr exp))
(define (last-exp? seq) (null? (cdr seq)))
(define (first-exp seq) (car seq))
(define (rest-exps seq) (cdr seq))
(define (let*-body exp) (cddr exp))
(define (make-let* parameters-and-values-list body)
  (cons 'let* (cons parameters-and-values-list body)))
(define (let*->nested-lets exp)
  (if (last-exp? (let*-parameters-and-val-list exp))
      (make-let (let*-parameters-and-val-list exp) (let*-body exp))
      (make-let (list (first-exp (let*-parameters-and-val-list exp)))
                (let*->nested-lets (make-let* (rest-exps (let*-parameters-and-val-list exp)) (let*-body exp))))))
;as to (eval (let*->nested-lets exp) env), it is sufficient to add a clause. the eval and apply will add
;environment for us




; ex4.8

(define (let->combination exp)
  (if (list? (cadr exp))
      (cons (make-lambda (let-parameters exp) (let-body exp))
            (let-parameters-val exp))
      (cons (make-lambda (map car (caddr exp)) (cdddr exp))
            (map cadr (caddr exp)))))