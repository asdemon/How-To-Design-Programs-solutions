#lang sicp
(#%require racket/base)

(define (memv x l)
  (cond
    ((null? l) #f)
    ((eqv? x (car l)) #t)
    (else (memv x (cdr l)))))


(define (make-account balance password)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin (set! balance (- balance amount))
               balance)
        "Insufficient funds"))
  (define (deposit amount)
    (begin
      (set! balance (+ balance amount))
      balance))
  (let ((passwd-list `(,password)))
    (define (dispatch p m)
      (if (memv p passwd-list)
          (cond ((eq? m 'withdraw) withdraw)
                ((eq? m 'deposit) deposit)
                ((eq? m 'make-joint) (λ (new-passwd) (set! passwd-list (cons new-passwd passwd-list))))
                (else (error "Unknown request: MAKE-ACCOUNT"
                             m)))
          (error "Incorrect password")))
    dispatch))



(define (make-joint acc old-passwd new-password)
  (begin ((acc old-passwd 'make-joint) new-password)
         acc)
  )





(define peter-acc (make-account 100 'open-sesame))
(define paul-acc
  (make-joint peter-acc 'open-sesame 'rosebud))