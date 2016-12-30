#lang sicp
;(#%require (prefix-in racket:racket))


(define error display)

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
;    (define (front-ptr) front-ptr)
;    (define (rear-ptr) rear-ptr)
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    
    (define (empty-queue?)
      (null? front-ptr))
    
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (car front-ptr)))
    
    
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! rear-ptr new-pair)
               (set-rear-ptr! new-pair)
               dispatch))))
    
    
    (define (delete-queue!)
      (cond ((empty-queue?)
             (display "DELETE! called with an empty queue"))
            (else (set-front-ptr! (cdr front-ptr))
                  dispatch)))
    
    (define (print-queue)
      front-ptr)
    (define (dispatch m)
      (cond
        ((eqv? m 'front-queue) (front-queue))
        ((eqv? m 'insert-queue!) insert-queue!)
        ((eqv? m 'delete-queue!) (delete-queue!))
        ((eqv? m 'empty-queue?) (empty-queue?))
        ((eqv? m 'print-queue) (print-queue))
        (else (error "A wrong operation on queue"))))
    dispatch))