#lang sicp
;(#%require (prefix-in racket:racket))


(define error display)

(define (make-deque)
  (let ((front-ptr '())
        (rear-ptr '()))
        ;(define (front-ptr) front-ptr)
        ;(define (rear-ptr) rear-ptr)
    (define (set-front-ptr! item)
      (set! front-ptr item))
    (define (set-rear-ptr! item)
      (set! rear-ptr item))
    
    (define (empty-queue?)
      (null? front-ptr))
    
    (define (front-queue)
      (if (empty-queue?)
          (error "FRONT called with an empty queue")
          (cadr front-ptr)))
    
    (define (rear-deque)
      (if (empty-queue?)
          (error "REAR called with an empty queue")
          (cadr rear-ptr)))
    
    (define (rear-deque-car)
      (if (empty-queue?)
          (error "rear-deque-car called with an empty queue")
          (car rear-ptr)))

    (define (front-deque-caddr)
      (if (empty-queue?)
          (error "front-deque-caddr called with an empty queue")
          (caddr front-ptr)))

    (define (front-deque-cddr)
      (if (empty-queue?)
          (error "front-deque-cddr called with an empty queue")
          (cddr front-ptr)))
    
    
    (define (front-insert-deque! item)
      (let ((new-pair (cons '() (cons item '()))))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! (cdr new-pair) front-ptr)
               (set-car! front-ptr new-pair)
               (set-front-ptr! new-pair)
               dispatch))))
    
    (define (rear-insert-deque! item)
      (let ((new-pair (cons '() (cons item '()))))
        (cond ((empty-queue?)
               (set-front-ptr! new-pair)
               (set-rear-ptr! new-pair)
               dispatch)
              (else
               (set-cdr! (cdr rear-ptr) new-pair)
               (set-car! new-pair rear-ptr)
               (set-rear-ptr! new-pair)
               dispatch))))
    
    
    (define (front-delete-deque!)
      (cond ((empty-queue?)
             (display "FRONT-DELETE! called with an empty queue"))
            (else
             (let ((next (cddr front-ptr)))
               (set-cdr! (cdr front-ptr) '())
               (set-car! next '())
               (set-front-ptr! next)
               next))))
    
    (define (rear-delete-deque!)
      (cond ((empty-queue?)
             (display "REAR-DELETE! called with an empty queue"))
            (else
             (let ((prev (car rear-ptr)))
               (set-car!  rear-ptr '())
               (set-cdr! (cdr prev) '())
               (set-rear-ptr! prev)
               dispatch))))
    
    (define (print-queue)
      (define (content q)
        (if (null? q)
            '()
            (cons (cadr q) (content (cddr q)))))
      (content front-ptr))
    
    (define (dispatch m)
      (cond
        ((eqv? m 'front-queue) (front-queue))
        ((eqv? m 'rear-deque) (rear-deque))
        ((eqv? m 'rear-deque-car) (rear-deque-car))
        ((eqv? m 'front-deque-caddr) (front-deque-caddr))
        ((eqv? m 'front-deque-cddr) (front-deque-cddr))
        ((eqv? m 'front-ptr) front-ptr)
        ((eqv? m 'front-delete-deque!) (front-delete-deque!))
        ((eqv? m 'rear-insert-deque!) rear-insert-deque!)
        ((eqv? m 'front-insert-deque!) front-insert-deque!)
        ((eqv? m 'rear-delete-deque!) (rear-delete-deque!))
        ((eqv? m 'empty-queue?) (empty-queue?))
        ((eqv? m 'print-queue) (print-queue))
        (else (error "A wrong operation on queue"))))
    dispatch))



(define q1 (make-deque))
((q1 'rear-insert-deque!) 'b)
((q1 'front-insert-deque!) 'a)
((q1 'front-insert-deque!) '1)
((q1 'front-insert-deque!) '2)
(q1 'front-queue)
(q1 'rear-deque)

(q1 'print-queue)
(q1 'rear-delete-deque!)
(q1 'print-queue)
(q1 'front-ptr)
(q1 'front-deque-cddr)
(q1 'front-deque-caddr)
(q1 'front-delete-deque!)
(q1 'front-delete-deque!)
(q1 'print-queue)