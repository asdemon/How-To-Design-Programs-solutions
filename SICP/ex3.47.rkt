#lang sicp
(#%require (only racket/base error build-list))

; there is an efficient way than this, but I know it after I did this
(define (make-semaphore n)
  (let ((cell (list (build-list n (lambda (x) false)))))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set! cell)
                 (the-mutex 'acquire))) ; retry
            ((eq? m 'release) (clear! cell))))
    the-mutex))
(define (clear! cell) (set-car! cell false))
(define (test-and-set! cell)
  (if (apply and cell) true (begin (set-first-false! cell true) false)))
(define (set-first-false! cell true)
  (cond
    ((not (car cell)) (set-car! cell true))
    (else (set-first-false! (cdr cell) true))))

(define (make-semaphore n)
  (let ((mutexes (build-list n (lambda (x) (make-mutex)))))
    (define (acquire list)
      (cond
        ;no more mutex
        ((null? list) (dispath 'acquire))
        ;not acquire a mutex
        (((car mutexes) 'test-and-set!) (acquire (cdr list)))
        ;acqurie a mutex, do nothing
        ))
    (define (dispath op)
      (cond
        ((eqv? op 'acquire) (acquire mutexes))
        ((eq? m 'release) (clear! mutexes))))
    dispath))
