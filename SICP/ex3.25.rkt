#lang sicp
(#%require (only racket/base error))

;I have an idea:have a key of a list and associate it with a value, this is actually a one-dimensional table
; For exercise purpose, I recurivelly implement it.


(define (make-table)
  (list '*table*))

(define (assoc key records)
  (cond ((null? records) false)
        ((equal? key (caar records)) (car records))
        (else (assoc key (cdr records)))))

(define (lookup table . key)
  (if (null? key)
      (error "empty key")
      (let ((subtable
             (assoc (car key) (cdr table))))
        (if subtable
            (if (null? (cdr key))
                (cadr subtable)
                (lookup subtable (cdr key)))
                false))))

(define (insert! value table . key)
  (if (null? key)
      (error "empty key")
  (let ((subtable (assoc (car key) (cdr table))))
    (if subtable
        (insert! value table (cdr key))
        ;not exist ,create one
        (let ((record (assoc key-2 (cdr subtable))))
          (if record
              (set-cdr! record value)
              (set-cdr! subtable
                        (cons (cons key-2 value)
                              (cdr subtable)))))
        (set-cdr! table
                  (cons (list key-1
                              (cons key-2 value))
                        (cdr table)))))
  'ok)


(define T1 (make-table))
