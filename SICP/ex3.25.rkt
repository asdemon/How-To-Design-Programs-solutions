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

(define (lookup  keylist table)
  (if (null? keylist)
      (error "empty key")
      (let ((subtable
             (assoc (car keylist) (cdr table))))
        (if subtable
            (if (null? (cdr keylist))
                (cdr subtable)
                (lookup  (cdr keylist) subtable))
            false))))

(define (insert! keylist value table)
  (if (null? keylist)
      (error "empty key")
      (let ((subtable (assoc (car keylist) (cdr table))))
        (if subtable
            (if (null? (cdr keylist))
                ;最后一个key值，则subtable中为record
                (set-cdr! subtable value)
                (insert! (cdr keylist) value subtable))
            ;not exist ,create one
            (if (null? (cdr keylist))
                (set-cdr! table
                          (cons (cons (car keylist) value)
                                (cdr table)))
                (begin (set-cdr! table
                                 (cons (cons (car keylist) '())
                                       (cdr table)))
                       (insert! (cdr keylist) value (cadr table)))))
        'ok)))


(define T1 (make-table))
