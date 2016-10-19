;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2.32) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; list -> list of list
; power set of input
; there is a bug in the language advanced student
; this program only run correctly in Intermediate Student with lambda
(define (power-set s)
  (cond
    ((empty? s) '(()))
    (else (local (
                  (define re (power-set (cdr s)))
                  )
            ;---------------------
            (append re (map (lambda (x)
                              (cons (car s) x)) re))))))