;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex345) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


(define-struct add [left right])
(define-struct mul [left right])

; ex345
; An BSL-expr  is one of: 
; – Atom
; – (make-add atom atom)
; – (make-mul atom atom)

; An Atom is one of: 
; – Number
; – add
; – mul 

; ex347
; BSL-expr -> boolean
; is BSL-expr atom?
(check-expect (atom? 0) #t)
(check-expect (atom? (make-add 0 1)) #t)
(check-expect (atom? (make-add (make-mul 1 3) 1)) #f)
(define (atom? e)
  (match e
    ((? number?) #t)
    ((add left right) (and (number? left)
                           (number? right)))
    ((mul left right) (and (number? left)
                           (number? right)))))

;  representation -> N
; eval the  representation's value
(check-expect (eval-expression 3) 3)
(check-expect (eval-expression (make-add 1 1)) 2)
(check-expect (eval-expression (make-mul 3 10)) 30)
(check-expect (eval-expression (make-add (make-mul 1 1) 10)) 11)
(define (eval-expression re)
  (cond
    ((atom? re) (match re
                  ((? number?) re)
                  ((add left right) (+ left right))
                  ((mul left right) (* left right))))
    (else (match re
                  ((add left right) (+ (eval-expression left) (eval-expression right)))
                  ((mul left right) (* (eval-expression left) (eval-expression right)))))))
                