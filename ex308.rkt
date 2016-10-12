;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex308) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct phone [area switch four])
; A Phone is a structure: 
;   (make-phone Three Three Four)
; A Three is a Number between 100 and 999. 
; A Four is a Number between 1000 and 9999.


(define (replace l)
  (match l
    ['() '()]
    [(cons (phone 713 switch four) tail) (cons (make-phone 218 switch four) tail)]
    [(cons head tail) (cons head (replace tail))]))


(check-expect (replace '()) '())
(check-expect (replace (list (make-phone 123 333 4444))) (list (make-phone 123 333 4444)))
(check-expect (replace (list (make-phone 713 333 4444))) (list (make-phone 218 333 4444)))
(check-expect (replace (list (make-phone 123 333 4444) (make-phone 713 333 4444)))
              (list (make-phone 123 333 4444) (make-phone 218 333 4444)))