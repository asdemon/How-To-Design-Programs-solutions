;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.46) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; procedure, procedure -> procedure
; it take two procedure as input ,a method for telling whether a guess is good
; enough and a method for improving a guess, and return a procedure that takes a
; guess as argument  and keeps improving the guess until it is
; good enough
(define (iterative-improve good-enough improve)
  (local (
          (define (result x)
            (if (good-enough x)
                x
                (result (improve x))))
          )
    ;--------------------
    result))

(define (our-sqrt x)
  (local (
          (define (improve y)
            (/ (+ y (/ x y)) 2.0))
          )
    ;---------------------
   ((iterative-improve (lambda (x) (< (/ (abs (- x (improve x))) (abs x)) 0.001))
                      improve) x)))


(define tolerance 0.00001)
(define (our-fixed-point f first-guess)
  ((iterative-improve (lambda (x) (< (abs (- x (f x))) tolerance))
                      f) first-guess))
                 