;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.45) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (average-damp f)
  (lambda (x) (/ (+ x (f x)) 2.0)))
(define tolerance 0.1)

(define (nth-root x n)
  (local (
          (define (f y)
            (/ x (expt y (sub1 n))))
          )
    ;---------------------------------
  ((iterative-improve (lambda (x) (< (abs (- x (f x))) tolerance))
                      ((repeated average-damp (floor (/ (log n) (log 2)))) f)) 2.0)))


(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (sub1 n)))))


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

(nth-root 2187 7)