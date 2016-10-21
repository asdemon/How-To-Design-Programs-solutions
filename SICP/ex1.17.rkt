;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex1.17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define (double x) (* 2 x))
(define (halve x) (/ x 2))

(define (fast-* a0 b0)
  (local (
          (define (mul/iterator a b acc)
            (cond
              ((= a 0) acc)
              ((even? a) (mul/iterator (halve a) (double b) acc))
              (else (mul/iterator (sub1 a) b (+ b acc)))))
          )
    ;--------------------
    (mul/iterator a0 b0 0)))


; ex 1.18
; N, N -> N
; a , b -> a*b
(define (mul/int a b)
  (cond
    ((>= a 0) (fast-* a b))
    ((>= b 0) (fast-* b a))
    (else (fast-* (- 0 a) (- 0 b)))))