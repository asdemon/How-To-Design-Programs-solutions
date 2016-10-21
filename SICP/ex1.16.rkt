;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex1.16) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (sqr (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))


(define (fast-expt-iterator b n0)
  (local (
          (define (expt/iterator b n a)
            (cond
              ((= n 0) a)
              ((even? n) (expt/iterator (sqr b) (/ n 2) a))
              (else (expt/iterator b (sub1 n) (* a b)))))
          )
    ;--------------------
    (expt/iterator b n0 1)))