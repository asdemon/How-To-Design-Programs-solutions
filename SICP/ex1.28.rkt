;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.28) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/abstraction)

(define (runtime) (/ (current-milliseconds) 1000.0))
(define (square n) (sqr n))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (local (
                 (define re (expmod base (/ exp 2) m))
                 )
           ;---------------
           (if (or (= re 1)
                   (= re (sub1 m)))
               0
               (remainder
                (square (expmod base (/ exp 2) m))
                m))))
        (else
         (remainder
          (* base (expmod base (- exp 1) m))
          m))))



;N -> bool
;test if the input is a prime or not
(define (miller-rabin-test n)
  (local (
          (define (try-it a)
            (= (expmod a (sub1 n) n) 1))
          )
    ;---------------------------
    (try-it (+ 1 (random (- n 1))))))


(define (fast-prime? n times)
  (cond ((= times 0) true)
        ((miller-rabin-test n) (fast-prime? n (- times 1)))
        (else false)))