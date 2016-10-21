;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.22) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(require 2htdp/abstraction)

(define (runtime) (/ (current-milliseconds) 1000.0))

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (sqr test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))
(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))

(define (timed-prime-test n)
  (begin
    (newline)
    (display n)
    (start-prime-test n (runtime))))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) start-time))
      (newline)))

(define (report-prime elapsed-time)
  (begin
    (display " *** ")
    (display elapsed-time)))

(define (search-for-primes n1 n2)
  (local (
          (define (search-for-primes/a n1 n2 cur)
            (if (<= cur n2)
                (begin (timed-prime-test cur)
                       (search-for-primes/a n1 n2 (+ 2 cur)))
                (newline)))
          )
    ;-----------
    (search-for-primes/a n1 n2 n1)))


; ex1.23
;To implement this change, define a procedure next that returns
;3 if its input is equal to 2 and otherwise returns its input
;plus 2.
(define (next n)
  (if (= n 2)
      3
      (+ n 2)))