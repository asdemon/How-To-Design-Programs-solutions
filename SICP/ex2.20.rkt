;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.20) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(check-expect (same-parity '(1 2 3 4 5 6 7)) '(1 3 5 7))
(check-expect (same-parity  '(2 3 4 5 6 7)) '(2 4 6))
(define same-parity
 (lambda (z) (filter (lambda (x) (= 0 (modulo (- x (car z)) 2))) z)))