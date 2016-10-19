;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.31) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (product term a next b)
  (local (
          (define (product/a term a next b acc)
            (if (> a b)
                acc
                (product/a term (next a) next b (* (term a) acc))))
          )
    ;----------------------
    (product/a term a next b 1)))

(define (factorial n)
  (product (lambda (x) x) 1 add1 n))

(define (sim-pi/4)
  (product (lambda (n) (/ (* 2 n 2 (add1 n)) (sqr (+ (* 2 n) 1)))) 1 add1 100))

(define (f g) (g 2))