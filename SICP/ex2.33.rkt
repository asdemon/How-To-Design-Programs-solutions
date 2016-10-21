;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.33) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (map1 p sequence)
  (accumulate (lambda (x y) (cons (p x) y)) '() sequence))
(define (append1 seq1 seq2)
  (accumulate cons seq2 seq1))
(define (length1 sequence)
  (accumulate (lambda (x y) (add1 y)) 0 sequence))