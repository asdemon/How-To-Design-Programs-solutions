;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.41) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; procedure -> procedure
; takes a procedure of one argument as argument and returns a
; procedure that applies the original procedure twice
(define (double f)
  (lambda (x)
    (f (f x))))


; ex1.42
(define (compose1 f g)
  (lambda (x)
    (f (g x))))

; ex1.43
(define (repeated f n)
  (if (= n 1)
      f
      (compose1 f (repeated f (sub1 n)))))

; ex1.44
(define dx 1e-5)
(define (smooth f)
  (lambda (x) (/ (+ (f (+ x dx)) (f x) (f (- x dx))) 3.0)))

; ex1.45