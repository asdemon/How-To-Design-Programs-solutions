;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.27) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; list -> list
; reverse every sublist in l and l
(define x (list (list 1 2) (list 3 4)))
(check-expect (deep-reverse x)
              '((4 3) (2 1)))
(define (deep-reverse l)
  (if (cons? l)
      (map deep-reverse (reverse l))
      l))