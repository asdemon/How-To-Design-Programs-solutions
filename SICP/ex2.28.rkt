;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.28) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; list -> list
; reverse every sublist in l and l
(define x (list (list 1 2) (list 3 4)))
(check-expect (fringe x)
              '(1 2 3 4))
(define (fringe l)
  (cond
    ((empty? l) l)
    ((cons? l)  (apply append (map fringe l)))
    (else `(,l))))