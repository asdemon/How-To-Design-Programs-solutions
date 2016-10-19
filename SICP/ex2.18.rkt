;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.18) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; list -> list
; return the reverse of list l
; if l is empty,return '()
(check-expect (reverse1 (list 1 4 9 16 25)) '(25 16 9 4 1))
(define (reverse1 l)
  (local (
          (define (reverse/a l a)
            (if (empty? l)
                a
                (reverse/a (cdr l) (cons (car l) a))))
          )
    ;----------------
    (reverse/a l '())))

