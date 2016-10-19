;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.17) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))


; list -> member
; return the last member of list l
; if l is empty,return '()
(check-expect (last-pair (list 23 72 149 34)) 34)
(define (last-pair l)
  (local (
          (define (last/a l a)
            (if (empty? l)
                a
                (last/a (cdr l) (car l))))
          )
    ;----------------
    (last/a l '())))

