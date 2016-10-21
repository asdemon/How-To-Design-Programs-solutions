;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex1.12) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)


; list of something -> list of something
; remove last element
(check-expect (remove-last (list "a" "b" "c"))
              (list "a" "b"))
(check-expect (remove-last (list "a"))
              '())
(check-expect (remove-last '())
              '())
(define (remove-last l)
  (cond
    ((empty? l) '())
    ((empty? (rest l)) '())
    (else (cons (first l)
                (remove-last (rest l))))))


;N -> list of N
;yang hui triangle'N row
(define (yang-hui-triangle n)
  (cond
    ((= n 0) '(1))
    ((= n 1) '(1 1))
    (else (local (
                  (define last-row (yang-hui-triangle (sub1 n)))
                  (define result-except-ends (for/list ((i (cdr last-row)) (j (remove-last last-row)))
                                               (+ i j)))
                  )
            ;----------------------
            (append (cons 1 result-except-ends) '(1))))))