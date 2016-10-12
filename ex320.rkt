;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex320) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
; An S-expr is one of: 
; – Number
; – String
; – Symbol 
; – [List-of S-expr]



; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hell) llo) 'o) 0)
(check-expect (count '(((world) hell llo) llo) 'llo) 2)
;(define (count sexp sy)
;  (match sexp
;    ((? symbol?) (if (symbol=? sexp sy) 1 0))
;    ((? number?) 0)
;    ((? string?) 0)
;    (l (match l
;         ('() 0)
;         (nl (+ (count (first l) sy)
;                (count (rest l) sy)))))))
(define (count sexp sy)
  (match sexp
    ((? symbol?) (if (symbol=? sexp sy) 1 0))
    ((? number?) 0)
    ((? string?) 0)
    (l (foldr + 0 (map (lambda (se) (count se sy)) l)) )))