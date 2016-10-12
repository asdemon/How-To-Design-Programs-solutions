;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex238) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; Nelon -> Number
; determines the smallest 
; number on l
(define (inf l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (< (first l)
            (inf (rest l)))
         (first l)
         (inf (rest l)))]))
    
; Nelon -> Number
; determines the largest 
; number on l
(define (sup l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (> (first l)
            (sup (rest l)))
         (first l)
         (sup (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(define (abstract-1 l op)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (if (op (first l)
            (abstract-1 (rest l) op))
         (first l)
         (abstract-1 (rest l) op))]))

(define (inf-1 l)
  (abstract-1 l <))
(define (sup-1 l)
  (abstract-1 l >))

; Nelon -> Number
; determines the smallest 
; number on l
(define (inf2 l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (min (first l)
         (inf2 (rest l)))]))
    
; Nelon -> Number
; determines the largest 
; number on l
(define (sup2 l)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (max (first l)
         (sup2 (rest l)))]))

; Nelon -> Number
; determines the largest 
; number on l
(define (abstract-2 l op)
  (cond
    [(empty? (rest l))
     (first l)]
    [else
     (op (first l)
         (abstract-2 (rest l) op))]))
(define (inf-2 l)
  (abstract-2 l min))
(define (sup-2 l)
  (abstract-2 l max))


(define l1 (list 25 24 23 22 21 20 19 18 17 16 15 14 13
      12 11 10 9 8 7 6 5 4 3 2 1))
(define l2 (list 1 2 3 4 5 6 7 8 9 10 11 12 13 14 15 16
      17 18 19 20 21 22 23 24 25))