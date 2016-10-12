;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex247) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

(define (extract R l t)
  (cond
    [(empty? l) '()]
    [else (cond
            [(R (first l) t)
             (cons (first l)
                   (extract R (rest l) t))]
            [else
             (extract R (rest l) t)])]))
; ex 247
;(extract < (cons 8 (cons 4 '())) 5)



; Number Number -> Boolean
; is the area of a square with side x larger than c
(define (squared>? x c)
  (> (* x x) c))

; ex248
(squared>? 3 10)
(squared>? 4 10)
(extract squared>? (list 3 4 5) 10)

; ex249
(define (f x) x)
(cons f '())
(f f)
(cons f (cons 10 (cons (f 10) '())))
