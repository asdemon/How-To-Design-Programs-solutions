;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex162) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

; List-of-numbers -> List-of-numbers
; computes the weekly wages for all given weekly hours
(define (wage* whrs)
  (cond
    [(empty? whrs) '()]
    [else (cons (wage (first whrs)) (wage* (rest whrs)))]))
 
; Number -> Number
; computes the wage for h hours of work
(define (wage h)
  (* 14 h))


; List-of-numbers -> boolean
; check list of numbers 
(define (wage-check whrs)
  (cond
    ((empty? whrs) #true)
    (else (and (and (number? (first whrs)) (< (first whrs) 100))
               (wage-check (rest whrs))))))