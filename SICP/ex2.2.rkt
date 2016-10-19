;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.2) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
; Point
;use posn

; x of point
(define (x-point p)
  (posn-x p))

; y of point
(define (x-point p)
  (posn-y p))



;line segment
; end-segment is a point
; start-segment is a point
(define-struct segment [start-segment end-segment])


; segment -> Point
; return the mid-point of line segment seg
(define (midpoint-segment seg)
  (make-posn (/ (+ (posn-x (start-segment seg)) (posn-x (end-segment seg))) 2.0)
             (/ (+ (posn-y (start-segment seg)) (posn-y (end-segment seg))) 2.0)))








                


(define (print-point p)
  (begin (newline)
         (display "(")
         (display (x-point p))
         (display ",")
         (display (y-point p))
         (display ")")))