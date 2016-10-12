;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex497) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define QUEENS 8)
; A QP is a structure:
;   (make-posn CI CI)
; A CI is an N in [0,QUEENS).
; interpretation (make-posn r c) denotes the square at 
; the r-th row and c-th column


; ex479
; QP, QP -> boolean
(define (threatening? q1 q2)
  (or (= (posn-x q1) (posn-x q2))
      (= (posn-y q1) (posn-y q2))
      (= (abs (/ (- (posn-y q2) (posn-y q1)) (- (posn-x q2) (posn-x q1)))) 1)))


; N -> [Maybe [List-of QP]]
; find a solution to the n queens problem 
 
; data example: [List-of QP]
(define 4QUEEN-SOLUTION-2
  (list  (make-posn 0 2) (make-posn 1 0)
         (make-posn 2 3) (make-posn 3 1)))
 
(define (n-queens n)
  #false)