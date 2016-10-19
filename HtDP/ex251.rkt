;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex251) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define EPSILODE 1e-4)

; Func,  Number -> [List-of Number]
; 
(define (fold1 f l initValue)
   (cond
    [(empty? l) initValue]
    [else
     (f (first l)
        (fold1 f (rest l) initValue))]))


; [List-of Number] -> Number
; computes the sum of 
; the numbers on l
(define (sum l)
  (cond
    [(empty? l) 0]
    [else
     (+ (first l)
        (sum (rest l)))]))
  
; [List-of Number] -> Number
; computes the product of 
; the numbers on l
(define (product l)
  (cond
    [(empty? l) 1]
    [else
     (* (first l)
        (product (rest l)))]))



(define (sum-abstract l)
  (fold1 + l 0))
(define (product-abstract l)
  (fold1 * l 1))


(check-within  (sum-abstract (list 1 2 3 4 5))
              (sum (list 1 2 3 4 5)) EPSILODE)
(check-within  (sum-abstract '())
              (sum '()) EPSILODE)
(check-within  (product-abstract (list 1 2 3 4 5))
              (product (list 1 2 3 4 5)) EPSILODE)
(check-within  (product-abstract '())
              (product '()) EPSILODE)