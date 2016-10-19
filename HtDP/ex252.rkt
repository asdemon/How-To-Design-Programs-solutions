;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex252) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/image)


(define EPSILODE 1e-4)

; Func,  Number -> [List-of Number]
; 
(define (fold2 f l initValue)
   (cond
    [(empty? l) initValue]
    [else
     (f (first l)
        (fold2 f (rest l) initValue))]))


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
  
; [List-of Posn] -> Image
(define (image* l)
  (cond
    [(empty? l) emt]
    [else
     (place-dot
      (first l)
      (image* (rest l)))]))
 
; Posn Image -> Image 
(define (place-dot p img)
  (place-image
     dot
     (posn-x p) (posn-y p)
     img))
 
; graphical constants:    
(define emt
  (empty-scene 100 100))
(define dot
  (circle 3 "solid" "red"))




(define (sum-abstract l)
  (fold2 + l 0))
(define (product-abstract l)
  (fold2 * l 1))
(define (image*-abstract l)
  (fold2 place-dot l emt))


(check-expect  (image*-abstract (list (make-posn 1 2) (make-posn 3 4) (make-posn 5 6)))
              (image* (list (make-posn 1 2) (make-posn 3 4) (make-posn 5 6))) )
(check-expect  (image*-abstract '())
              (image* '()) )
(check-within  (product-abstract (list 1 2 3 4 5))
              (product (list 1 2 3 4 5)) EPSILODE)
(check-within  (product-abstract '())
              (product '()) EPSILODE)