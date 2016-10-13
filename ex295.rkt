;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex295) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; distances in terms of pixels 
(define WIDTH 300)
(define HEIGHT 300)
 
; N -> [List-of Posn]
; generate n random Posns in [0,WIDTH) by [0,HEIGHT)
(check-satisfied (random-posns 3)
                 (n-inside-playground? 3))
(define (random-posns n)
  (build-list
    n
    (lambda (i)
      (make-posn (random WIDTH) (random HEIGHT)))))


(define (n-inside-playground? n)
  (lambda (r0)
    (and
     (= (length r0) n)
     (andmap (lambda (p)
               (and (< (posn-x p) WIDTH)
                    (< (posn-y p) HEIGHT))) r0)))
        )

(define (p1 x y)
  (+ (* x y)
     (+ (* 2 x)
        (+ (* 2 y) 22))))
 
(define (p2 x)
  (+ (* 55 x) (+ x 11)))
 
(define (p3 x)
  (+ (p1 x 0)
     (+ (p1 x 1) (p2 x))))

