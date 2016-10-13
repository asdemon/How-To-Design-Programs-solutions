;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex492) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (relative->absolute l)
 (reverse
   (foldr (lambda (x b) (cons (+ x (first b)) b))
          (list (first l))
          (reverse (rest l)))))
