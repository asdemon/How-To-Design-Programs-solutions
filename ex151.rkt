;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex151) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;Number Number -> Number
; multyply two number without *
(check-expect (multyply 3 3) 9)
(check-expect (multyply 3 0) 0)
(check-expect (multyply 0 3) 0)

(define (multyply m n)
  (cond
    ((= n 0) 0)
    ((positive?  n) (+ m (multyply m (sub1 n))))))

