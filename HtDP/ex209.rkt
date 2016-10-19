;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex209) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String -> Word
; convert s to the chosen word representation
(check-expect (string->word "ab")
              (list "a" "b"))
(check-expect (string->word "")
              '())
(define (string->word s)
  (explode s))
 
; Word -> String
; convert w to a string
(define (word->string w)
  (implode w))