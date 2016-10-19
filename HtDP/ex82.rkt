;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex82) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define-struct word (first secend third))
; A word is a structure:
;   (make-word com com com)
;com is
; 1String 'a'~’z'
;or #fasle



;word word -> word or #false
;The function retains the content of the structure fields
;if the two agree; otherwise it places #false in the field
;of the resulting word.

(define (compare-word w1 w2)
  (if (and (equal? (word-first w1) (word-first w2))
           (equal? (word-secend w1) (word-secend w2))
           (equal? (word-third w1) (word-third w2)))
      w1
      #false))
