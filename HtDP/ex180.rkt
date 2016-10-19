;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex180) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)


(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 


(define (editor-text s)
  (text (implode s) FONT-SIZE FONT-COLOR))

; implement editor-text without implode
(define (editor-text1 s)
  (text (implode1 s) FONT-SIZE FONT-COLOR))

(define (implode1 s)
  (cond
    ((empty? s) "")
  (else (string-append (first s) (implode1 (rest s))))))