;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex175) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

; string -> number number number
; count the number of 1Strings, words, and lines in a given file
(define (wc filename)
    (list (count-1string (read-words/line filename))
          (count-word (read-words/line filename))
          (length (read-words/line filename))
  ))

; list of list string -> number
; 计算list of list string的字符数
(define (count-1string l)
  (cond
    ((empty? l) 0)
    (else (+ (count-1string-line (first l))
             (count-1string (rest l))))))
; list string -> number
; 计算 list string中的字符数
(check-expect (count-1string-line (list "ddd")) 3)
(define (count-1string-line l)
  (cond
    ((empty? l) 0)
    (else (+ (length (explode (first l)))
             (count-1string-line (rest l))))))


; list of list string -> number
; 计算list of list string的word数
(check-expect (count-word (list (list "ddd"))) 1)
(check-expect (count-word (list '())) 0)
(define (count-word l)
  (cond
    ((empty? l) 0)
    (else (+ (length (first l))
             (count-word (rest l))))))