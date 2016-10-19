;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex172) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

; list-of-lines -> string
; converts a list of lines into a string
(check-expect (collapse (list (list "line 0") '() (list "line 2")))
              "line 0\n\nline 2\n")
(define (collapse list1)
  (cond
    ((empty? list1) "")
    (else (string-append (cat (first list1)) (collapse (rest list1))))))
; list-of-strings -> string
; converts a list of strings into a string
(check-expect (cat (list "line 0" "line 2"))
              "line 0 line 2\n")
(define (cat line)
  (cond
    ((empty? line) "\n")
    ((empty? (rest line)) (string-append (first line) "\n"))
    (else (string-append (first line) " " (cat (rest line))))))