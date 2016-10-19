;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex173) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)

(define ARTICLE (list "a" "an" "the"))

; list-of-lines -> list-of-lines
;removes all articles 
(check-expect (remove-ac (list (list "a") '() (list "line 2")))
              (list '() '() (list "line 2")) )
(define (remove-ac list1)
  (cond
    ((empty? list1) '())
    (else (cons (remove-ful (first list1)) (remove-ac (rest list1))))))
; list-of-strings -> list
;remove the articles in list
(check-expect (remove-ful (list "a" "line 2"))
              (list "line 2"))
(check-expect (remove-ful (list "a"))
              '())
(check-expect (remove-ful '())
              '())
(define (remove-ful line)
  (cond
    ((empty? line) '())
    (else (if (in? (first line) ARTICLE)  (remove-ful (rest line)) (cons (first line) (remove-ful (rest line)))))))

(define (in? ele l)
  (cond
    ((empty? l) #false)
    (else (or (equal? ele (first l))
        (in? ele (rest l))))))


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