;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex331) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; A Dir.v1 (short for directory) is one of: 
; – '()
; – (cons File.v1 Dir.v1)
; – (cons Dir.v1 Dir.v1)
 
; A File.v1 is a String.


; ex331
; Dir.v1 -> N
; how many files a given Dir.v1 contains.
(define (how-many d1)
  (match d1
    ('() 0)
    ((cons head tail) (if (string? head)
                          (+ 1 (how-many tail))
                          (+ (how-many head) (how-many tail))))))



(define-struct dir [name content])
; A Dir.v2 is a structure: 
;   (make-dir String LOFD)
 
; A LOFD (short for list of files and directories) is one of:
; – '()
; – (cons File.v2 LOFD)
; – (cons Dir.v2 LOFD)
 
; A File.v2 is a String.


; ex333
; Dir.v2 -> N
; how many files a given Dir.v1 contains.
(check-expect (how-many.v2 (make-dir "TS" '())) 0)
(check-expect (how-many.v2 (make-dir "TS"
                                  `(,(make-dir "sub1" '())))) 0)
(check-expect (how-many.v2 (make-dir "TS"
                                  `(,(make-dir "sub1" '()) "dd"))) 1)
(check-expect (how-many.v2 (make-dir "TS"
                                  `(,(make-dir "sub1" '("ab" "cd")) "dd"))) 3)
(define (how-many.v2 d1)
  (local
    (;LOFD -> N
     ; how many files
     (define (h-m l)
       (match l
         ('() 0)
         ((cons head tail) (if (string? head)
                               (+ 1 (h-m tail))
                               (+ (h-m (dir-content head)) (h-m tail)))))))
    ;--IN---
    (h-m (dir-content d1))))




(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)
(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

; ex335
(check-expect (how-many.v3 (make-dir.v3 "TS"
                                  `(,(make-dir.v3 "sub1" '() '("ab" "cd")))
                                  '())) 2)
(check-expect (how-many.v3 (make-dir.v3 "TS"
                                  `(,(make-dir.v3 "sub1" '() '()))
                                  '())) 0)
(check-expect (how-many.v3 (make-dir.v3 "TS"
                                  `(,(make-dir.v3 "sub1" '() '()))
                                  '("ab" "cd"))) 2)
(define (how-many.v3 d3)
  (+ (foldr + 0 (map how-many.v3 (dir.v3-dirs d3))) (length (dir.v3-files d3))))