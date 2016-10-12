;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex452) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; File -> [List-of Line]
; converts a file into a list of lines 

(check-expect (file->list-of-lines
               (list "a" "b" "c" "\n"
                     "d" "e" "\n"
                     "f" "g" "h" "\n"))
              (list (list "a" "b" "c")
                    (list "d" "e")
                    (list "f" "g" "h")))

(define (file->list-of-lines afile)
  (cond
    ((empty? afile) '())
    (else (cons (list-to-first-n afile) (file->list-of-lines (remainder-of-first-n afile))))))


; File -> [List-of words]
; return file's first line , represent by a list of string
(check-expect (list-to-first-n
               (list "a" "b" "c" "\n"
                     "d" "e" "\n"
                     "f" "g" "h" "\n"))
              (list "a" "b" "c"))
(define (list-to-first-n afile)
  (local
    ((define find-re (find afile "\n")))
    ;=---------------
    (if (boolean? find-re)
        afile
        (take afile find-re))))


(define (remainder-of-first-n afile)
  (local
    ((define find-re (find afile "\n")))
    ;=---------------
    (if (boolean? find-re)
        '()
        (drop afile (+ 1 find-re)))))



; [List-of 1String] N -> [List-of String]
; bundles chunks of s into strings of length n
; idea take n items and drop n at a time
(define (bundle s n)
  (cond
    [(empty? s) '()]
    [else
     (cons (implode (take s n)) (bundle (drop s n) n))]))

; [List-of X] N -> [List-of X]
; keep the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; remove the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
; 在l中查找s，如果存在，返回第一个的索引，如果不存在，返回false
(define (find l s)
  (cond
    ((empty? l) #f)
    (else (if (equal? s (car l))
              0
              (local ((define re (find (cdr l) s)))
                ;------------------
                (if (boolean? re)
                    #f
                    (+ 1 re)))))))

; 在l中查找white-space，如果存在，返回第一个的索引，如果不存在，返回false
(define (find-white-space l)
  (cond
    ((empty? l) #f)
    (else (if (string-whitespace? (car l))
              0
              (local ((define re (find-white-space (cdr l))))
                ;------------------
                (if (boolean? re)
                    #f
                    (+ 1 re)))))))

; ex453
; Line -> [List-of tokenizes]
; converts a line into a list of token  

(check-expect (tokenize
               (list "a" "b" " " "c" "\n"))
              (list "ab" "c"))

(define (tokenize aline)
  (local
    (
     (define (f aline)
       (cond
         ((empty? aline) '())
         (else (cons (list-to-first-white aline) (f (remainder-of-first-white aline)))))))
    ;-----------------
    (filter (lambda (x) (not (string=? x ""))) (f aline))))
     
     
     ; Line -> String
     ; 第一个white-space之前1string组成的string
     (define (list-to-first-white afile)
       (local
         ((define find-re (find-white-space afile)))
         ;=---------------
         (if (boolean? find-re)
             (implode afile)
             (implode (take afile find-re)))))
     
     ; Line -> Line
     ; 第一个white-space之后的list
     (define (remainder-of-first-white afile)
       (local
         ((define find-re (find-white-space afile)))
         ;=---------------
         (if (boolean? find-re)
             '()
             (drop afile (+ 1 find-re)))))