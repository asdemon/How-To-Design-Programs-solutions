;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex242) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; String [List-of String] -> [Maybe [List-of String]]
; returns the remainder of los starting with s 
; #false otherwise 
(check-expect (occurs "a" (list "b" "a" "d" "e"))
              (list "d" "e"))
(check-expect (occurs "a" (list "b" "c" "d")) #f)
(define (occurs s los)
  (cond
    ((empty? los) #f)
    (else (if (start-with? s (first los))
              (rest los)
              (occurs s (rest los))))))
;auxiliary function
; string s1 ,string s2 -> boolean
; return whether s2 is start with s1
(check-expect (start-with? "a" "a") #t)
(check-expect (start-with? "ab" "a") #f)
(check-expect (start-with? "ab" "abc") #t)
(check-expect (start-with? "" "abc") #t)
(define (start-with? s1 s2)
  (if (> (string-length s1) (string-length s2))
      #f
      (string=? s1 (substring s2 0 (string-length s1)))))