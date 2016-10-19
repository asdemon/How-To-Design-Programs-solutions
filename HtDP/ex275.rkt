;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex275) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)

; On OS X: 
(define DICTIONARY-LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings.
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))


; A Letter is one of the following 1Strings: 
; – "a"
; – ... 
; – "z"
; or, equivalently, a member? of this list: 
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))


; ex195
; Letter DICTIONARY -> Number
;  counts how many words in the given Dictionary start with the given Letter
(define (starts-with# letter dic)
  (cond
    ((empty? dic) 0)
    (else (if (char=? (char-downcase letter) (char-downcase (string-ref (first dic) 0))) (+ 1 (starts-with# letter (rest dic))) (starts-with# letter (rest dic))))))

; a list of Letter-Counts
; pairs of Letters and Ns


; ex196
;list of Letter, DICTIONARY -> a list of Letter-Counts
(define (count-by-letter listOfLetter dic)
  (cond
    ((empty? listOfLetter) '())
    (else (cons (list (first listOfLetter) (starts-with# (first listOfLetter) dic))  (count-by-letter (rest listOfLetter) dic)))))



; ex197
; DICTIONARY -> Letter
; the letter that is most frequently used as the first one in the words of the given Dictionary
(define (most-frequent dic)
  (first (max-lc (count-by-letter LETTERS dic))))

; a list of Letter-Counts -> Letter-Counts
; consume a list of Letter-Counts (at least one element) return the max number which corresponding Letter
(define (max-lc listOflc)
  (cond
    ((empty? (rest listOflc)) (first listOflc))
    (else (if (> (second (first listOflc)) (second (max-lc (rest listOflc)))) (first listOflc) (max-lc (rest listOflc))))))


; ex198
; DICTIONARY -> list of DICTIONARY
; The function consumes a Dictionary and produces a list of Dictionarys, one per Letter.
(define (words-by-first-letter dic)
  (cond
    ((empty? dic) '())
    (else (add-word-to-listOfDic (first dic) (words-by-first-letter (rest dic))))))

; DICTIONARY -> list of DICTIONARY
; put word into DICTIONARY with same prefix
(define (add-word-to-listOfDic word ll)
  (cond
    ((empty? ll) (list (list word)))
    (else (if (prefix? (first (first ll)) word)
              (cons (cons word (first ll))   (rest ll))
              (cons (first ll)  (add-word-to-listOfDic word (rest ll)))))))


; String String -> boolean
; judge word1 and word2 have same prefix
(check-expect (prefix? "Ab" "a") #true)
(check-expect (prefix? "ab" "b") #false)
(check-expect (prefix? "" "b") #false)
(check-expect (prefix? "b" "") #false)
(define (prefix? word1 word2)
  (cond
    ((and (string=? word1 "") (string=? word2 "")) #true)
    ((or (string=? word1 "") (string=? word2 "")) #false)
    (else (char=? (char-downcase (string-ref word1 0)) (char-downcase (string-ref word2 0))))))



; ex275
(define (words-by-first-letter-abstract dict)
  (local
    (; letter -> list of string
     ; find word start with letter in the diectionary and place them in a list
     (define (f letter)
       (filter (lambda (word) (prefix? letter word)) dict)))
    ;------IN------
    (filter (lambda (l) (not (empty? l))) (map f LETTERS))))