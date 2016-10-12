;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex205) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LLists
(define list-tracks
  (read-itunes-as-lists ITUNES-LOCATION))


; ex206
; String, LAssoc Any -> BSDN
; It consumes three arguments: a String called key; an LAssoc; and an element of Any called default.
; It produces the first Association whose first item is equal to key or default if there is no such Association.
(check-expect (find-association "Track ID" 0 (first list-tracks))
              188)
(check-expect (find-association "Track IdD" 0 (first list-tracks))
              0)
(define (find-association key default lA)
  (cond
    ((empty? lA) default)
    (else (if (string=? (first (first lA)) key)
              (second (first lA))
              (find-association key default (rest lA))))))


; ex207
; LLists -> N
; LLists and produces the total amount of play time
(check-expect (total-time/list list-tracks)
              (+ 2125 202318 281256))
(define (total-time/list ll)
  (cond
    ((empty? ll) 0)
    (else (+ (find-association "Total Time" 0 (first ll)) (total-time/list (rest ll))))))



; ex208
; LLists -> list of Strings
; The function consumes an LLists and produces the Strings that are associated with a Boolean attribute
(check-expect (boolean-attributes list-tracks)
              (list "Has Video" "HD"))
(define (boolean-attributes ll)
  (cond
    ((empty? ll) '())
    (else (create-set (append (boolean-attributes-sub (first ll)) (boolean-attributes (rest ll)))))))
; LAssoc -> list of Strings
; consume LAssoc and produces the Strings that are associated with a Boolean attribute
(check-expect (boolean-attributes-sub (first list-tracks))
              (list "Has Video" "HD"))
(define (boolean-attributes-sub lA)
  (cond
    ((empty? lA) '())
    (else (if (boolean? (second (first lA)))
              (cons (first (first lA)) (boolean-attributes-sub (rest lA)))
              (boolean-attributes-sub (rest lA))))))
; List-of-strings -> List-of-strings.
; It consumes a List-of-strings and constructs one that contains every String from the given list exactly once
(define (create-set l)
  (cond
    ((empty? l) '())
    (else (if (in? (first l) (create-set (rest l)))
              (create-set (rest l))
              (cons (first l) (create-set (rest l)))))))

(define (in? word ll)
  (cond
    ((empty? ll) #f)
    (else (or (string=? word (first ll)) (in? word (rest ll))))))