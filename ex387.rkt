;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex387) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

(check-expect (cross '(a b c) '(1 2))
              '((a 1) (a 2) (b 1) (b 2) (c 1) (c 2)))
(define (cross a b)
  (for*/list ((i a) (j b))
    (list i j)))


; ex395
(check-expect (take '(1 2 3) 0) '())
(check-expect (take '(1 2 3) 1) '(1))
(check-expect (take '(1 2 3) 6) '(1 2 3))
(define (take l n)
  (if (>= 0 n)
      '()
      (match l
        ('() '())
        ((cons h t) (cons h (take t (sub1 n)))))))

(check-expect (drop '(1 2 3) 0) '(1 2 3))
(check-expect (drop '(1 2 3) 1) '(2 3))
(check-expect (drop '(1 2 3) 6) '())
(define (drop l n)
  (if (>= 0 n)
      l
      (match l
        ('() '())
        ((cons h t) (drop t (sub1 n))))))

; ex396
; On OS X: 
(define DICTIONARY-LOCATION "/usr/share/dict/words")
; A Dictionary is a List-of-strings.
(define DICTIONARY-AS-LIST (read-lines DICTIONARY-LOCATION))
(define LETTERS
  (explode "abcdefghijklmnopqrstuvwxyz"))
; A HM-Word is a [List-of Letter or "_"]
; interpretation "_" represents a letter to be guessed 

; HM-Word N -> String
; run a simplistic Hangman game, produce the current state
(define (play the-pick time-limit)
  (local ((define the-word  (explode the-pick))
          (define the-guess (make-list (length the-word) "_"))
          ; HM-Word -> HM-Word
          (define (do-nothing s) s)
          ; HM-Word KeyEvent -> HM-Word 
          (define (checked-compare current-status ke)
            (if (member? ke LETTERS)
                (compare-word the-word current-status ke)
                current-status)))
    (implode
     (big-bang the-guess ; HM-Word
               [to-draw render-word]
               [on-tick do-nothing 1 time-limit]
               [on-key  checked-compare]))))

; HM-Word -> Image
(define (render-word w)
  (text (implode w) 22 "black"))

; list of letter, HM-Word, letter -> HM-Word
;是否猜中
(define (compare-word the-word current-status ke)
  (match the-word
    ('() '())
    ((cons h t) (if (and (string=? (car current-status) "_")
                         (string=? h ke))
                    (cons h (cdr current-status))
                    (cons (car current-status) (compare-word t (cdr current-status) ke))))))

(define LOCATION "/usr/share/dict/words") ; on OS X
(define AS-LIST (read-lines DICTIONARY-LOCATION))
(define SIZE (length DICTIONARY-AS-LIST))

;(play (list-ref AS-LIST (random SIZE)) 20)

; ex397
(define-struct employee [id name pay-rate])
(define-struct record1 [id hours])

(define (find-hous l id)
  (apply + (for/list ((r l))
             (if (equal? id (record1-id r))
                 (record1-hours r)
                 0))))


; ex398
(check-expect (value (list 5 17 3) (list 0 1 1)) 20)
(define (value coff v)
  (apply + (map * coff v)))

; ex400
; lists of 'a, 'c, 'g, and 't, lists of 'a, 'c, 'g, and 't -> boolean
; The function returns #true if the pattern is identical to the initial part of the search string; otherwise it returns #false.
(check-expect (DNAprefix '() '(a c g)) #t)
(check-expect (DNAprefix '(a) '(a c g)) #t)
(check-expect (DNAprefix '(a c) '(a c g)) #t)
(check-expect (DNAprefix '(g c) '(a c g)) #f)
(check-expect (DNAprefix '(a c g f) '(a c g)) #f)
(define (DNAprefix pattern search)
  (if (<= (length pattern) (length search))
      (match pattern
        ('() #t)
        ((cons h t) (if (symbol=? h (car search))
                        (DNAprefix t (cdr search))
                        #f)))
      #f))

(check-expect (DNAdelta '() '(a c g)) 'a)
(check-expect (DNAdelta '(a) '(a c g)) 'c)
(check-expect (DNAdelta '(a c) '(a c g)) 'g)
(check-expect (DNAdelta '(g c) '(a c g)) #f)
(check-expect (DNAdelta '(a c g f) '(a c g)) #f)
(define (DNAdelta pattern search)
  (cond
    ((and (empty? pattern) (empty? search)) (error "ab"))
    ((and (empty? pattern) (not (empty? search))) (car search))
    ((and (not (empty? pattern)) (empty? search)) #f)
    (else (if (symbol=? (car pattern) (car search))
              (DNAdelta (cdr pattern) (cdr search))
              #f))))


; ex401
; An S-expr (S-expression) is one of: 
; – Atom
; – [List-of S-expr]
; 
; An Atom is one of: 
; – Number
; – String
; – Symbol

; S-expr, S-expr -> boolean
; determines whether two S-expressions are equal.
(check-expect (sexp=? '(a (a b) (a b)) '(a (a b) (a b))) #t)
(check-expect (sexp=? '() '()) #t)
(check-expect (sexp=? '(a (a b) (a b)) '(f (a b) (a b))) #f)
(check-expect (sexp=? '() '(a)) #f)
(define (sexp=? a b)
  (match a
    ('() (empty? b))
    ((cons h t) (if (list? b)
                    (and (equal? h (car b))
                        (sexp=? t (cdr b)))
                    #f))
    (s (if (list? b)
           #f
           (equal? s b)))))
                
                
                
                ; ex401
                