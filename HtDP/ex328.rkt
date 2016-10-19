;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex328) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
; An S-expr is one of: 
; – Atom
; – SL

; An SL is one of: 
; – '()
; – (cons S-expr SL)


; An Atom is one of: 
; – Number
; – String
; – Symbol 

; any -> boolean
; input is atom or not
(define (atom? i)
  (or (number? i)
      (string? i)
      (symbol? i)))

; S-expr Symbol Atom -> S-expr
; replaces all occurrences of old in sexp with new
 
(check-expect (substitute '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute '(((world bye (world)) bye) bye) 'bye '42)
              '(((world 42 (world)) 42) 42))
(check-expect (substitute '(((world bye (world)) bye) bye) 'ww '42)
              '(((world bye (world)) bye) bye))
 
(define (substitute sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp) (for-atom sexp)]
              [else (for-sl sexp)]))
          ; SL -> S-expr 
          (define (for-sl sl)
            (cond
              [(empty? sl) '()]
              [else (cons (for-sexp (first sl))
                          (for-sl (rest sl)))]))
          ; Atom -> S-expr
          (define (for-atom at)
            (cond
              [(number? at) at]
              [(string? at) at]
              [(symbol? at) (if (equal? at old) new at)])))
    (for-sexp sexp)))

(check-expect (substitute.v2 '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute.v2 '(((world bye (world)) bye) bye) 'bye '42)
              '(((world 42 (world)) 42) 42))
(check-expect (substitute.v2 '(((world bye (world)) bye) bye) 'ww '42)
              '(((world bye (world)) bye) bye))
(define (substitute.v2 sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp) (for-atom sexp)]
              [else (for-sl sexp)]))
          ; SL -> S-expr 
          (define (for-sl sl) (map for-sexp sl))
          ; Atom -> S-expr
          (define (for-atom at)
            (if (equal? at old) new at)))
    (for-sexp sexp)))

(check-expect (substitute.v3 '(((world) bye) bye) 'bye '42)
              '(((world) 42) 42))
(check-expect (substitute.v3 '(((world bye (world)) bye) bye) 'bye '42)
              '(((world 42 (world)) 42) 42))
(check-expect (substitute.v3 '(((world bye (world)) bye) bye) 'ww '42)
              '(((world bye (world)) bye) bye))
(define (substitute.v3 sexp old new)
  (local (; S-expr -> S-expr
          (define (for-sexp sexp)
            (cond
              [(atom? sexp)
               (if (equal? sexp old) new sexp)]
              [else
               (map for-sexp sexp)])))
    (for-sexp sexp)))