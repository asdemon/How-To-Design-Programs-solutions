;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex317) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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

(check-expect (count 'world 'hello) 0)
(check-expect (count '(world hello) 'hello) 1)
(check-expect (count '(((world) hell) llo) 'o) 0)

; S-expr Symbol -> N 
; counts all occurrences of sy in sexp 
(define (count sexp sy)
  (cond
    [(atom? sexp) (count-atom sexp sy)]
    [else (count-sl sexp sy)]))

; SL Symbol -> N 
; counts all occurrences of sy in sl 
(define (count-sl sl sy)
  (cond
    [(empty? sl) 0]
    [else
     (+ (count (first sl) sy) (count-sl (rest sl) sy))]))

; Atom Symbol -> N 
; counts all occurrences of sy in at 
(define (count-atom at sy)
  (cond
    [(number? at) 0]
    [(string? at) 0]
    [(symbol? at) (if (symbol=? at sy) 1 0)]))

; ex317
; S-expr Symbol -> N 
; counts all occurrences of sy in sexp
(check-expect (count.v2 'world 'hello) 0)
(check-expect (count.v2 '(world hello) 'hello) 1)
(check-expect (count.v2 '(((world) hell) llo) 'o) 0)
(define (count.v2 sexp sy)
  (local (
          (define (count-atom1 at sy)
            (cond
              [(number? at) 0]
              [(string? at) 0]
              [(symbol? at) (if (symbol=? at sy) 1 0)]))
          ; SL Symbol -> N 
          ; counts all occurrences of sy in sl 
          (define (count-sl sl sy)
            (cond
              [(empty? sl) 0]
              [else
               (+ (count (first sl) sy) (count-sl (rest sl) sy))])))
          ;----IN----------
          (cond
            [(atom? sexp) (count-atom1 sexp sy)]
            [else (count-sl sexp sy)])))


; ex318
; S-expr -> N 
; determines its depth
(check-expect (depth 'world ) 1)
(check-expect (depth '(world) ) 2)
(check-expect (depth '((world) hell) ) 3)
(check-expect (depth '(((world) hell) llo) ) 4)
(define (depth sexp)
  (cond
    [(atom? sexp) 1]
    [else (depth-sl sexp)]))
; SL -> N
; depth of SL
(define (depth-sl sexp)
  (match sexp
    ('() 0)
    ((cons se sl) (+ (max (depth se) (depth-sl sl)) 1))))


; ex319
;S-expr old-symbol new-symbol -> S-expr
(check-expect (substitute 'world 'www 'eeee ) 'world)
(check-expect (substitute '(world) 'world 'sex ) '(sex))
(check-expect (substitute '((world) hell) 'hell 'sex) '((world) sex))
(check-expect (substitute '(((world) hell) llo) 'world 'sex) '(((sex) hell) llo))
(define (substitute sexp old-symbol new-symbol)
    (local (
          (define (substitute-atom sexp)
            (cond
              [(symbol? sexp) (if (symbol=? sexp old-symbol) new-symbol sexp)]
              [else sexp]))
          ; SL -> SL 
          ; substitute
          (define (substitute-sl sl)
            (cond
              [(empty? sl) sl]
              [else
               (cons (substitute (first sl) old-symbol new-symbol) (substitute-sl (rest sl)))])))
          ;----IN----------
          (match sexp
            [(? atom?) (substitute-atom sexp)]
            [sl (substitute-sl sl)])))
