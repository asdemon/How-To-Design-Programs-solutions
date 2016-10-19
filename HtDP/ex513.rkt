;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex513) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; A Lam is one of: 
; – a Symbol
; – (list 'λ (list Symbol) Lam)
; – (list Lam Lam)

(define ex1 '(λ (x) x))
(define ex2 '(λ (x) y))
(define ex3 '(λ (y) (λ (x) y)))
(define ex4 '((λ (x) (x x)) (λ (x) (x x))))

; ex513
; Lam -> boolean
; is a var?
(check-expect (is-var? ex1) #f)
(check-expect (is-var? ex4) #f)
(check-expect (is-var? 'λ) #t)
(define (is-var? l)
  (symbol? l))

; Lam -> boolean
; is a λ?
(check-expect (is-λ? ex1) #t)
(check-expect (is-λ? ex4) #f)
(check-expect (is-λ? 'λ) #f)
(define (is-λ? l)
  (if (list? l)
      (local ((define re-first (car l)))
        ;-------------
        (if (symbol? re-first)
            (symbol=? re-first 'λ)
            #f))
      #f))

; Lam -> boolean
; is a applications?
(check-expect (is-app? ex1) #f)
(check-expect (is-app? ex4) #t)
(check-expect (is-app? 'λ) #f)
(define (is-app? l)
  (if (list? l)
      (if (= 2 (length l))
          #t
          #f)
      #f))

; Lam -> list of symbols
; it extracts the parameter from a λ expression
(check-expect (λ-para ex1) '(x))
(define (λ-para l)
  (if (is-λ? l)
      (cadr l)
      (error "input is not a λ")))

; Lam -> Lam
; it extracts the body from a λ expression
(check-expect (λ-body ex1) 'x)
(define (λ-body l)
  (if (is-λ? l)
      (third l)
      (error "input is not a λ")))

; Lam -> Lam
; it extracts the function from an application
(check-expect (app-fun ex4) '(λ (x) (x x)))
(define (app-fun l)
  (if (is-app? l)
      (car l)
      (error "input is not a application")))

; Lam -> Lam
; it extracts the argument from an application
(check-expect (app-arg ex4) '(λ (x) (x x)))
(define (app-arg l)
  (if (is-app? l)
      (cadr l)
      (error "input is not a application")))

; Lam -> list of symbols
; list of all symbols used as λ parameters in a λ term
(check-expect (declareds ex4) '(x x))
(define (declareds l)
  (cond
    ((is-var? l) '())
    ((is-λ? l) (λ-para l))
    ((is-app? l) (append (declareds (car l)) (declareds (cadr l))))
    (else (error "input error in declareds"))))


; ex518
; Lam -> "some kind of Lam"
; It replaces all occurrences of variables with a natural number that represents how far away the declaring λ is
(check-expect (static-distance '((λ (x) (λ (y) (y x))) (λ (z) z)))
              '((λ (x) (λ (y) (0 1))) (λ (z) 0)))
(define (static-distance l0)
  (local (
          (define (static-distance/a le a)
            (cond
              [(is-var? le)
               (local ((define re (index-of a le)))
                 ;---------------
                 (if  (boolean? re) (error "can find re") re))]
              [(is-λ? le)
               (local ((define para (λ-para le))
                       (define body (λ-body le))
                       (define newd (cons para a)))
                 (list 'λ  para
                   (static-distance/a body newd)))]
              [(is-app? le)
               (local ((define fun (app-fun le))
                       (define arg (app-arg le)))
               (list (static-distance/a fun a)
                     (static-distance/a arg a)))]))
          ;list of list of symbol , symbol-> number or #f
          ;it consumes a list of list of symbol l and a symbol s, find s in the list l, and return the index of the list contains s, otherwise it
          ;return #f
          (define (index-of l s)
            (cond
              ((empty? l) #f)
              (else (if (member? s (car l))
                        0
                        (local (
                                (define re (index-of (cdr l) s))
                                )
                          ;------------------
                          (if (boolean? re)
                              #f
                              (+ 1 re)))))))
          )
    ;---------------------------------------------------
    (static-distance/a l0 '())))
