;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex349) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define WRONG "wrong")

(define-struct add [left right])
(define-struct mul [left right])

(define (atom? e)
  (or (number? e)
      (string? e)
      (symbol? e)))

; S-expr -> BSL-expr
(define (parse s)
  (cond
    [(atom? s) (parse-atom s)]
    [else (parse-sl s)]))

; SL -> BSL-expr 
(define (parse-sl s)
  (local ((define L (length s)))
    (cond
      [(< L 3) (error WRONG)]
      [(and (= L 3) (symbol? (first s)))
       (cond
         [(symbol=? (first s) '+)
          (make-add (parse (second s)) (parse (third s)))]
         [(symbol=? (first s) '*)
          (make-mul (parse (second s)) (parse (third s)))]
         [else (error WRONG)])]
      [else (error WRONG)])))

; Atom -> BSL-expr 
(define (parse-atom s)
  (cond
    [(number? s) s]
    [(string? s) (error WRONG)]
    [(symbol? s) (error WRONG)]))

; ex351
; S-expressions -> N
; it produces their value. Otherwise, it signals the same error as parse
(define (interpreter-expr sexp)
  (local (;BSL-expr-> N
          (define (eval-expression re)
            (cond
              ((number? re) re)
              (else (match re
                      ((add left right) (+ (eval-expression left) (eval-expression right)))
                      ((mul left right) (* (eval-expression left) (eval-expression right))))))))
    ;-------IN--------
    (eval-expression (parse sexp))))




; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)




; ex352
; BSL-var-expr ex, a Symbol x, and a Number v -> BSL-var-expr
; It produces a BSL-var-expr like ex with all occurrences of x replaced by v.
(check-expect (subst 	
               (make-add (make-mul 'x 'x)
                         (make-mul 'y 'y))
               'x
               5)
              (make-add (make-mul 5 5)
                        (make-mul 'y 'y)))

(check-expect (subst 	
               (make-add 'x 3)
               'x
               5)
              (make-add 5 3))
(define (subst ex x v)
  (match ex
    ((? number?) ex)
    ((? symbol?) (if (symbol=? ex x)
                     v
                     ex))
    ((add left right) (make-add (subst left x v) (subst right x v)))
    ((mul left right) (make-mul (subst left x v) (subst right x v)))))



; A BSL-expr is one of: 
; – Number
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)

; ex353
; BSL-var-expr -> boolean
; It determines whether a BSL-var-expr is also a BSL-expr
(check-expect (numeric? 	
               (make-add 'x 3))
              #f)
(check-expect (numeric? 	
               (make-add 2 3))
              #t)
(define (numeric? ex)
  (match ex
    ((? number?) #t)
    ((? symbol?) #f)
    ((add left right) (and (numeric? left) (numeric? right)))
    ((mul left right) (and (numeric? left) (numeric? right)))))


; ex354
; BSL-var-expr -> N
; It consumes a BSL-var-expr and determines its value if numeric? yields true for the input. Otherwise it signals an error.
(check-expect (eval-variable 	
               (make-add (make-mul 1 2) 3))
              5)
(define (eval-variable se)
  (if (numeric? se)
      (local (;BSL-expr-> N
              (define (eval-expression re)
                (cond
                  ((number? re) re)
                  (else (match re
                          ((add left right) (+ (eval-expression left) (eval-expression right)))
                          ((mul left right) (* (eval-expression left) (eval-expression right))))))))
        ;-------IN--------
        (eval-expression se))
      (error WRONG)))



; An AL (short for association list) is [List-of Association].
; An Association is a list of two items:
;   (cons Symbol (cons Number '())).

;  BSL-var-expr ex, an association list da -> N
; If numeric? holds for the result, it determines its value; otherwise it signals error
(check-expect (eval-variable* (make-add 'x 3) '((x 3))) 6)
(check-error (eval-variable* (make-add 'x 3) '()) WRONG)
(check-expect (eval-variable* (make-add (make-mul 'x 'x)
                                        (make-mul 'y 'y))
                              '((x 3) (y 4))) 25)
(define (eval-variable* ex da)
  (local
    (; BSL-var-expr ex, an association list da -> BSL-var-expr
     ; substi ex with da
     (define (substi ex da)
       (foldr (lambda (ele-da ex) (subst ex (first ele-da) (second ele-da))) ex da))
     (define substi-result (substi ex da))
     )
    ;-----
    (eval-variable substi-result)))





; ex355
; BSL-var-expr AL -> Number
(check-expect (eval-var-lookup (make-add 'x 3) '((x 3))) 6)
(check-error (eval-var-lookup (make-add 'x 3) '()) WRONG)
(check-expect (eval-var-lookup (make-add (make-mul 'x 'x)
                                         (make-mul 'y 'y))
                               '((x 3) (y 4))) 25)
(define (eval-var-lookup ex da)
  (match ex
    ((? number?) ex)
    ((? symbol?) (local ((define re (assq ex da)))
                   ;-------------------------
                   (if (boolean? re) (error WRONG) (second re))))
    ((add left right) (+ (eval-var-lookup left da) (eval-var-lookup right da)))
    ((mul left right) (* (eval-var-lookup left da) (eval-var-lookup right da)))))


; ex356
(define-struct func [name arg expr])
; name是函数的名字
; arg为参数字典的list，字典的第一个是形参，第二个是实参
; expr是函数体

; A BSL-var-expr is one of: 
; – Number
; – Symbol 
; – (make-add BSL-var-expr BSL-var-expr)
; – (make-mul BSL-var-expr BSL-var-expr)
; – (make-func name '((parameter BSL-var-expr)) expr)



; ex357
;eval-definition1 consumes four arguments:
;a BSL-fun-expr ex;
;a symbol f, which represents a function name;
;a symbol x, which represents the functions’s parameter; and
;a BSL-fun-expr b, which represents the function’s body.
(check-expect (eval-definition1 (make-func 'k '((x 4)) '())
                                'k
                                'x
                                (make-add 'x 0))
              4)
(check-expect (eval-definition1 (make-add 1 (make-func 'k '((x 4)) '()))
                                'k
                                'x
                                (make-add 'x 0))
              5)

(define (eval-definition1 ex f x b)
  (match ex
    ((? number?) ex)
    ((? symbol?) (local ((define re (assq ex '())))
                   ;-------------------------
                   (if (boolean? re) (error WRONG) (second re))))
    ((add left right) (+ (eval-definition1 left f x b) (eval-definition1 right f x b)))
    ((mul left right) (* (eval-definition1 left f x b) (eval-definition1 right f x b)))
    ((func name arg expr) (eval-var-lookup (subst b x
                                                  (eval-var-lookup x (map (lambda (ele) (list (first ele) (eval-definition1 (second ele) f x b))) arg))) '()))))

(check-expect (eval-definition1.v2 (make-func 'k '((x 4)) '())
                                   'k
                                   'x
                                   (make-add 'x 0))
              4)
(check-expect (eval-definition1.v2 (make-add 1 (make-func 'k '((x 4)) '()))
                                   'k
                                   'x
                                   (make-add 'x 0))
              5)

(define (eval-definition1.v2 ex f x b)
  (match ex
    ((? number?) ex)
    ((? symbol?) (local ((define re (assq ex '())))
                   ;-------------------------
                   (if (boolean? re) (error WRONG) (second re))))
    ((add left right) (+ (eval-definition1 left f x b) (eval-definition1 right f x b)))
    ((mul left right) (* (eval-definition1 left f x b) (eval-definition1 right f x b)))
    ((func name arg expr) (if (equal? name f)
                              (eval-definition1.v2 (subst b x
                                                          (eval-var-lookup x (map (lambda (ele) (list (first ele) (eval-definition1.v2 (second ele) f x b))) arg))) f x b)
                              (error WRONG)))))


; ex359
; BSL-fun-def* Symbol -> BSL-fun-def
; retrieves the definition of f in da
; signals an error if there is none
(define (lookup-def da f)
  (match da
    ('() (error WRONG))
    ((cons h t) (if (symbol=? f (func-name h))
                    h
                    (lookup-def t f)))))

(check-expect (eval-function* (make-add 1 (make-func 'k '((x 4)) '()))
                                   (list (make-func 'k '((x '())) (make-add 'x 0))))
              5)
(define (eval-function* ex list-of-f)
  (match ex
    ((? number?) ex)
    ((? symbol?) (local ((define re (assq ex '())))
                   ;-------------------------
                   (if (boolean? re) (error WRONG) (second re))))
    ((add left right) (+ (eval-function* left list-of-f) (eval-function* right list-of-f)))
    ((mul left right) (* (eval-function* left list-of-f) (eval-function* right list-of-f)))
    ((func name arg expr) (local (
                                  (define func-expr-res (lookup-def list-of-f name))
                                  ; BSL-var-expr ex, an association list da -> BSL-var-expr
                                  ; substi ex with da
                                  (define (substi ex da)
                                    (foldr (lambda (ele-da ex) (subst ex (first ele-da) (second ele-da))) ex da))
                                  )
                            ;------------------
                            (eval-function* (substi (func-expr func-expr-res) 
                                                        (map (lambda (ele) (list (first ele) (eval-function* (second ele) list-of-f))) arg)) list-of-f) ))))



(check-expect (eval-all* (make-mul (make-add 1 (make-func 'k '((x 4)) '())) 'y)
                                   (list (make-func 'k '((x '())) (make-add 'x 0))) '((y 3)))
              15)
(define (eval-all* ex list-of-f da)
  (match ex
    ((? number?) ex)
    ((? symbol?) (local ((define re (assq ex da)))
                   ;-------------------------
                   (if (boolean? re) (error WRONG) (second re))))
    ((add left right) (+ (eval-all* left list-of-f da) (eval-all* right list-of-f da)))
    ((mul left right) (* (eval-all* left list-of-f da) (eval-all* right list-of-f da)))
    ((func name arg expr) (local (
                                  (define func-expr-res (lookup-def list-of-f name))
                                  ; BSL-var-expr ex, an association list da -> BSL-var-expr
                                  ; substi ex with da
                                  (define (substi ex da)
                                    (foldr (lambda (ele-da ex) (subst ex (first ele-da) (second ele-da))) ex da))
                                  )
                            ;------------------
                            (eval-all* (substi (func-expr func-expr-res) 
                                                        (map (lambda (ele) (list (first ele) (eval-function* (second ele) list-of-f))) arg)) list-of-f da) ))))
