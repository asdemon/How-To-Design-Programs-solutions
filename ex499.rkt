;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex499) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define-struct node [left right])
; A Tree is one of: 
; – '()
; – (make-node Tree Tree)
(define example
  (make-node (make-node '() (make-node '() '())) '()))

; Tree -> N
; measure the height of abt0
(check-expect (height.v2 example) 3)
(define (height.v2 abt0)
  (local (; Tree N -> N
          ; measure the height of abt
          ; accumulator a is the number of steps 
          ; it takes to reach abt from abt0
          (define (height/a abt a)
            (cond
              [(empty? abt) a]
              [else
               (max
                (height/a (node-left abt)  (+ a 1))
                (height/a (node-right abt) (+ a 1)))])))
    (height/a abt0 0)))

; ex500
; list of number -> number
; it computes the product of a list of numbers
(check-expect (product.v2 '(1 2 2 3)) 12)
(define (product.v2 l0)
  (local (
          (define (product.v2/a l a)
            (cond
              ((empty? l) a)
              (else (product.v2/a (cdr l) (* (car l) a)))))
          )
    ;---------------------------
    (if (empty? l0)
        (error "list can be empty")
        (product.v2/a l0 1))))


; ex501
; list -> number
; it compute the list's length
(check-expect (how-many '(1 2 2 3)) 4)
(check-expect (how-many '()) 0)
(define (how-many l0)
  (local (
          (define (how-many/a l a)
            (cond
              ((empty? l) a)
              (else (how-many/a (cdr l) (add1 a)))))
          )
    ;--------------------
    (how-many/a l0 0)))

; ex502
; N -> Number 
; add n to pi without use +
(check-within (add-to-pi 2) (+ 2 pi) 0.001)
(define (add-to-pi n0)
  (local (
          (define (add-to-pi/a n a)
            (cond
              [(zero? n) a]
              [else (add-to-pi/a (sub1 n) (add1 a))]))
          )
    ;-------------------
    (add-to-pi/a n0 pi)))


; ex503
; [NEList-of 1String] -> [NEList-of 1String]
; create a palindrome from s0
(check-expect
 (mirror (explode "abc")) (explode "abcba"))
(define (mirror s0)
  (append s0
          (cdr (reverse s0))))


; ex504
; Matrix -> Matrix 
; find a row that doesn't start with 0 and
; use it as the first one
; generative move the first row to last place 
; no termination if all rows start with 0
(check-expect (rotate.v2 '((0 4 5) (1 2 3)))
              '((1 2 3) (0 4 5)))
(define (rotate M)
  (if (andmap (lambda (row) (= 0 (car row))) M)
      (error "all rows start with 0")
      (cond
        [(not (= (first (first M)) 0)) M]
        [else
         (rotate (append (rest M) (list (first M))))])))

(define (rotate.v2 M0)
  (local (; Matrix ... -> Matrix 
          ; accumulator ...
          (define (rotate/a M seen)
            (cond
              [(empty? M) seen]
              [else (if (not (= (first (first M)) 0))
                         (append M seen)
                         (rotate/a (rest M)
                                   (cons (car M) seen)))]))
          )
    ;----------------------------
    (rotate/a M0 '())))

; ex505
; list of number -> number
; It consumes a list of digits and produces the corresponding number
(define (to10 l0)
  (local (
          (define (to10/a l ac)
            (cond
              ((empty? l) ac)
              (else (to10/a (cdr l) (+ (* 10 ac) (car l))))))
          )
    ;--------------------------
    (to10/a l0 0)))

; ex508 task1
; Assume that the difference between l0 and l is (list x1 x2 x3). What is a then?
; f(x3, f(x2, f(x1, a)))

; task2
(define (build-l*st n f)
  (local (
          (define (build-l*st/a n f a)
            (cond
              ((= n 0) a)
              (else (build-l*st/a (sub1 n) f (cons (f (sub1 n)) a) ))))
          )
    ;-----------------------
    (build-l*st/a n f '())))