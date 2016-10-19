;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex522) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require 2htdp/image)
(define-struct no-info [])
(define NONE (make-no-info))

; ex522
; left is a Posn, x is number of missionary,y is number of cannibal
; right is as left
; boat is LEFT or RIGHT, represent boat's location
; prev is the list of previous state
(define LEFT 0)
(define RIGHT 1)
(define-struct PuzzleState [left right boat prev])



; ex529
(define (mid-point a b)
  (make-posn (/ (+ (posn-x a) (posn-x b)) 2) (/ (+ (posn-y a) (posn-y b)) 2)))
(define A (make-posn 0 0))
(define B (make-posn 0 40))
(define C (make-posn 40 50))
(define SMALL 0.2)
(define (line-my a c b)
  (local (
          ; posn, posn -> N
          ; calculate the distance between a and b
          (define (dist a b)
            (sqrt (+ (sqr (- (posn-x a) (posn-x b))) (sqr (- (posn-y a) (posn-y b))))))
          (define (line/a a c b scene)
            (cond
              ((< (dist a b) SMALL) (scene+line scene (posn-x a)
                                                (posn-y a) (posn-x c) (posn-y c) "red"))
              (else (local (
                            (define a-b (mid-point a b))
                            (define b-c (mid-point c b))
                            (define a-b-c (mid-point a-b b-c))
                            (define scene2 (line/a a a-b-c a-b scene))
                            (define scene3 (line/a c a-b-c b-c scene2))
                            )
                      ;------------------
                      scene3))))
                            
          )
    ;-------------------------
    (line/a a c b (empty-scene (max (posn-x a)
                                    (posn-x b)
                                    (posn-x c))
                               (max (posn-y a)
                                    (posn-y b)
                                    (posn-y c))))))