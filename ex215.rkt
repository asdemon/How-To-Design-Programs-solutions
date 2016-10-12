;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex215) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

; state is the position of snake
(define SNAKE-WIDTH 6)
(define SNAKE-IMAGE (circle (/ SNAKE-WIDTH 2) 200 "red"))
(define BACKGROUNG (empty-scene (* SNAKE-WIDTH 40) (* SNAKE-WIDTH 40)))

; world state
; posn : snake's position
; "up","down","left" "right"
(define-struct ws [los direction])

; world state -> Image
; renders the given game state on top of BACKGROUND 
(define (render s)
  (place-image SNAKE-IMAGE (* SNAKE-WIDTH (posn-x (ws-los s)))
               (* SNAKE-WIDTH (posn-y (ws-los s)))
               BACKGROUNG))


;world state -> world state
; tick

(define (tick-handler s)
  (cond
    [(string=? (ws-direction s) "up") (handle-up s) ]
    [(string=? (ws-direction s) "down") (handle-down s)]
    [(string=? (ws-direction s) "left") (handle-left s)]
    [(string=? (ws-direction s) "right") (handle-right s)]
    [else s]))
; world state -> world state
; upwards
(define (handle-up s)
  (make-ws (make-posn (posn-x (ws-los s)) (- (posn-y (ws-los s)) 1)) "up"))
; world state -> world state
; downwards
(define (handle-down s)
  (make-ws (make-posn (posn-x (ws-los s)) (+ (posn-y (ws-los s)) 1)) "down"))
; world state -> world state
; left
(define (handle-left s)
  (make-ws (make-posn (- (posn-x (ws-los s)) 1) (posn-y (ws-los s))) "left"))
; world state -> world state
; left
(define (handle-right s)
  (make-ws (make-posn (+ (posn-x (ws-los s)) 1) (posn-y (ws-los s))) "right"))

; world state -> world state
(define (keyh s ke)
  (cond
    [(key=? ke "left") (make-ws (make-posn (posn-x (ws-los s)) (posn-y (ws-los s))) "left")]
    [(key=? ke "right") (make-ws (make-posn (posn-x (ws-los s)) (posn-y (ws-los s))) "right")]
    [(key=? ke "up") (make-ws (make-posn (posn-x (ws-los s)) (posn-y (ws-los s))) "up")]
    [(key=? ke "down") (make-ws (make-posn (posn-x (ws-los s)) (posn-y (ws-los s))) "down")]
    [else s]))


; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tick-handler 1]
    [on-key keyh]
    [to-draw render]))




(main (make-ws (make-posn 1 1) "right"))
