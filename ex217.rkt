;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex217) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

; state is the position of snake
(define SNAKE-WIDTH 6)
(define SNAKE-IMAGE (circle (/ SNAKE-WIDTH 2) 200 "red"))
; world's height and width, means containing how many SNAKE-WIDTHs 
(define WIDTH 40)
(define HEIGHT 40)
(define BACKGROUNG (empty-scene (* SNAKE-WIDTH WIDTH) (* SNAKE-WIDTH HEIGHT)))

; world state
; list of posn : snake's position at first
; "up","down","left" "right"
(define-struct ws [los direction])

; world state -> Image
; renders the given game state on top of BACKGROUND 
(define (render s)
  (render-snake (ws-los s)
               BACKGROUNG))
; list of posn, image -> Image
; render snake on the image passed in
(define (render-snake lis im)
  (cond
    ((empty? lis) im)
    (else (place-image SNAKE-IMAGE (* SNAKE-WIDTH (posn-x (first lis)))
               (* SNAKE-WIDTH (posn-y (first lis)))
               (render-snake (rest lis) im)))))



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
  (make-ws (cons (posn-up (first (ws-los s))) (remove-last (ws-los s))) "up"))
; world state -> world state
; downwards
(define (handle-down s)
  (make-ws (cons (posn-down (first (ws-los s))) (remove-last (ws-los s))) "down"))
; world state -> world state
; left
(define (handle-left s)
  (make-ws (cons (posn-left (first (ws-los s))) (remove-last (ws-los s))) "left"))
; world state -> world state
; left
(define (handle-right s)
  (make-ws (cons (posn-right (first (ws-los s))) (remove-last (ws-los s))) "right"))


(define (posn-up x)
  (make-posn (posn-x x) (- (posn-y x) 1)))
(define (posn-down x)
  (make-posn (posn-x x) (+ (posn-y x) 1)))
(define (posn-left x)
  (make-posn (- (posn-x x) 1) (posn-y x)))
(define (posn-right x)
  (make-posn (+ (posn-x x) 1) (posn-y x)))
; world state -> world state
(define (keyh s ke)
  (cond
    [(key=? ke "left") (make-ws (ws-los s) "left")]
    [(key=? ke "right") (make-ws (ws-los s) "right")]
    [(key=? ke "up") (make-ws (ws-los s) "up")]
    [(key=? ke "down") (make-ws (ws-los s) "down")]
    [else s]))

; world state -> boolean
; decide whether to stop program
(define (stop? s)
  (or (<= (posn-x (first (ws-los s))) 0)
      (>= (posn-x (first (ws-los s))) WIDTH)
      (<= (posn-y (first (ws-los s))) 0)
      (>= (posn-y (first (ws-los s))) HEIGHT)
      (and (string=? (ws-direction s) "up") (member? (posn-up (first (ws-los s))) (ws-los s) ))
      (and (string=? (ws-direction s) "down") (member? (posn-down (first (ws-los s))) (ws-los s) ))
      (and (string=? (ws-direction s) "left") (member? (posn-left (first (ws-los s))) (ws-los s) ))
      (and (string=? (ws-direction s) "right") (member? (posn-right (first (ws-los s))) (ws-los s) ))))
  
(define (stop-picture s)
  (place-image/align (text "worm hit border" 24 "indigo")
                     0 0 "left" "top"
                     (render s)))



; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tick-handler 1]
    [on-key keyh]
    [to-draw render]
    [stop-when stop? stop-picture]))


; list of something -> list of something
; remove last element
(check-expect (remove-last (list "a" "b" "c"))
              (list "a" "b"))
(check-expect (remove-last (list "a"))
              '())
(check-expect (remove-last '())
              '())
(define (remove-last l)
  (cond
    ((empty? l) '())
    ((empty? (rest l)) '())
    (else (cons (first l)
                (remove-last (rest l))))))
     



(main (make-ws (list (make-posn 3 1) (make-posn 2 1) (make-posn 1 1) (make-posn 0 1)) "right"))
