;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex109) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
;(require lang/htdp-beginner)

(define YELLOW (rectangle 100 100 "solid" "yellow"))
(define RED (rectangle 100 100 "solid" "red"))
(define GREED (rectangle 100 100 "solid" "green"))
(define BACKGROUND (empty-scene 100 100))


; World State
; ExpectsToSee.v2 is one of:
; – AA
; – BC
; – DD 
; – ER 
 
(define AA "start, ...")
(define BC "expect ...")
(define DD "finished")
(define ER "error, ...")

; World State -> Image
; renders the given game state on top of BACKGROUND 
; for examples see figure 32
(define (si-render s)
  (cond
    [(equal? s AA) BACKGROUND]
    [(equal? s BC) YELLOW]
    [(equal? s DD) GREED]
    [(equal? s ER) RED]))

;on-key
;空格由“stop”转化为10，其他时机条件不变
(define (key-handler s a-key)
  (cond
     [(equal? s AA) (if (key=? a-key "a") BC ER)]
    [(equal? s BC) (if (or (key=? a-key "b") (key=? a-key "c")) BC (if (key=? a-key "d") DD ER))]
    [(equal? s DD) DD]
    [(equal? s ER) ER]))



; World State -> World State
; launches the program from some initial state 
(define (main ws)
   (big-bang ws
     [to-draw si-render]
     [on-key key-handler]))