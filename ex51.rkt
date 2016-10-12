;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex51) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require  2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

(define WIDTH-OF-WORLD 100)
(define HEIGHT-OF-WORLD 300)
;(define BACKGROUND (rectangle  WIDTH-OF-WORLD HEIGHT-OF-WORLD "outline" "green"))
(define BACKGROUND (empty-scene  WIDTH-OF-WORLD HEIGHT-OF-WORLD))

; WorldState -> Image
; places the image of the car x pixels from 
; the left margin of the BACKGROUND image
; 0-> none red rect
; 100 -> full red rect
(define (render s)
  (place-image/align (circle 50 "solid" s) 0 HEIGHT-OF-WORLD "left" "bottom" BACKGROUND))
 

(define (tock s)
  (traffic-light-next s))


; TrafficLight -> TrafficLight
; yields the next state given current state s
(check-expect (traffic-light-next "red") "green")
(define (traffic-light-next s)
  (cond
    [(string=? "red" s) "green"]
    [(string=? "green" s) "yellow"]
    [(string=? "yellow" s) "red"]
    [else ""]))


  
; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang "yellow"
     [on-tick tock]
     [to-draw render]))