;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex59) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH-LIGHTS 90)
(define HEIGHT-LIGHTS 30)
(define RADIUS (/ (min HEIGHT-LIGHTS WIDTH-LIGHTS) 3))
(define H-CENTER (/ (min HEIGHT-LIGHTS WIDTH-LIGHTS) 2))
(define BG
  (place-image (circle RADIUS "outline" "green") (* H-CENTER 5) H-CENTER
  (place-image (circle RADIUS "outline" "yellow") (* H-CENTER 3) H-CENTER
  (place-image (circle RADIUS "outline" "red") H-CENTER H-CENTER (empty-scene WIDTH-LIGHTS HEIGHT-LIGHTS)))))

;TrafficLight mode Image -> Image
;在相应位置画出相应的指示灯
(define (place-lignt cs mode background)
  (cond
    [(string=? cs "green") (place-image (circle RADIUS mode "green") (* RADIUS 5) RADIUS background)]
    [(string=? cs "yellow") (place-image (circle RADIUS mode "yellow") (* RADIUS 3) RADIUS background)]
    [(string=? cs "red") (place-image (circle RADIUS mode "red") RADIUS RADIUS background)]))

; TrafficLight -> TrafficLight
; yields the next state given current state cs
; "green" -> "yellow"
; "yellow" -> "red"
; "red" -> "green"
(check-expect (tl-next "green") "yellow")
(check-expect (tl-next "yellow") "red")
(check-expect (tl-next "red") "green")
(define (tl-next cs)
  (cond
    [(string=? cs "green") "yellow"]
    [(string=? cs "yellow") "red"]
    [(string=? cs "red") "green"]))
 
; TrafficLight -> Image
; renders the current state cs as an image
(define (tl-render current-state)
  (cond
     [(string=? current-state "green") (place-image (circle RADIUS "solid" "green") (* H-CENTER 5) H-CENTER BG)]
    [(string=? current-state "yellow") (place-image (circle RADIUS "solid" "yellow") (* H-CENTER 3) H-CENTER BG)]
    [(string=? current-state "red") (place-image (circle RADIUS "solid" "red") H-CENTER H-CENTER BG)]))


; TrafficLight -> TrafficLight
; simulates a clock-based American traffic light
(define (traffic-light-simulation initial-state)
  (big-bang initial-state
    [to-draw tl-render]
    [on-tick tl-next 1]))