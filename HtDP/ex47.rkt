;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex47) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
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
(define (render x)
  (place-image/align (rectangle (- WIDTH-OF-WORLD 2) (* HEIGHT-OF-WORLD (/ x 100)) "solid" "red") 1 HEIGHT-OF-WORLD "left" "bottom" BACKGROUND))
 
; WorldState -> WorldState
; substract 0.1 to x to move the car right
; 0 -> 0
; 1 -> 0.9
(define (tock x)
  (if (< (- x 0.1) 0)
      0
      (- x 0.1)))

; WorldState -> WorldState
; substract 0.1 to x to move the car right
; 0 -> 0
; 1 -> 0.9
(define (key-handler w a-key)
  (cond
    ;[(key=? a-key "left")  (world-go w -DELTA)]
   ; [(key=? a-key "right") (world-go w +DELTA)]
    ;[(= (string-length a-key) 1) w] ; order-free checking
    [(key=? a-key "up")    (restrict100 (+ w 1/3))]
    [(key=? a-key "down")  (restrict100 (+ w 1/5))]
    ))

; Number -> Number
; if input > 100 return 100 else return input
; 101 -> 100
; 1 -> 1
(define (restrict100 x)
  (if (> x 100)
      100
      x))


  
; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang 100
     [on-tick tock]
     [to-draw render]
     [on-key key-handler]))