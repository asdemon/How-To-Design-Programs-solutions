;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname ex56) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require  2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

(define HEIGHT 300) ; distances in pixels 
(define WIDTH  100)
(define YDELTA 3)
 
(define BACKG  (empty-scene WIDTH HEIGHT))
(define ROCKET (rectangle 5 30 "solid" "red"))
 
(define CENTER (- HEIGHT (/ (image-height ROCKET) 2)))
; A LRCD (for launching rocket count down) is one of:
; – "resting"
; – a Number between -3 and -1
; – a NonnegativeNumber 
; interpretation a grounded rocket, in count-down mode,
; a number denotes the number of pixels between the
; top of the canvas and the rocket (its height)


; LRCD -> Image
; renders the state as a resting or flying rocket 
(define (show x)
  (cond
    [(string? x)
     (place-image ROCKET 10 (- HEIGHT CENTER) BACKG)]
    [(<= -3 x -1)
     (place-image (text (number->string x) 20 "red")
                  10 (* 3/4 WIDTH)
                  (place-image ROCKET
                               10 (- HEIGHT CENTER)
                               BACKG))]
    [(>= x 0)
     (place-image ROCKET 10 (- x CENTER) BACKG)]))

 
; LRCD KeyEvent -> LRCD
; starts the count-down when space bar is pressed, 
; if the rocket is still resting 
(define (launch x ke)
  (cond
    [(string? x) (if (string=? " " ke) -3 x)]
    [(<= -3 x -1) x]
    [(>= x 0) x]))
 
; LRCD -> LRCD
; raises the rocket by YDELTA,
;  if it is moving already 
(define (fly x)
  (cond
    [(string? x) x]
    [(<= -3 x -1) (if (= x -1) HEIGHT (+ x 1))]
    [(>= x 0) (- x YDELTA)]))


; WorldState -> WorldState
; launches the program from some initial state 
(define (main ws)
   (big-bang "resting"
     [on-tick fly 1]
     [to-draw show]
     [on-key launch]))