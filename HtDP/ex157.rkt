;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex157) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)


(define HEIGHT 80) ; distances in terms of pixels 
(define WIDTH 100)
(define XSHOTS (- (/ WIDTH 2) 5))
 
; graphical constants 
(define BACKGROUND (empty-scene WIDTH HEIGHT))
(define SHOT (triangle 3 "solid" "red"))


; A List-of-shots is one of: 
; – '()
; – (cons Shot List-of-shots)
; interpretation the collection of shots fired 

; A Shot is a Number.
; interpretation represents the shot's y-coordinate 

; ShotWorld -> Image
; adds the image of a shot for each  y on w 
; at (MID,y} to the background image
(check-expect (to-image (cons 9 '()))
              (place-image SHOT XSHOTS 9 BACKGROUND))
; ShotWorld -> Image 
; adds each shot y on w at (XSHOTS,y} to BACKGROUND
(define (to-image w)
  (cond
    [(empty? w) BACKGROUND]
    [else (place-image SHOT XSHOTS (first w)
                       (to-image (rest w)))]))

; ShotWorld -> ShotWorld 
; moves each shot up by one pixel 
(define (tock w)
  (cond
    [(empty? w) '()]
    [else (remove-invisible (cons (sub1 (first w)) (tock (rest w))))]))

; ShotWorld -> ShotWorld 
; remove y which is invisible
(check-expect (remove-invisible (cons -10 '()))
              '())
(check-expect (remove-invisible (cons -10 (cons 1 '())))
              (cons 1 '()))
(define (remove-invisible w)
  (cond
    ((empty? w) '())
    (else (if (< (first w) (- (/ (image-height SHOT) 2)))
              (remove-invisible (rest w))
              (cons (first w) (remove-invisible (rest w)))))))
 
; ShotWorld KeyEvent -> ShotWorld 
; adds a shot to the world if the space bar was hit 
(define (keyh w ke)
  (if (key=? ke " ") (cons HEIGHT w) w))
 





; ShotWorld -> ShotWorld 
(define (main w0)
  (big-bang w0
    [on-tick tock]
    [on-key keyh]
    [to-draw to-image]))