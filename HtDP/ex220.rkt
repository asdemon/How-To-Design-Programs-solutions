;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex220) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

(define WIDTH 10) ; # of blocks, horizontally 
(define SIZE 10) ; blocks are squares
(define SCENE-SIZE (* WIDTH SIZE))
 (define BACKGROUNG (empty-scene SCENE-SIZE SCENE-SIZE))
 
(define BLOCK ; red squares with black rims
  (overlay
    (square (- SIZE 1) "solid" "red")
    (square SIZE "outline" "black")))

(define-struct tetris [block landscape])
(define-struct block [x y])
 
; A Tetris is a structure:
;   (make-tetris Block Landscape)
; A Landscape is one of: 
; – '() 
; – (cons Block Landscape)
; Block is a structure:
;   (make-block N N)
 
; interpretations
; (make-block x y) depicts a block whose left 
; corner is (* x SIZE) pixels from the left and
; (* y SIZE) pixels from the top;
; (make-tetris b0 (list b1 b2 ...)) means b0 is the
; dropping block, while b1, b2, and &hellip are resting


; tetris -> Image
; renders the given game state on top of BACKGROUND 
(define (tetris-render s)
  (render-list (cons (tetris-block s) (tetris-landscape s))
               BACKGROUNG))
; list of posn, image -> Image
; render snake on the image passed in
(define (render-list lis im)
  (cond
    ((empty? lis) im)
    (else (place-image/align BLOCK (* SIZE (posn-x (first lis)))
               (* SIZE (posn-y (first lis))) "left" "top"
               (render-list (rest lis) im)))))




; tetris-> tetris
; tick

(define (tick-handler s)
  (if (land? s)
      (make-tetris (block-generate 1) (cons (tetris-block s) (tetris-landscape s)))
      (make-tetris (next-posn (tetris-block s)) (tetris-landscape s))))

; tetris -> boolean
; 判定是否落地
(define (land? s)
  (or (= (posn-y (tetris-block s)) (- WIDTH 1))
      (member? (next-posn (tetris-block s)) (tetris-landscape s))))

(define (next-posn p)
  (posn-down p))
  
(define (posn-up x)
  (make-posn (posn-x x) (- (posn-y x) 1)))
(define (posn-down x)
  (make-posn (posn-x x) (+ (posn-y x) 1)))
(define (posn-left x)
  (make-posn (- (posn-x x) 1) (posn-y x)))
(define (posn-right x)
  (make-posn (+ (posn-x x) 1) (posn-y x)))




; ShotWorld -> ShotWorld 
(define (tetris-main w0)
  (big-bang w0
    [on-tick tick-handler 0.3]
    ;[on-key keyh]
    [to-draw tetris-render]
    ;[stop-when stop? stop-picture]
    [state #t]))


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
     

; -> Posn 
; ???
(define (block-generate x)
  (make-posn (+ 1 (random (- WIDTH 1))) 0))
 

(tetris-main (make-tetris (make-posn 1 1) '()))
