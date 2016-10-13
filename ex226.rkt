;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-reader.ss" "lang")((modname ex226) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/universe)
(require 2htdp/image)

; A FSM is one of:
;   – '()
;   – (cons Transition FSM)
 
(define-struct transition [current next])
; A Transition is a structure:
;   (make-transition FSM-State FSM-State)
 
; FSM-State is a Color.
 
; interpretation A FSM represents the transitions that a
; finite state machine can take from one state to another 
; in reaction to key strokes
(define fsm-traffic
  (list (make-transition "red" "green")
        (make-transition "green" "yellow")
        (make-transition "yellow" "red")))

; ex226
(define (state? s)
  (image-color? s))

; ex228
(define-struct fs [fsm current])
; A SimulationState.v2 is a structure: 
;   (make-fs FSM FSM-State)


; SimulationState.v2 -> Image 
; renders current world state as a colored square 
 
(check-expect (state-as-colored-square
                (make-fs fsm-traffic "red"))
              (square 100 "solid" "red"))
 
(define (state-as-colored-square a-fs)
  (square 100 "solid" (fs-current a-fs)))
 
; SimulationState.v2 KeyEvent -> SimulationState.v2
; finds the next state from ke and cs
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "n")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "red") "a")
  (make-fs fsm-traffic "green"))
(check-expect
  (find-next-state (make-fs fsm-traffic "green") "q")
  (make-fs fsm-traffic "yellow"))
(define (find-next-state a-fs ke)
  (make-fs
    (fs-fsm a-fs)
    (find (fs-fsm a-fs) (fs-current a-fs))))
; FSM FSM-State -> FSM-State
; finds the state matching current in the table
 
(check-expect (find fsm-traffic "red") "green")
(check-expect (find fsm-traffic "green") "yellow")
(check-error (find fsm-traffic "black")
             "not found: black")
 
(define (find transitions current)
  (cond
    ((empty? transitions) (error (string-append "not found: " current)))
    (else (if (string=? (transition-current (first transitions)) current)
              (transition-next (first transitions))
              (find (cdr transitions) current)))))

; FSM FSM-State -> SimulationState.v2 
; match the keys pressed with the given FSM 
(define (simulate.v2 a-fsm s0)
  (big-bang (make-fs a-fsm s0)
    [to-draw state-as-colored-square]
    [on-key find-next-state]))


(simulate.v2 fsm-traffic "red")