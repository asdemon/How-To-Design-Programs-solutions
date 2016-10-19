;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex471) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D))
    (D ())
    (E (C F))
    (F (D G))
    (G ())))


; A Node is a Symbol.
; A Graph is one of:
; 1) '()
; 2) (cons (Node (list of Node)) Graph

; Node n , a Graph g -> list of Node
; It consumes a Node n and a Graph g and produces the list of immediate neighbors of n in g
(check-expect (neighbors sample-graph 'A)
              '(B E))
(define (neighbors g n)
  (match g
    ('() #f)
    ((cons h t) (if (symbol=? (car h) n)
                    (cadr h)
                    (neighbors t n)))))


; A Path is a [List-of Node].
; interpretation The list of nodes specifies a sequence
; of immediate neighbors that leads from the first 
; Node on the list to the last one. 
 
; Node Node Graph -> [Maybe Path]
; finds a path from origination to destination in G
; if there is no path, the function produces #false
 
(check-expect (find-path 'C 'D sample-graph)
              '(C D))
(check-member-of (find-path 'E 'D sample-graph)
                 '(E F D) '(E C D))
(check-expect (find-path 'C 'G sample-graph)
              #false)
 
(define (find-path origination destination G)
  (cond
    ((symbol=? origination destination) (cons destination '()))
    ((empty? (neighbors G origination)) #f)
    (else (if (member? destination (neighbors G origination))
              (cons origination (cons destination '()))
              (local (
                      (define re (map (lambda (origin) (find-path origin destination G)) (neighbors G origination)))
                      ; list -> element
                      ;find first value which is not boolean and return it, if list does not have one, return #f
                      (define (find-not-bool l)
                        (match l
                          ('() #f)
                          ((cons h t) (if (boolean? h)
                                          (find-not-bool t)
                                          h))))
                      (define find-res (find-not-bool re))
                      )
                ;----------------------
                (if (boolean? find-res)
                    #f
                    (cons origination find-res)))))))



(check-expect (abstract-find-path 'C 'D sample-graph neighbors)
              '(C D))
(check-member-of (abstract-find-path 'E 'D sample-graph neighbors)
                 '(E F D) '(E C D))
(check-expect (abstract-find-path 'C 'G sample-graph neighbors)
              #false)
(define (abstract-find-path origination destination G neighbors)
  (cond
    ((equal? origination destination) (cons destination '()))
    ((empty? (neighbors G origination)) #f)
    (else (if (member? destination (neighbors G origination))
              (cons origination (cons destination '()))
              (local (
                      (define re (map (lambda (origin) (abstract-find-path origin destination G neighbors)) (neighbors G origination)))
                      ; list -> element
                      ;find first value which is not boolean and return it, if list does not have one, return #f
                      (define (find-not-bool l)
                        (match l
                          ('() #f)
                          ((cons h t) (if (boolean? h)
                                          (find-not-bool t)
                                          h))))
                      (define find-res (find-not-bool re))
                      )
                ;----------------------
                (if (boolean? find-res)
                    #f
                    (cons origination find-res)))))))


; ex476
(define-struct transition [current key next])
(define-struct fsm [initial transitions final])
 
; A FSM is a structure:
;   (make-fsm FSM-State [List-of 1Transition] FSM-State)
; A 1Transition is a structure:
;   (make-transition FSM-State 1String FSM-State)
; A FSM-State is String.
 
 
 
; data example: see exercise 109
(define fsm-a-bc*-d
  (make-fsm
   "AA"
   (list (make-transition "AA" "a" "BC")
         (make-transition "BC" "b" "BC")
         (make-transition "BC" "c" "BC")
         (make-transition "BC" "d" "DD"))
   "DD"))

; FSM String -> Boolean 
; does a-fsm recognize the given string
(check-expect (fsm-match? fsm-a-bc*-d "abcd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "acd") #t)
(check-expect (fsm-match? fsm-a-bc*-d "ad") #t)
(define (fsm-match? a-fsm a-string)
  (local (
          (define (next-stat current a-key l)
            (match l
              ('() (error "error"))
              ((cons h t) (if (and (equal? (transition-current h) current)
                                   (equal? (transition-key h) a-key))
                              (transition-next h)
                              (next-stat current a-key t)))))
          (define final-state (foldr (lambda (key state) (next-stat state key (fsm-transitions a-fsm))) (fsm-initial a-fsm) (reverse (explode a-string))))
          )
    ;----------------------
    (string=? final-state (fsm-final a-fsm))))


; [List-of X] -> [List-of [List-of X]]
; creates a list of all rearrangements of the items in w
(define (arrangements w)
  (cond
    [(empty? w) '(())]
    [else
      (foldr (lambda (item others)
               (local ((define without-item
                         (arrangements (remove item w)))
                       (define add-item-to-front
                         (map (lambda (a) (cons item a))
                              without-item)))
                 (append add-item-to-front others)))
        '()
        w)]))