;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex493) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

(define sample-graph
  '((A (B E))
    (B (E F))
    (C (D B))
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

(define (find-path origination destination G)
  (local (
          (define (find-path-inter origination destination G seen)
            (cond
              ((symbol=? origination destination) (cons destination '()))
              ((empty? (neighbors G origination)) #f)
              ((member? origination seen) #f)
              (else (if (member? destination (neighbors G origination))
                        (cons origination (cons destination '()))
                        (local (
                                (define re (map (lambda (origin) (find-path-inter origin destination G (cons origination seen))) (neighbors G origination)))
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
          )
    ;---------------------------------
    (find-path-inter origination destination G '())))