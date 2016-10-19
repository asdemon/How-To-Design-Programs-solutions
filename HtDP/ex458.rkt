;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex458) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define ε 0.01)

; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 

;(check-within (integrate (lambda (x) 20) 12 22) 200 ε)
;(check-within (integrate (lambda (x) (* 2 x)) 0 10) 100 ε)
;(check-within (integrate (lambda (x) (* 3 (sqr x))) 0 10)
;              1000
;              ε)
; 
;(define (integrate f a b) #i0.0)



; ex458
; [Number -> Number] Number Number -> Number
; function f, number l, number r -> area of f between r and l and x
(define (integrate-kepler-helper f l r)
  (* 0.5 (* (- r l) (+ (f l) (f r)))))
; function, number, number -> number
; compute an estimate of the area under f between a and b
; assume (< a b) holds 
(check-within (integrate-kepler (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-kepler (lambda (x) (* 2 x)) 0 10) 100 ε)
;(check-within (integrate-kepler (lambda (x) (* 3 (sqr x))) 0 10)
;              1000
;              ε)
(define (integrate-kepler f a b)
  (+ (integrate-kepler-helper f a (/ (+ a b) 2))
     (integrate-kepler-helper f (/ (+ a b) 2) b)))



; ex459
(define R 500)
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 

(check-within (integrate-459 (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-459 (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-459 (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)

;(define (integrate-459 f a b)
;  (local
;    (
;     ;number -> number
;     ;第n个矩形的面积
;     (define (rect-area n)
;       (local
;         ((define W (/ (- b a) R))
;          (define S (/ W 2)))
;         ;--------------
;       (* W (f (+ a S (* n W))))))
;     ;number -> number
;     ;对n个矩形进行累加
;     (define (acc m)
;       (if (= m 0)
;           (rect-area 0)
;           (+ (rect-area m) (acc (sub1 m)))))
;     )
;    ;------
;    (acc (sub1 R))))

(define (integrate-459 f a b)
  (local
    (
     ;number -> number
     ;第n个矩形的面积
     (define (rect-area n)
       (local
         ((define W (/ (- b a) R))
          (define S (/ W 2)))
         ;--------------
         (* W (f (+ a S (* n W))))))
     )
    ;------
    (apply + (build-list R rect-area))))



; ex460
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 

(check-within (integrate-dc (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-dc (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-dc (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)

(define (integrate-dc f a b)
  (cond
    ((< (abs (- a b)) ε) (integrate-kepler f a b) )
    (else (local (
                  (define mid (/ (+ a b) 2))
                  )
            ;------------------
            (+ (integrate-dc f a mid)
               (integrate-dc f mid b))))))


; ex461
; [Number -> Number] Number Number -> Number
; computes the area under the graph of f between a and b
; assume (< a b) holds 

(check-within (integrate-adaptive (lambda (x) 20) 12 22) 200 ε)
(check-within (integrate-adaptive (lambda (x) (* 2 x)) 0 10) 100 ε)
(check-within (integrate-adaptive (lambda (x) (* 3 (sqr x))) 0 10)
              1000
              ε)

(define (integrate-adaptive f a b)
  (local (
          ;将a,b之间的整个当成一个梯形来计算
          (define all-area (* (- b a) (/ (+ (f a) (f b)) 2)))
          ;使用kepler方法计算
          (define kepler-area (integrate-kepler f a b))
          )
    ;-------------------------------
    (if (< (abs (- all-area kepler-area)) (/ (* (- b a) ε) 2))
        kepler-area
        (local (
                (define mid (/ (+ a b) 2))
                )
          ;------------------
          (+ (integrate-adaptive f a mid)
             (integrate-adaptive f mid b))))))
