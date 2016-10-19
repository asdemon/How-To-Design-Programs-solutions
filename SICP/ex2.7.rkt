;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.7) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define-struct interval [low high])

(define (lower-bound it)
  (interval-low it))

(define (upper-bound it)
  (interval-high it))

(define (add-interval x y)
  (make-interval (+ (lower-bound x) (lower-bound y))
                 (+ (upper-bound x) (upper-bound y))))

(define (mul-interval x y)
  (local (
          (define p1 (* (lower-bound x) (lower-bound y)))
          (define p2 (* (lower-bound x) (upper-bound y)))
          (define p3 (* (upper-bound x) (lower-bound y)))
          (define p4 (* (upper-bound x) (upper-bound y)))
          )
    ;---------------------------
    (make-interval (min p1 p2 p3 p4)
                   (max p1 p2 p3 p4))))


(define (div-interval x y)
  (mul-interval
   x
   (make-interval (/ 1.0 (upper-bound y))
                  (/ 1.0 (lower-bound y)))))

; ex2.10
(define (div-interval-revised x y)
  (if (< (lower-bound y) 0 (upper-bound y))
      (error "second argument cross 0")
      (mul-interval
       x
       (make-interval (/ 1.0 (upper-bound y))
                      (/ 1.0 (lower-bound y))))))

(define A (make-interval 1.4 1.6))
(define B (make-interval 1.95 2.05))
(define rep-A (make-interval (/ 1 1.4) (/ 1 1.6)))
(div-interval-revised  A A)
(div-interval-revised  A B)
(div-interval-revised (make-interval 1 3) (make-interval -2 4))