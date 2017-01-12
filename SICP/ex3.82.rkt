#lang sicp
(#%require (only racket/base error random))

(define (stream-car stream) (car stream))
(define (stream-cdr stream) (force (cdr stream)))



(define (stream-ref s n)
  (if (= n 0)
      (stream-car s)
      (stream-ref (stream-cdr s) (- n 1))))
(define (stream-map proc . argstreams)
  (if (stream-null? (car argstreams))
      the-empty-stream
      (cons-stream
       (apply proc (map stream-car argstreams))
       (apply stream-map
              (cons proc (map stream-cdr argstreams))))))
(define (stream-for-each proc s)
  (if (stream-null? s)
      'done
      (begin (proc (stream-car s))
             (stream-for-each proc (stream-cdr s)))))
(define (stream-enumerate-interval low high)
  (if (> low high)
      the-empty-stream
      (cons-stream
       low
       (stream-enumerate-interval (+ low 1) high))))
(define (stream-filter pred stream)
  (cond ((stream-null? stream) the-empty-stream)
        ((pred (stream-car stream))
         (cons-stream (stream-car stream)
                      (stream-filter
                       pred
                       (stream-cdr stream))))
        (else (stream-filter pred (stream-cdr stream)))))

;display the previous n element of stream s
(define (display-stream-1 s n)
  (stream-for-each (lambda (n) (display-line (stream-ref s n))) (stream-enumerate-interval 0 (- n 1))))
(define (display-stream s)
  (stream-for-each display-line s))
(define (display-line x) (newline) (display x))

(define (add-streams a b)
  (stream-map + a b))

(define ones (cons-stream 1 ones))

(define (integers-starting-from n)
  (cons-stream n (add-streams ones (integers-starting-from n))))

(define integers (integers-starting-from 1))


(define (mul-streams a b)
  (stream-map * a b))

(define (partial-sums s)
  (cons-stream
   (stream-car s)
   (add-streams (stream-cdr s) (partial-sums s))))

(define (scale-stream stream factor)
  (stream-map (lambda (x) (* x factor))
              stream))

;contruct a stream of random number with init
(define (random-numbers random-init)
  (define s
    (cons-stream
     random-init
     (stream-map (lambda (x) (random)) s)))
  s)

(define (rand-update x)
  (modulo  (+ 111 (* 417 x)) 4747))



(define (monte-carlo experiment-stream passed failed)
  (define (next passed failed)
    (cons-stream
     (/ (* passed 1.0) (+ passed failed))
     (monte-carlo
      (stream-cdr experiment-stream) passed failed)))
  (if (stream-car experiment-stream)
      (next (+ passed 1) failed)
      (next passed (+ failed 1))))

(define (random-in-range-stream low high)
  (stream-map (lambda (x) (+ low (* x (- high low)))) (random-numbers (random))))


(define (estimate-integral P x1 x2 y1 y2)
  (define s1 (random-in-range-stream x1 x2))
  (define s2 (random-in-range-stream y1 y2))
  ;------------------------
  (scale-stream (monte-carlo (stream-map P s1 s2) 0 0)
     (* (- y2 y1) (- x2 x1))))


(define (square x)
  (* x x))
(define w1 (estimate-integral (lambda (x y) (<= (+ (square x) (square y)) 1)) -1.0 1.0 -1.0 1.0))
(stream-ref w1 30000)