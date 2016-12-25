#lang sicp
;(#%require racket/base)
(#%require lang/htdp-advanced)

(define (monte-carlo trials experiment)
  (local (
          (define (iter trials-remaining trials-passed)
            (cond ((= trials-remaining 0)
                   (/ trials-passed trials))
                  ((experiment)
                   (iter (- trials-remaining 1)
                         (+ trials-passed 1)))
                  (else
                   (iter (- trials-remaining 1)
                         trials-passed))))
          )
    ;------------------
    (iter trials 0)))

(define (random-in-range low high)
  (let ((range (- high low)))
    (+ low (* (random) range))))



(define (estimate-integral P x1 x2 y1 y2 num-of-trials)
  (local (
          (define (integral-test)
            (let ((x (random-in-range x1 x2))
                  (y (random-in-range y1 y2)))
              (P x y)))
          )
    ;------------------------
    (* (monte-carlo num-of-trials integral-test)
       (- y2 y1)
       (- x2 x1))))

(estimate-integral (lambda (x y) (<= (+ (sqr x) (sqr y)) 1)) -1.0 1.0 -1.0 1.0 100000)