;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.37) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (cont-frac n d k)
  (local (
          ;N -> N
          ;calc the frac, count is current number,form k to 0, a is the accumulator
          (define (cont-frac/a count n d k a)
            (cond
              ((= count 0) a)
              (else (cont-frac/a (sub1 count) n d k (/ (n count) (+ (d count) a))))))
          )
    ;----------------------
    (cont-frac/a k n d k 0)))


; ex1.38
(define (simu/e)
  (+ 2 (cont-frac (lambda (i) 1.0)
                  (lambda (i)
                    (cond
                      ((= 2 (remainder i 3)) (* 2.0 (/ (add1 i) 3)))
                      (else 1.0)))
                  100)))