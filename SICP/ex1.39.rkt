;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.39) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (tan-cf x k)
  (local (
          ;N -> N
          ;calc the frac, count is current number,form k to 0, a is the accumulator
          (define (cont-frac/a count n d k a)
            (cond
              ((= count 0) a)
              (else (cont-frac/a (sub1 count) n d k (/ (n count) (- (d count) a))))))

          (define (n i) (if (= i 1) x (sqr x)))
          (define (d i) (- (* 2 i) 1))
          )
    ;----------------------
    (cont-frac/a k n d k 0)))

