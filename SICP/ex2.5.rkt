;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.5) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (cons1 x y)
  (* (expt 2 x) (expt 3 y)))


(define (car1 z)
  (local (
          (define (divide-by-2 n acc)
            (if (even? n)
                (divide-by-2 (/ n 2) (add1 acc))
                acc))
          )
    ;---------------------
    (divide-by-2 z 0)))

(define (cdr1 z)
  (local (
          (define (divide-by-3 n acc)
            (if (= (remainder n 3) 0)
                (divide-by-3 (/ n 3) (add1 acc))
                acc))
          )
    ;---------------------
    (divide-by-3 z 0)))