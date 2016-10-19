;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex1.29) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (sum term a next b)
  (if (> a b)
      0
      (+ (term a)
         (sum term (next a) next b))))

; simpson's rule
(define (integral-simppson f a b n)
  (local (
          (define h (/ (- b a) (* 1.0 n)))
          ;each addition's value
          (define (add-f start)
            (+ (f start) (* 4 (f (+ start h))) (f (+ start h h))))
          ;next
          (define (next start)
            (+ start h h))
          )
    ;---------------------------
    (* (sum add-f a next b) h (/ 1.0 3.0))))


(define (cube x) (* x x x))

(integral-simppson cube 0 1 1000)