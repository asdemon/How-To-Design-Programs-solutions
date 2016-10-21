;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex1.11) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
;N -> N
;formulor see book
(define (f n)
  (cond
    ((< n 3) n)
    (else (+ (f (sub1 n)) (* 2 (f (- n 2))) (* 3 (f (- n 3)))))))

(define (f-iter n)
  (local (
          (define (f/i a b c count)
            (cond
              ((= count 0) c)
              (else (f/i (+ a (* 2 b) (* 3 c)) a b(sub1 count)))))
          )
    (f/i 2 1 0 n)))