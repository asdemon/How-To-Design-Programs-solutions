;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname nChoosek) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define (nChoosek l k)
  (cond
    ((= k 1) (map (lambda (x) (list x)) l))
    ((= (length l) k) (list l))
    (else (append (map (lambda (l0) (cons (car l) l0)) (nChoosek (cdr l) (sub1 k)))
              (nChoosek (cdr l) k)))))


(nChoosek '(1 2 3 4 5) 2)