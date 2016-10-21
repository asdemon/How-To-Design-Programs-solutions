;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.31) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (tree-map f tree)
  (if (cons? tree)
      (map (lambda (tr) (tree-map f tr)) tree)
      (f tree)))


(define (square-tree tree) (tree-map sqr tree))