;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.41) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (unique-tribles n)
  (accumulate append '() (map (lambda (i)
                                (accumulate append '() (map (lambda (j)
                                                              (map (lambda (k)
                                                                     (list i j k))
                                                                   (enumerate-interval 1 (sub1 j))))
                                                              (enumerate-interval 1 (sub1 i)))))
                              (enumerate-interval 1 n))))
