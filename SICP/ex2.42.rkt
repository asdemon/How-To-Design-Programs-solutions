;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2.42) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))

(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))

(define (enumerate-interval low high)
  (if (> low high)
      '()
      (cons low (enumerate-interval (+ low 1) high))))

(define (flatmap proc seq)
  (accumulate append '() (map proc seq)))

(define empty-board '())

(define (adjoin-position new-row k rest-of-queens)
  (append rest-of-queens `((,new-row ,k))))

(define (last l)
  (local (
          (define (last/a l a)
            (if (empty? l)
                a
                (last/a (cdr l) (car l))))
          )
    ;----------
    (last/a l '())))

(define (safe? k positions)
  (local (
          (define last-ele (last positions))
          (define lRow (car last-ele))
          (define lCol (cadr last-ele))
          )
    ;-----------------
    (andmap (lambda (p)
              (local (
                      (define row (car p))
                      (define col (cadr p))
                      )
                ;--------------
                (and (not (= row lRow))
                     (not (= (abs (- row lRow)) (abs (- col lCol)))))))
            (remove last-ele positions))))


(define (queens board-size)
  (local (
          (define (queen-cols k)
            (if (= k 0)
                (list empty-board)
                (filter
                 (lambda (positions) (safe? k positions))
                 (flatmap
                  (lambda (rest-of-queens)
                    (map (lambda (new-row)
                           (adjoin-position
                            new-row k rest-of-queens))
                         (enumerate-interval 1 board-size)))
                  (queen-cols (- k 1))))))
          )
    ;------------------
    (queen-cols board-size)))

(queens 8)
