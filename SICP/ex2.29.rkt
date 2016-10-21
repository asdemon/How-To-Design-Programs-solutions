;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-advanced-reader.ss" "lang")((modname ex2.29) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #t #t none #f () #f)))
(define (make-mobile left right)
  (list left right))


(define (make-branch length structure)
  (list length structure))

; a
(define (left-branch mobile)
  (car mobile))

(define (right-branch mobile)
  (cadr mobile))

(define (branch-length br)
  (car br))

(define (branch-structure br)
  (cadr br))

; b
(define (total-weight mobile)
  (local (
          ;left weight
          (define left-weight
            (local (
                    (define re (branch-structure (left-branch mobile)))
                    )
              ;--------------
            (if (number? re)
                re
                (total-weight re))))
          ;right weight
          (define right-weight
            (local (
                    (define re (branch-structure (right-branch mobile)))
                    )
              ;--------------
            (if (number? re)
                re
                (total-weight re))))
          )
    ;-------------------------
    (+ left-weight right-weight)))


; c
(define (balanced? mobile)
  (and ( < (abs (- (* (branch-length (left-branch mobile)) (total-weight (branch-structure (left-branch mobile))))
                   (* (branch-length (right-branch mobile)) (total-weight (branch-structure (right-branch mobile))))))
           1e-4)
       (if (number? (branch-structure (right-branch mobile)))
           #t
           (balanced? (branch-structure (right-branch mobile))))
       (if (number? (branch-structure (left-branch mobile)))
           #t
           (balanced? (branch-structure (left-branch mobile))))))
       
