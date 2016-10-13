;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex293) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
; X [List-of X] -> [Maybe [List-of X]]
; returns the first sublist of l that starts
; with x, #false otherwise
(define (find x l)
  (cond
    [(empty? l) #false]
    [else
     (if (equal? (first l) x) l (find x (rest l)))]))


(define (find? x l)
  (lambda (r0)
    (if (member? x l)
        (and (equal? (first r0) x)
             (contains? l r0)
             (= (length l) (+ (index x l) (length r0))))
        (boolean? r0))))



; X [List-of X] -> [Maybe N]
; determine the index of the first occurrence
; of x in l, #false otherwise
(define (index x l)
  (cond
    [(empty? l) #false]
    [else (if (equal? (first l) x)
              0
              (local ((define i (index x (rest l))))
                (if (boolean? i) i (+ i 1))))]))


; [List-of X] [List-of X] -> Boolean 
; are all items in list k members of list l
 
(check-expect (contains? '(1 2 3) '(1 4 3)) #false)
(check-expect (contains? '(1 2 3 4) '(1 3)) #true)
 
(define (contains? l k)
  (andmap (lambda (in-k) (member? in-k l)) k))


(check-satisfied (find 1 '(0 1 2))
                 (find? 1 '(0 1 2)))
(check-satisfied (find 1 '(0 1 1 2))
                 (find? 1 '(0 1 1 2)))
(check-satisfied (find 5 '(0 1 1 2))
                 (find? 5 '(0 1 1 2)))