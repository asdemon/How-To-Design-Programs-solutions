;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex462) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)

; An SOE is a non-empty Matrix.
; constraint for (list r1 ... rn), (length ri) is (+ n 1)
; interpretation represents a system of linear equations

; An Equation is a [List-of Number].
; constraint an Equation contains at least two numbers. 
; interpretation if (list a1 ... an b) is an Equation, 
; a1, ..., an are the left-hand side variable coefficients 
; and b is the right-hand side

; A Solution is a [List-of Number]

(define M ; an SOE 
  (list (list 2 2  3 10) ; an Equation 
        (list 2 5 12 31)
        (list 4 1 -2  1)))

(define S '(1 1 2)) ; a Solution


; Equation -> [List-of Number]
; extracts the left-hand side from a row in a matrix
(check-expect (lhs (first M)) '(2 2 3))
(define (lhs e)
  (reverse (rest (reverse e))))

; Equation -> Number
; extracts the right-hand side from a row in a matrix
(check-expect (rhs (first M)) 10)
(define (rhs e)
  (first (reverse e)))

; ex462
; SOE, Solution -> boolean
; Its result is #true if plugging in the numbers from the Solution for the variables in
; the Equations of the SOE produces equal left-hand side values and right-hand side values;
; otherwise the function produces #false
(define (check-solution soe s)
  (andmap (lambda (l) (= (for/sum ([i (lhs l)] [j s]) (* i j)) (rhs l))) soe))


(define M1
  (list (list 2 2  3 10)
        (list   0 3  9 21)
        (list   0 0  1  2)))

(define M2
  (list (list 2 2  3 10)
        (list   0 3  9 21)
        (list   0 -3 -8  -19)))

; ex465
; Equation, Equation -> Equation
; The function consumes two Equations of equal length. It subtracts the second from the first, item by item, as many times as necessary to obtain an Equation with a 0 in the first position
(check-expect (subtract (list 2 2  3 10) (list 2 5 12 31))
              (list 3  9 21))
(define (subtract e1 e2)
  (cdr (sub e2 (mul-a e1 (/ (car e2) (car e1))))))

; vector l1 subtract vector l2, return l2 - l1
(define (sub l1 l2)
  (for/list ((i l1) (j l2))
    (- i j)))
; vector l1 mul number a, return l1 * a
(define (mul-a l1 a)
  (for/list ((i l1))
    (* i a)))


; ex466
; A TM is a [List-of Equation]
; such that the Equations are of decreasing length: 
;   n + 1, n, n - 1, ..., 2. 
; interpretation represents a triangular matrix

; SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate M)
              (list (list 2 2  3 10)
                    (list   3  9 21)
                    (list      1  2))
              )
(define (triangulate M)
  (if (= (length M) 1)
      M
      (local (
              ;Equation -> Equation
              ;将第一列消为0
              (define (guass-eli-first m)
                (cons (car m)
                      (map (lambda (x) (subtract (car m) x)) (cdr m))))
              
              (define re (guass-eli-first M))
              )
        ;---------------------------
        (cons (car M) (triangulate (cdr re))))))


; ex467
; [List-of X] N -> [List-of X]
; keep the first n items from l if possible or everything
(define (take l n)
  (cond
    [(zero? n) '()]
    [(empty? l) '()]
    [else (cons (first l) (take (rest l) (sub1 n)))]))

; [List-of X] N -> [List-of X]
; remove the first n items from l if possible or everything
(define (drop l n)
  (cond
    [(zero? n) l]
    [(empty? l) l]
    [else (drop (rest l) (sub1 n))]))
; SOE -> TM
; triangulates the given system of equations
(check-expect (triangulate-with-rotate (list (list 2  3  3 8)
                                             (list 2  3 -2 3)
                                             (list 4 -2  2 4)))
              (list (list 2 3 3 8)
                    (list -8 -4 -12)
                    (list -5 -5)))
(define (triangulate-with-rotate M)
  (if (= (length M) 1)
      M
      (local (
              ;Equation -> Equation
              ;将第一列消为0
              (define (guass-eli-first m)
                (cons (car m)
                      (map (lambda (x) (subtract (car m) x)) (cdr m))))
              ;找到第一个非0的行
              (define (find-not-0 M)
                (if (empty?  M)
                    #f
                    (local ((define re (find-not-0 (cdr M))))
                      (if (not (= (caar M) 0))
                          0
                          (if (boolean? re)
                              #f
                              (+ 1 re))))))
              
              
              ;Equation -> Equation
              ;将左上角的元素交换为非0
              (define (rot M)
                (if (not (= (car (car M)) 0))
                    M
                    (local
                      ((define re (find-not-0 M)))
                      ;------------
                      (if (boolean? re)
                          (error "all 0s")
                          (append (drop M (find-not-0 M)) (take M (find-not-0 M)))))))
              (define rotM (rot M))
              (define re (guass-eli-first rotM))
              )
        ;---------------------------
        (cons (car rotM) (triangulate-with-rotate (cdr re))))))



;(define (find-not-0 M)
;  (if (empty?  M)
;      #f
;      (local ((define re (find-not-0 (cdr M))))
;        (if (not (= (caar M) 0))
;            0
;            (if (boolean? re)
;                #f
;                (+ 1 re))))))
;(define (rot M)
;  (if (not (= (car (car M)) 0))
;      M
;      (local
;        ((define re (find-not-0 M)))
;        ;------------
;        (if (boolean? re)
;            (error "all 0s")
;            (append (drop M (find-not-0 M)) (take M (find-not-0 M)))))))

; ex469
;TM -> Solution
;It consumes triangular systems of equations and produces a solution.
(check-expect (solve (triangulate-with-rotate M)) S)
;点乘
(define (dot x y)
  (for/sum ([i x] [j y]) (* i j)))
(define (solve tm)
  (cond
    ((empty? (cdr tm)) (list (/ (cadar tm) (caar tm))))
    (else (local (
                  (define re (solve (cdr tm)))
                  ;first row
                  (define first-row (car tm))
                  ;calc first element
                  (define x (/ (- (rhs first-row) (dot re (cdr (lhs first-row)))) (car (lhs first-row))))
                  )
            ;-------------
            (cons x re)))))


; ex470
(define (gauss M)
  (solve (triangulate-with-rotate M)))