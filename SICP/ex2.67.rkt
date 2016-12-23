;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex2.67) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(define (make-leaf symbol weight) (list 'leaf symbol weight))
(define (leaf? object) (eq? (car object) 'leaf))
(define (symbol-leaf x) (cadr x))
(define (weight-leaf x) (caddr x))


(define (make-code-tree left right)
  (list left
        right
        (append (symbols left) (symbols right))
        (+ (weight left) (weight right))))


(define (left-branch tree) (car tree))
(define (right-branch tree) (cadr tree))
(define (symbols tree)
  (if (leaf? tree)
      (list (symbol-leaf tree))
      (caddr tree)))
(define (weight tree)
  (if (leaf? tree)
      (weight-leaf tree)
      (cadddr tree)))



(define (decode bits tree)
  (local (
          (define (decode-1 bits current-branch)
            (if (null? bits)
                '()
                (let ((next-branch
                       (choose-branch (car bits) current-branch)))
                  (if (leaf? next-branch)
                      (cons (symbol-leaf next-branch)
                            (decode-1 (cdr bits) tree))
                      (decode-1 (cdr bits) next-branch)))))
          )
    ;-----------------
    (decode-1 bits tree)))
(define (choose-branch bit branch)
  (cond ((= bit 0) (left-branch branch))
        ((= bit 1) (right-branch branch))
        (else (error "bad bit: CHOOSE-BRANCH" bit))))


(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((< (weight x) (weight (car set))) (cons x set))
        (else (cons (car set)
                    (adjoin-set x (cdr set))))))

(define (make-leaf-set pairs)
  (if (null? pairs)
      '()
      (let ((pair (car pairs)))
        (adjoin-set (make-leaf (car pair) ; symbol
                               (cadr pair)) ; frequency
                    (make-leaf-set (cdr pairs))))))

;ex2.67
(define sample-tree
  (make-code-tree (make-leaf 'A 4)
                  (make-code-tree
                   (make-leaf 'B 2)
                   (make-code-tree
                    (make-leaf 'D 1)
                    (make-leaf 'C 1)))))
(define sample-message '(0 1 1 0 0 1 0 1 0 1 1 1 0))
(decode sample-message sample-tree)

;ex2.68
(define (encode message tree)
  (if (null? message)
      '()
      (append (encode-symbol (car message) tree)
              (encode (cdr message) tree))))
(define (encode-symbol sym tree)
  (if (member? sym (symbols tree))
      (cond
        ((leaf? tree) '())
        ((member? sym (symbols (left-branch tree))) (cons '0 (encode-symbol sym (left-branch tree))))
        (else (cons '1 (encode-symbol sym (right-branch tree)))))
      (error "not in tree")))

(encode-symbol 'D sample-tree)


;ex2.69
(define (generate-huffman-tree pairs)
  (successive-merge (make-leaf-set pairs)))
(define (successive-merge pairs)
  (cond
    ((null? pairs) (error "input must not be empty"))
    ((= 1 (length pairs)) (car pairs))
    (else (successive-merge (adjoin-set (make-code-tree (if (pair? (car pairs))
                                                            (make-leaf (car (car pairs)) (cadr (car pairs)))
                                                            (car pairs))
                                                        (if (pair? (cadr pairs))
                                                            (make-leaf (car (cadr pairs)) (cadr (cadr pairs)))
                                                            (cadr pairs)))
                                        (cddr pairs))))))
  
  (define (pair? x)
    (= 2 (length x)))
  
  
  ;ex2.70
  (generate-huffman-tree '((A 2) (GET 2) (SHA 3) (WAH 1) (BOOM 1) (JOB 2) (NA 16) (YIP 9)))