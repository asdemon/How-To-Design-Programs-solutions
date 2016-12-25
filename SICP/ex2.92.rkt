#lang racket
(require trace)

(define (list-index-self element list)
  (cond
    ((null? list) #f)
    ((eqv? element (car list)) 0)
    (else (+ 1
             (list-index-self  element (cdr list))))))

; use a list to represent the tower, the index is the level and low level can coerce to high level
(define level-list '(rational scheme-number complex polynomial-dense polynomial-sparse ))

(define (apply-generic op . args)
  (define (reduce-type x)
    (cond ((eq? op 'add) (drop x))
          ((eq? op 'sub) (drop x))
          ((eq? op 'mul) (drop x))
          ((eq? op 'div) (drop x))
          (else x)))
  
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let* ((indices (map (lambda (x) (begin (display x)(list-index-self x level-list))) type-tags))
                 (maxIndex (apply max indices)))
            (if (andmap (lambda (x) (= x maxIndex)) indices)
                (error "No method for these types: APPLY-GENERIC"
                       (list op type-tags))
                (apply (curry apply-generic op) (map (lambda (x) (if (< (list-ref indices (list-index-self (type-tag x) type-tags)) maxIndex)
                                                                     (raise x)
                                                                     x)) args))))))))

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (negative x) (apply-generic 'negative x))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zeros? x) (apply-generic '=zeros? x))
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))
;only have rational -> real -> complex
(define (raise x) (apply-generic 'raise x))
(define (project x) (apply-generic 'project x))

(define global-array '())

(define (make-entry k v) (list k v))
(define (key entry) (car entry))
(define (value entry) (cadr entry))

(define (put op type item)
  (define (put-helper k array)
    (cond ((null? array) (list(make-entry k item)))
          ((equal? (key (car array)) k) array)
          (else (cons (car array) (put-helper k (cdr array))))))
  (set! global-array (put-helper (list op type) global-array)))

(define (get op type)
  (define (get-helper k array)
    (cond ((null? array) #f)
          ((equal? (key (car array)) k) (value (car array)))
          (else (get-helper k (cdr array)))))
  (get-helper (list op type) global-array))
; from http://stackoverflow.com/questions/5499005/how-do-i-get-the-functions-put-and-get-in-sicp-scheme-exercise-2-78-and-on

(define (attach-tag type-tag contents)
  (cond ((eqv? type-tag 'scheme-number) contents)
        (else (cons type-tag contents))))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error "Bad tagged datum: TYPE-TAG" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error "Bad tagged datum: CONTENTS" datum))))


(define (install-scheme-number-package)
  (define (tag x) (attach-tag 'scheme-number x))
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number (lambda (x) (tag x)))
  (put 'raise '(scheme-number)
       (lambda (x) (make-complex-from-real-imag x 0)))
  (put 'negative '(scheme-number)
       (lambda (x) (- 0 x)))
  'done)

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))

(define (install-rectangular-package)
  
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  
  (define (make-from-real-imag x y) (cons x y))
  
  (define (magnitude z)
    (sqrt (+ (sqr (real-part z))
             (sqr (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  
  (define (make-from-mag-ang r a) 
    (cons (* r (cos a)) (* r (sin a))))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  
  (put 'make-from-real-imag 'rectangular 
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  'done)

(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (install-polar-package)
  
  ;; internal procedures
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  
  (define (make-from-mag-ang r a) (cons r a))
  
  (define (real-part z)
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  
  (define (make-from-real-imag x y) 
    (cons (sqrt (+ (sqr x) (sqr y)))
          (atan y x)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar 
       (lambda (r a) (tag (make-from-mag-ang r a))))
  
  'done)

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))

(define (install-complex-package)
  
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  
  (define (negative-complex z1)
    (make-from-real-imag (- 0 (real-part z1))
                         (- 0 (imag-part z1))))
  
  ;; interface to rest of the system
  (define (tag z)
    (attach-tag 'complex z))
  
  (put 'add '(complex complex)
       (lambda (z1 z2)
         (tag (add-complex z1 z2))))
  
  (put 'sub '(complex complex)
       (lambda (z1 z2)
         (tag (sub-complex z1 z2))))
  
  (put 'mul '(complex complex)
       (lambda (z1 z2)
         (tag (mul-complex z1 z2))))
  
  (put 'div '(complex complex)
       (lambda (z1 z2)
         (tag (div-complex z1 z2))))
  
  (put 'make-from-real-imag 'complex
       (lambda (x y)
         (tag (make-from-real-imag x y))))
  
  (put 'make-from-mag-ang 'complex
       (lambda (x y)
         (tag (make-from-mag-ang x y))))
  
  (put 'real-part '(complex) real-part)
  
  (put 'negative '(complex) negative-complex)
  (put 'raise '(complex)
       (lambda (x) (make-polynomial-dense 'qwer `,(x))))
  
  'done)

(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))

(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y))
                 (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y))
              (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y))
              (* (denom x) (numer y))))
  (define (negative-rat x)
    (make-rat (- 0 (numer x))
              (denom x) ))
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'numer '(rational)
       numer)
  (put 'denom '(rational)
       denom)
  (put 'raise '(rational)
       (lambda (x) (make-scheme-number (/ (numer x) (denom x)))))
  (put 'negative '(rational) negative-rat)
  'done)
(define (make-rational n d)
  ((get 'make 'rational) n d))

;ex2.79
(define (install-equ)
  ;; internal procedures
  ;; interface to the rest of the system
  
  (put 'equ? '(complex complex)
       (lambda (x y) (and (= (real-part x) (real-part y))
                          (= (imag-part x) (imag-part y)))))
  (put 'equ? '(scheme-number scheme-number)
       (lambda (x y) (= y x)))
  (put 'equ? '(rational rational)
       (lambda (x y) (equal? x y)));here We use the rational number's represntation, I can't find a better way to decouple it except add an abstract layer
  'done)

;ex2.80
(define (install-zeros)
  ;; internal procedures
  ;; interface to the rest of the system
  
  (put '=zeros? '(complex)
       (lambda (x) (and (= (real-part x) 0)
                        (= (imag-part x) 0))))
  (put '=zeros? '(scheme-number)
       (lambda (x) (= 0 x)))
  (put '=zeros? '(rational)
       (lambda (x) (and (= (car x) 0)
                        (not (= (cdr x) 0)))));here We use the rational number's represntation, I can't find a better way to decouple it except add an abstract layer
  'done)


;ex2.85
(define (install-project)
  ;; internal procedures
  ;; interface to the rest of the system
  
  (put 'project '(complex)
       (lambda (x) (real-part x)))
  (put 'project '(scheme-number)
       (lambda (x) (make-rational (round (* x 1e5)) 1e5)))
  
  'done)

(define (drop x)
  (cond
    ((boolean? x) x)
    ((eqv? (type-tag x) (list-ref level-list 0)) x)
    ((equ? x (raise (project x))) (drop (project x)))
    (else x)))
;(define (drop x) x)


(define (install-polynomial-dense-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (=zeros-p? p)
    (andmap (lambda (x) (=zeros?  x)) (term-list p)))
  
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  ;   (define (make-term order coeff) (list order coeff))
  ;  (define (order term) (car term))
  ;  (define (coeff term) (cadr term))
  
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((order1 (length L1))
                 (order2 (length L2)))
             (cond ((> order1 order2)
                    (cons
                     (first-term L1) (add-terms (rest-terms L1) L2)))
                   ((< order1 order2)
                    (cons
                     (first-term L2) (add-terms  L1 (rest-terms L2))))
                   (else
                    (map + L1 L2)))))))
  (define (mul-terms L1 L2)
    (foldr add-terms '() (for/list ((i (length L2)) (j (reverse L2)))
                           (append (map (lambda (x) (* j x)) L1) (make-list i 0)))))
  
  
  (define (negative-terms L1)
    (make-poly (variable L1)
               (map (lambda (x) (negative  x)) (term-list L1) )))
  
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial-dense p))
  (put 'add '(polynomial-dense polynomial-dense)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial-dense polynomial-dense)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put '=zeros? '(polynomial-dense) =zeros-p?)
  (put 'negative '(polynomial-dense)
       (lambda (p1) (tag (negative-terms p1))))
  (put 'sub '(polynomial-dense polynomial-dense)
       (lambda (p1 p2) (tag (add-poly p1 (negative-terms p2)))))
  (put 'make 'polynomial-dense
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(define (make-polynomial-dense var terms)
  ((get 'make 'polynomial-dense) var terms))

;sparse
(define (install-polynomial-sparse-package)
  ;; internal procedures
  ;; representation of poly
  (define (make-poly variable term-list) (cons variable term-list))
  (define (variable p) (car p))
  (define (term-list p) (cdr p))
  
  (define (=zeros-p? p)
    (andmap (lambda (x) (=zeros? (coeff x))) (term-list p)))
  
  
  (define (variable? x) (symbol? x))
  (define (same-variable? v1 v2)
    (and (variable? v1) (variable? v2) (eq? v1 v2)))
  ;; representation of terms and term lists
  (define (order term) (car term))
  (define (coeff term) (cadr term))
  (define (adjoin-term term term-list)
    (if (=zeros? (coeff term))
        term-list
        (cons term term-list)))
  (define (the-empty-termlist) '())
  (define (first-term term-list) (car term-list))
  (define (rest-terms term-list) (cdr term-list))
  (define (empty-termlist? term-list) (null? term-list))
  (define (make-term order coeff) (list order coeff))
  
  (define (add-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (add-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: ADD-POLY" (list p1 p2))))
  (define (mul-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (make-poly (variable p1)
                   (mul-terms (term-list p1) (term-list p2)))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  (define (div-poly p1 p2)
    (if (same-variable? (variable p1) (variable p2))
        (let ((div-result (div-terms (term-list p1) (term-list p2))))
          (list (make-poly (variable p1) (car div-result))
                (make-poly (variable p1) (cadr div-result))   ))
        (error "Polys not in same var: MUL-POLY" (list p1 p2))))
  ;if polynomial coeff has the var in it, this fuction return true, otherwise return false
  ; this is for sparse represent only
  (define (poly-coeff-has-var poly var)
    (ormap (lambda (x) (cond
                         ((eqv? (variable poly) var) #t)
                         ((eqv? (type-tag (coeff x)) 'polynomial-sparse) (poly-coeff-has-var (contents (coeff x)) var))
                         (else #f)))
           (term-list poly)))
  
  (define (tranlate poly var1 var2)
    (make-poly var2 (foldr add-terms '() (map (lambda (x) (if (poly-coeff-has-var (coeff x) var2)
                                                              (map (lambda (y) (make-term (order y) (mul (coeff y) (make-polynomial-sparse var1 (list (make-term (order x) 1)))))) (term-list (contents (coeff x))))
                                                              (list (make-term 0 (mul (coeff x) (make-polynomial-sparse var1 (list (make-term (order x) 1))))))))
                                              (term-list poly)))))
  
  
  (define (add-terms L1 L2)
    (cond ((empty-termlist? L1) L2)
          ((empty-termlist? L2) L1)
          (else
           (let ((t1 (first-term L1))
                 (t2 (first-term L2)))
             (cond ((> (order t1) (order t2))
                    (adjoin-term
                     t1 (add-terms (rest-terms L1) L2)))
                   ((< (order t1) (order t2))
                    (adjoin-term
                     t2 (add-terms L1 (rest-terms L2))))
                   (else
                    (adjoin-term
                     (make-term (order t1)
                                (add (coeff t1) (coeff t2)))
                     (add-terms (rest-terms L1)
                                (rest-terms L2)))))))))
  
  (define (negative-poly L1)
    (make-poly (variable L1)
               (map (lambda (x) (make-term (order x) (negative (coeff x)))) (term-list L1) )))
  
  (define (negative-terms L1)
    (map (lambda (x) (make-term (order x) (negative (coeff x))))  L1 ))
  
  (define (mul-terms L1 L2)
    (if (empty-termlist? L1)
        (the-empty-termlist)
        (add-terms (mul-term-by-all-terms (first-term L1) L2)
                   (mul-terms (rest-terms L1) L2))))
  (define (mul-term-by-all-terms t1 L)
    (if (empty-termlist? L)
        (the-empty-termlist)
        (let ((t2 (first-term L)))
          (adjoin-term
           (make-term (+ (order t1) (order t2))
                      (mul (coeff t1) (coeff t2)))
           (mul-term-by-all-terms t1 (rest-terms L))))))
  
  (define (div-terms L1 L2)
    (if (empty-termlist? L1)
        (list (the-empty-termlist) (the-empty-termlist))
        (let ((t1 (first-term L1))
              (t2 (first-term L2)))
          (if (> (order t2) (order t1))
              (list (the-empty-termlist) L1)
              (let ((new-c (div (coeff t1) (coeff t2)))
                    (new-o (- (order t1) (order t2))))
                (let ((rest-of-result
                       (div-terms (add-terms L1 (mul-terms (negative-terms (adjoin-term (make-term new-o new-c) (the-empty-termlist))) L2)) L2)))
                  (list (adjoin-term (make-term new-o new-c) (car rest-of-result))
                        (cadr rest-of-result))
                  ))))))
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial-sparse p))
  (put 'add '(polynomial-sparse polynomial-sparse)
       (lambda (p1 p2) (tag (add-poly p1 p2))))
  (put 'mul '(polynomial-sparse polynomial-sparse)
       (lambda (p1 p2) (tag (mul-poly p1 p2))))
  (put 'div '(polynomial-sparse polynomial-sparse)
       (lambda (p1 p2) (map tag (div-poly p1 p2))))
  (put '=zeros? '(polynomial-sparse) =zeros-p?)
  (put 'negative '(polynomial-sparse)
       (lambda (p1) (tag (negative-poly p1))))
  (put 'sub '(polynomial-sparse polynomial-sparse)
       (lambda (p1 p2) (tag (add-poly p1 (negative-poly p2)))))
  (put 'make 'polynomial-sparse
       (lambda (var terms) (tag (make-poly var terms))))
  'done)
(define (make-polynomial-sparse var terms)
  ((get 'make 'polynomial-sparse) var terms))

; install all polynomial
; I do't implement the mix operation of the two polynomial, I have an idea:use the sparse to represent the dense and this
; give a tower structure, and can use the raise operation to implement this.
(define (install-polynomial-package)
  ;; internal procedures
  ;; representation of poly
  
  ;; interface to rest of the system
  (define (tag p) (attach-tag 'polynomial p))
  (put 'add '(polynomial polynomial)
       (lambda (p1 p2) (tag (add p1 p2))))
  (put 'mul '(polynomial polynomial)
       (lambda (p1 p2) (tag (mul p1 p2))))
  (put '=zeros? '(polynomial) =zeros?)
  (put 'negative '(polynomial)
       (lambda (p1) (tag (negative p1))))
  (put 'sub '(polynomial polynomial)
       (lambda (p1 p2) (tag (add p1 (negative p2)))))
  (put 'make 'polynomial-all-dense
       (lambda (var terms) (tag (make-polynomial-dense var terms))))
  (put 'make 'polynomial-all-sparse
       (lambda (var terms) (tag (make-polynomial-sparse var terms))))
  'done)
(define (make-polynomial-all-dense var terms)
  ((get 'make 'polynomial-all-dense) var terms))
(define (make-polynomial-all-sparse var terms)
  ((get 'make 'polynomial-all-sparse) var terms))

(define (install-all)
  (install-scheme-number-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  (install-rational-package)
  (install-equ)
  (install-zeros)
  (install-project)
  (install-polynomial-package)
  (install-polynomial-sparse-package)
  (install-polynomial-dense-package)
  'install-all)

(install-all)



;(raise (make-rational 0 1))
;(raise (make-scheme-number 1))

(div (make-polynomial-sparse 'x '((5 1) (0 -1))) (make-polynomial-sparse 'x '((2 1) (0 -1))))
(sub (make-polynomial-sparse 'x '((1 1))) (make-polynomial-sparse 'x '((1 1))))
(sub (make-polynomial-sparse 'x '((5 3) (2 1) (1 1))) (make-polynomial-sparse 'x '((1 1))))
(sub (make-polynomial-sparse 'x `((5 ,(make-polynomial-sparse 'y '((1 1)))) (2 ,(make-polynomial-sparse 'y '((1 1)))) (1 ,(make-polynomial-sparse 'y '((1 1))))))
     (make-polynomial-sparse 'x `((3 ,(make-polynomial-sparse 'y '((1 1))))(1 ,(make-polynomial-sparse 'y '((1 1)))))))
