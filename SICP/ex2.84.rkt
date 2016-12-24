#lang racket

(define (list-index-self element list)
  (cond
    ((null? list) #f)
    ((eqv? element (car list)) 0)
    (else (+ 1
             (list-index-self  element (cdr list))))))

; use a list to represent the tower, the index is the level and low level can coerce to high level
(define level-list '(rational scheme-number complex))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
          (apply proc (map contents args))
          (let* ((indices (map (lambda (x) (list-index-self x level-list)) type-tags))
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
  (define (equ? x y) (apply-generic 'equ? x y))
  (define (=zeros? x) (apply-generic '=zeros? x))
  (define (real-part z) (apply-generic 'real-part z))
  (define (imag-part z) (apply-generic 'imag-part z))
  (define (magnitude z) (apply-generic 'magnitude z))
  (define (angle z) (apply-generic 'angle z))
  ;only have rational -> real -> complex
  (define (raise x) (apply-generic 'raise x))
  
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
  
  (define (install-all)
    (install-scheme-number-package)
    (install-rectangular-package)
    (install-polar-package)
    (install-complex-package)
    (install-rational-package)
    (install-equ)
    (install-zeros)
    'install-all)
  
  (install-all)
  
  
  
  ;(raise (make-rational 0 1))
 ;(raise (make-scheme-number 1))
(add (make-scheme-number 1) (make-rational 0 1))
(add (make-scheme-number 1) (make-complex-from-real-imag 0 1))
(add (make-complex-from-real-imag 0 -1) (make-complex-from-real-imag 1 1))