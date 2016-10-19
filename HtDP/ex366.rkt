;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex366) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require 2htdp/image)
(require 2htdp/batch-io)
(require 2htdp/universe)

; An Xexpr.v2 is a list: 
; – (cons Symbol XL)
; An XL is one of:
; – [List-of Xexpr.v2]
; – (cons [List-of Attribute] [List-of Xexpr.v2])
; 
; An Attribute is a list of two items:
;   (cons Symbol (cons String '()))

(define a0 '((initial "X")))

(define e0 '(machine))
(define e1 `(machine ,a0))
(define e2 '(machine (action)))
(define e3 '(machine () (action)))
(define e4 `(machine ,a0 (action) (action)))


; Xexpr.v2 -> [List-of Attribute]
; retrieves the list of attributes of xe
(check-expect (xexpr-attr e0) '())
(check-expect (xexpr-attr e1) '((initial "X")))
(check-expect (xexpr-attr e2) '())
(check-expect (xexpr-attr e3) '())
(check-expect (xexpr-attr e4) '((initial "X")))

(define (xexpr-attr xe)
  (match xe
    ((cons s xl) (match xl
                   ('() '())
                   ((cons (cons (? symbol?) sub-xl) xl-xl) '())
                   ((cons h t) h)))))


; ex366
; Xexpr.v2 -> symbol
; retrieves the name of xe
(check-expect (xexpr-name e0) 'machine)
(check-expect (xexpr-name e1) 'machine)
(check-expect (xexpr-name e2) 'machine)
(check-expect (xexpr-name e3) 'machine)
(check-expect (xexpr-name e4) 'machine)

(define (xexpr-name xe)
  (match xe
    ((cons s xl) s)))
; Xexpr.v2 -> [List-of Xexpr.v2]
; it extracts the list of content elements.
(check-expect (xexpr-content e0) '())
(check-expect (xexpr-content e1) '())
(check-expect (xexpr-content e2) '((action)))
(check-expect (xexpr-content e3) '((action)))
(check-expect (xexpr-content e4) '((action) (action)))
(define (xexpr-content xe)
  (match xe
    ((cons s xl) (match xl
                   ('() '())
                   ((cons (cons (? symbol?) sub-xl) xl-xl) xl)
                   ((cons h t) t)))))


; ex369
; a list of attributes ,a symbol -> string
; If the attributes list associates the symbol with a string, the function retrieves this string; otherwise it returns #false.
(check-expect (find-attr 'initial (xexpr-attr e1) ) "X")
(check-expect (find-attr 'init3ial (xexpr-attr e1)) #f)
(define (find-attr sy l)
  (local (
          (define re (assq sy l))
          )
    ;------------------------------
    (if (boolean? re)
        re
        (second re))))


; An XWord is '(word ((text String))).

; ex370

; any -> boolean
; test whether any is a XWord
(check-expect (word? '(word ((text "a")))) #t)
(check-expect (word? '(word ((aa "a")))) #f)
(define (word? a)
  (match a
    ((cons 'word (cons (cons (cons 'text (cons (? string?) '())) '()) '())) #t)
    (s #f)))

; XWord -> text
; it extracts the value of the only attribute of an instance of XWord
(check-expect (word-text '(word ((text "a")))) "a")
(define (word-text xw)
  (find-attr 'text (xexpr-attr xw)))

(define BULLET (circle 4 "solid" "blue"))
(define e10
  '(ul
    (li (word ((text "one"))))
    (li (word ((text "two"))))))
(define e10-rendered
  (above/align 'left
               (beside/align 'center BULLET
                             (text "one" 12 'black))
               (beside/align 'center BULLET
                             (text "two" 12 'black))))

; An XEnum.v1 is one of: 
; – (cons 'ul [List-of XItem.v1])
; – (cons 'ul (cons Attributes [List-of XItem.v1]))
; An XItem.v1 is one of:
; – (cons 'li (cons XWord '()))
; – (cons 'li (cons Attributes (cons XWord '())))

(define (render-item1 i image)
  (local ((define content (xexpr-content i))
          (define element (first content))
          (define a-word (word-text element))
          (define item (text a-word 12 'black)))
    (beside/align 'center image item)))



; XEnum.v1 -> Image 
; renders a simple enumeration as an image 
(check-expect (render-enum1 e10) e10-rendered)
(define (render-enum1 xe)
  (local (
          (define content (xexpr-content xe))
          (define (render-list-of-item x)
            (match x
              ('() (empty-scene 0 0))
              ((cons h t) (above/align 'left (render-item1 h BULLET) (render-list-of-item t)))))
            )
          ;----------
          (render-list-of-item content)))



; An Xexpr.v3 is one of:
;  – Symbol
;  – String
;  – Number
;  – (cons Symbol (cons Attribute*.v3 [List-of Xexpr.v3]))
;  – (cons Symbol [List-of Xexpr.v3])
; 
; An Attribute*.v3 is a [List-of Attribute.v3].
;   
; An Attribute.v3 is a list of two items:
;   (list Symbol String)

(define PREFIX "https://www.google.com/finance?q=")
(define SUFFIX "&btnG=Search")
(define SIZE 22) ; font size 
 
(define-struct data [price delta])
; A StockWorld is a structure: (make-data String String)
 
; String -> StockWorld
; retrieves the stock price of co and its change every 15s
(define (stock-alert co)
  (local ((define url (string-append PREFIX co SUFFIX))
          ; [StockWorld -> StockWorld]
          ; retrieve stock information from web
          (define (retrieve-stock-data __w)
            (local ((define x (read-xexpr/web url)))
              (make-data (get x "price")
                         (get x "priceChange"))))
          ; StockWorld -> Image 
          (define (render-stock-data w)
            (local (; [StockWorld -> String] -> Image
                    (define (word sel col)
                      (text (sel w) SIZE col)))
              (overlay (beside (word data-price 'black)
                               (text "  " SIZE 'white)
                               (word data-delta 'red))
                       (rectangle 300 35 'solid 'white)))))
    (big-bang (retrieve-stock-data 'no-use)
      [on-tick retrieve-stock-data 15]
      [to-draw render-stock-data])))


; ex386
; Xexpr.v3 String -> String
; retrieves the value of the "content" attribute 
; from a 'meta element that has attribute "itemprop"
; with value s
(check-expect
  (get '(meta ((content "+1") (itemprop "F"))) "F")
  "+1")
(check-error
  (get '(meta ((content "+1") (itemprop "F"))) "788")
  "not found")
 
(define (get x s)
  (local ((define result (get-xexpr x s)))
    (if (string? result)
        result
        (error "not found"))))

(define (get-xexpr x s)
  (local (
          (define find-itemprop-res (find-attr 'itemprop (xexpr-attr x)))
          (define find-content-res (find-attr 'content (xexpr-attr x)))
          )
    ;--------------------
    (if (boolean? find-itemprop-res)
        #f
        (if (string=? s find-itemprop-res)
            find-content-res
            #f))))