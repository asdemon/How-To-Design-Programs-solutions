;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-reader.ss" "lang")((modname editor) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

(define EDITOR-WIDTH 200)
(define EDITOR-HEIGHT 20)
(define CURSOR-WIDTH 1)
(define CURSOR-HEIGHT 20)
(define BG (empty-scene EDITOR-WIDTH EDITOR-HEIGHT))
(define CURSOR (rectangle CURSOR-WIDTH CURSOR-HEIGHT "solid" "red"))

(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor String String)
; interpretation (make-editor s t) describes an editor
; whose visible text is (string-append s t) with 
; the cursor displayed between s and t

;editor ->image
(define (render e)
  (overlay/align "left" "center"
                (beside (text (editor-pre e) 16 "black") CURSOR (text (editor-post e) 16 "black"))
               BG))

(define (str-cat e)
         (string-append (editor-pre e) (editor-post e)))

(define (string-first s)
  (if (string=? s "") "" (string-ith s 0)))
(define (string-last s)
  (if (string=? s "") "" (string-ith s (- (string-length s) 1))))
(define (delete-from-rear-char s)
  (if (<= (string-length s) 1) "" (substring s 0 (- (string-length s) 1) )))
(define (delete-from-front-char s)
  (if (<= (string-length s) 1) "" (substring s 1 (string-length s))))
;editor KeyEvent -> editor
;模拟键盘输入操作
(check-expect (edit (make-editor "a" "bc") "a")
              (make-editor "aa" "bc"))
(check-expect (edit (make-editor "a" "bc") "\b")
              (make-editor "" "bc"))
(check-expect (edit (make-editor "a" "bc") "a")
              (make-editor "aa" "bc"))
(check-expect (edit (make-editor "" "bc") "\b")
              (make-editor "" "bc"))
(check-expect (edit (make-editor "" "bc") "left")
              (make-editor "" "bc"))
(check-expect (edit (make-editor "" "bc") "right")
              (make-editor "b" "c"))
(check-expect (edit (make-editor "bc" "") "right")
              (make-editor "bc" ""))
(check-expect (edit (make-editor "ab" "c") "right")
              (make-editor "abc" ""))
(define (edit ed ke)
  (cond
    ((= (string-length ke) 1) (if (equal? ke "\b")
                                  (if (= (string-length (editor-pre ed)) 0)
                                      ed
                                      (make-editor (delete-from-rear-char (editor-pre ed))
                                                   (editor-post ed)))
                                  (make-editor (string-append (editor-pre ed) ke)
                                                  (editor-post ed))))
    ((string=? ke "left") (if (= (string-length (editor-pre ed)) 0)
                                  ed
                                  (make-editor (delete-from-rear-char (editor-pre ed))
                                                  (string-append (string-last (editor-pre ed)) (editor-post ed)))))
    ((string=? ke "right") (if (= (string-length (editor-post ed)) 0)
                                  ed
                                  (make-editor (string-append (editor-pre ed) (string-first (editor-post ed)))
                                                  (delete-from-front-char (editor-post ed)))))
    (else ed)
    ))


;run
(define (run s)
  (big-bang (make-editor s "")
            (on-key edit)
            (on-draw render)))

