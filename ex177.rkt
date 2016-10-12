;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex177) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)
(require 2htdp/batch-io)


(define HEIGHT 20) ; the height of the editor 
(define WIDTH 200) ; its width 
(define FONT-SIZE 16) ; the font size 
(define FONT-COLOR "black") ; the font color 
 
(define MT (empty-scene WIDTH HEIGHT))
(define CURSOR (rectangle 1 HEIGHT "solid" "red"))


(define-struct editor [pre post])
; An Editor is a structure:
;   (make-editor Lo1S Lo1S) 
; An Lo1S is one of: 
; – '()
; – (cons 1String Lo1S)


; string string -> editor
; The function consumes two strings and produces an Editor.
(check-expect (create-editor "123" "456")
              (make-editor (list "3" "2" "1") (list "4" "5" "6")))
(check-expect (create-editor "" "456")
              (make-editor '() (list "4" "5" "6")))
(check-expect (create-editor "123" "")
              (make-editor (list "3" "2" "1") '()))
(define (create-editor s1 s2)
  (make-editor (reverse (explode s1)) (explode s2)))


; Editor -> Image
; renders an editor as an image of the two texts 
; separated by the cursor 
(define (editor-render ed)
  (place-image/align
  (beside (text (implode (reverse (editor-pre ed))) FONT-SIZE FONT-COLOR)
          CURSOR
          (text (implode (editor-post ed)) FONT-SIZE FONT-COLOR))
  1 1
  "left" "top"
  MT))
 
; Editor KeyEvent -> Editor
; deals with a key event, given some editor
(check-expect (editor-kh (create-editor "" "") "e")
              (create-editor "e" ""))
(check-expect
  (editor-kh (create-editor "cd" "fgh") "e")
  (create-editor "cde" "fgh"))
(check-expect (editor-kh (create-editor "a" "bc") "\b")
              (create-editor "" "bc"))
(check-expect (editor-kh (create-editor "a" "bc") "a")
              (create-editor "aa" "bc"))
(check-expect (editor-kh (create-editor "" "bc") "\b")
              (create-editor "" "bc"))
(check-expect (editor-kh (create-editor "" "bc") "left")
              (create-editor "" "bc"))
(check-expect (editor-kh (create-editor "" "bc") "right")
              (create-editor "b" "c"))
(check-expect (editor-kh (create-editor "bc" "") "right")
              (create-editor "bc" ""))
(check-expect (editor-kh (create-editor "ab" "c") "right")
              (create-editor "abc" ""))
(define (editor-kh ed k)
  (cond
    [(key=? k "left") (editor-lft ed)]
    [(key=? k "right") (editor-rgt ed)]
    [(key=? k "\b") (editor-del ed)]
    [(key=? k "\t") ed]
    [(key=? k "\r") ed]
    [(= (string-length k) 1) (editor-ins ed k)]
    [else ed]))

(define (editor-ins ed k)
  (make-editor (cons k (editor-pre ed))
               (editor-post ed)))

; Editor -> Editor
; moves the cursor position one 1String left, 
; if possible 
(define (editor-lft ed)
  (if (= (length (editor-pre ed)) 0)
      ed
      (make-editor (rest (editor-pre ed))
                   (cons (first (editor-pre ed)) (editor-post ed)))))
 
; Editor -> Editor
; moves the cursor position one 1String right, 
; if possible 
(define (editor-rgt ed)
  (if (= (length (editor-post ed)) 0)
      ed
      (make-editor (cons (first (editor-post ed)) (editor-pre ed))
                   (rest (editor-post ed)))))
 
; Editor -> Editor
; deletes a 1String to the left of the cursor
; if possible 
(define (editor-del ed)
  (if (= (length (editor-pre ed)) 0)
      ed
      (make-editor (rest (editor-pre ed))
                   (editor-post ed))))



; main : String -> Editor
; launches the editor given some initial string 
(define (main s)
   (big-bang (create-editor s "")
     [on-key editor-kh]
     [to-draw editor-render]))