;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex338) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(require htdp/dir)

;(define O (create-dir "/Users/...")) ; on OS X 
;(define L (create-dir "/var/log/")) ; on Linux
(define W (create-dir "E:\\111\\")) ; on Windows 




;(define-struct file [name size content])
; A File.v3 is a structure: 
;   (make-file String N String)
;(define-struct dir.v3 [name dirs files])
; A Dir.v3 is a structure: 
;   (make-dir.v3 String Dir* File*)
 
; A Dir* is one of: 
; – '()
; – (cons Dir.v3 Dir*)
 
; A File* is one of: 
; – '()
; – (cons File.v3 File*)

(define (how-many.v3 d3)
  (+ (foldr + 0 (map how-many.v3 (dir-dirs d3))) (length (dir-files d3))))

; ex339
; Dir, name -> boolean
; it consumes a Dir and a file name and determines whether or not a file with this name occurs in the directory tree
(check-expect (find? "1.JPG" W) #t)
(check-expect (find? "1+(10).JPG" W) #t)
(check-expect (find? "1+(1xxx0).JPG" W) #f)
(define (find? name dir1)
  (or (ormap (lambda (f1) (string=? name (file-name f1))) (dir-files dir1))
      (ormap (lambda (dir3) (find? name dir3)) (dir-dirs dir1))))


; ex340
; Dir -> list of name
; lists the names of all files and directories in a given Dir
(define (ls d3)
  (append `(,(dir-name d3))
          (map file-name (dir-files d3))
          (map ls (dir-dirs d3))))

; A Path is [List-of String].
; interpretation directions into in a directory tree

; ex342
; Dir , filename -> path or #f
(define (find name d3)
  (if (ormap (lambda (f1) (string=? name (file-name f1))) (dir-files d3))
      (dir-name d3)
      (local
        (; list of Dir -> path or #f
         ;find the filname
         (define (f l)
           (match l
             ('() #f)
             ((cons h t) (local ((define inter-res (find name h)))
                           ;----IN-------
                           (if (boolean? inter-res)
                             (f t)
                             inter-res))))))
        ;---IN--------
        (f (dir-dirs d3)))))

; ex344
; dir
; (cons dirname [list of filename])
; (cons dir [list of filename])

; Dir , filename -> path or '()
; find all file with name in d3
(define (find-all name d3)
  (local
    (; l中的所有文件
     (define (all-file-in-list l)
       (filter (lambda (e) (not (list? e))) l))
     ;l中的所有文件夹
     (define (all-dir-in-list l)
       (filter (lambda (e)  (list? e)) l))
     ;只搜索一个文件夹，不搜索文件夹中的文件夹
     (define (ff l)
       (if (ormap (lambda (f1) (if (string? f1)
                                   (string=? name  f1)
                                   #f)) (all-file-in-list l))
           (list (first l))
           '()))
    (define ll-ls (ls d3))
    ;find-all的第三个参数为list，其余和find-all函数作用一样
    (define (fff name l)
      (append (ff l) (apply append (map (lambda (l1) (fff name l1)) (all-dir-in-list l))))))
    ;----
    (fff name ll-ls)))

(define (all-dir-in-list l)
       (filter (lambda (e)  (list? e)) l))