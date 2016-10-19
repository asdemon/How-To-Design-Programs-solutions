;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-intermediate-lambda-reader.ss" "lang")((modname ex322) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/abstraction)
(define-struct no-info [])
(define NONE (make-no-info))

(define-struct node [ssn name left right])
; A BinaryTree (short for BT) is one of:
; – NONE
; – (make-node Number Symbol BT BT)


; ex322
; N, BT -> boolean
; determines whether a given number occurs in some given BT
(check-expect (contains-bt? 15 (make-node
                                15
                                'd
                                NONE
                                (make-node
                                 24 'i NONE NONE)))
              #t)
(check-expect (contains-bt? 24 (make-node
                                15
                                'd
                                NONE
                                (make-node
                                 24 'i NONE NONE)))
              #t)
(check-expect (contains-bt? 11 (make-node
                                15
                                'd
                                NONE
                                (make-node
                                 24 'i NONE NONE)))
              #f)
(define (contains-bt? n bt)
  (match bt
    ((no-info) #f)
    ((node ssn name left right) (or (= n ssn)
                                    (contains-bt? n left)
                                    (contains-bt? n right)))))

; ex323
; N, BT -> #f or name
; If the tree contains a node structure whose ssn field is n,
; the function produces the value of the name field in that node. Otherwise,
; the function produces #false.
(check-expect (search-bt 15 (make-node
                             15
                             'd
                             NONE
                             (make-node
                              24 'i NONE NONE)))
              'd)
(check-expect (search-bt 24 (make-node
                             15
                             'd
                             NONE
                             (make-node
                              24 'i NONE NONE)))
              'i)
(check-expect (search-bt 11 (make-node
                             15
                             'd
                             NONE
                             (make-node
                              24 'i NONE NONE)))
              #f)
(define (search-bt n bt)
  (match bt
    ((no-info) #f)
    ((node ssn name left right) (if (= n ssn)
                                    name
                                    (local (
                                            ;右子树结果
                                            (define right-re (search-bt n right))
                                            ;左子树结果
                                            (define left-re (search-bt n left)))
                                      ;-------IN--------------
                                      (if (boolean? right-re)
                                          left-re
                                          right-re))))))


; ex324
; BT -> list of N
; 中序遍历
(define (inorder bt)
  (match bt
    ((no-info) '())
    ((node ssn name left right) (append (inorder left) `(,ssn) (inorder right)))))


; ex325
; BT -> name or #f
;; If the tree contains a node structure whose ssn field is n,
; the function produces the value of the name field in that node. Otherwise,
; the function produces #false.
(check-expect (search-bst 15 (make-node
                              15
                              'd
                              NONE
                              (make-node
                               24 'i NONE NONE)))
              'd)
(check-expect (search-bst 24 (make-node
                              15
                              'd
                              NONE
                              (make-node
                               24 'i NONE NONE)))
              'i)
(check-expect (search-bst 11 (make-node
                              15
                              'd
                              NONE
                              (make-node
                               24 'i NONE NONE)))
              #f)
(define (search-bst n bst)
  (match bst
    ((no-info) #f)
    ((node ssn name left right) (cond
                                  ((< ssn n) (search-bst n right))
                                  ((> ssn n) (search-bst n left))
                                  (else name)))))


; ex326
; bst N symbol -> bst
; inset an entry into a bst
(check-expect (create-bst (make-node
                           15
                           'd
                           NONE
                           (make-node
                            24 'i NONE NONE)) 11 'add1)
              (make-node
               15
               'd
               (make-node
                11 'add1 NONE NONE)
               (make-node
                24 'i NONE NONE)))
(check-expect (create-bst (make-node
                           15
                           'd
                           (make-node
                            11 'add1 NONE NONE)
                           (make-node
                            24 'i NONE NONE)) 10 'add2)
              (make-node
               15
               'd
               (make-node
                11 'add1 (make-node
                          10 'add2 NONE NONE) NONE)
               (make-node
                24 'i NONE NONE)))
(check-expect (create-bst (make-node
                           15
                           'd
                           (make-node
                            11 'add1 (make-node
                                      10 'add2 NONE NONE) NONE)
                           (make-node
                            24 'i NONE NONE)) 12 'add3)
              (make-node
               15
               'd
               (make-node
                11 'add1 (make-node
                          10 'add2 NONE NONE) (make-node
                                               12 'add3 NONE NONE))
               (make-node
                24 'i NONE NONE)))
;(define (create-bst bst n name)
;  (cond
;    ((and (< n (node-ssn bst))
;          (no-info? (node-left bst))) (make-node (node-ssn bst)
;                                                 (node-name bst)
;                                                 (make-node n
;                                                            name
;                                                            NONE
;                                                            NONE)
;                                                 (node-right bst)))
;    ((and (>= n (node-ssn bst))
;          (no-info? (node-right bst))) (make-node (node-ssn bst)
;                                                  (node-name bst)
;                                                  (node-left bst)
;                                                  (make-node n
;                                                             name
;                                                             NONE
;                                                             NONE)))
;    (else (if (< n (node-ssn bst))
;              (make-node (node-ssn bst)
;                         (node-name bst)
;                         (create-bst (node-left bst) n name)
;                         (node-right bst))
;              (make-node (node-ssn bst)
;                         (node-name bst)
;                         (node-left bst)
;                         (create-bst (node-right bst) n name))))))
(define (create-bst bst n name)
  (cond
    ((no-info? bst) (make-node n
                               name
                               NONE
                               NONE))
    (else (if (< n (node-ssn bst))
              (make-node (node-ssn bst)
                         (node-name bst)
                         (create-bst (node-left bst) n name)
                         (node-right bst))
              (make-node (node-ssn bst)
                         (node-name bst)
                         (node-left bst)
                         (create-bst (node-right bst) n name))))))



; ex327
; [List-of [List Number Symbol]] -> BST
; It consumes a list of numbers and names and produces a binary search tree by repeatedly applying create-bst.
(define (create-bst-from-list l)
  (match l
    ('() NONE)
    ((cons head tail) (create-bst (create-bst-from-list tail) (first head) (second head)))))
(define (create-bst-from-list-abstract l)
  (foldr (lambda (ele bst) (create-bst bst (first ele) (second ele) )) NONE l))