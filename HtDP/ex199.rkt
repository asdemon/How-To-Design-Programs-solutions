;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname ex199) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/batch-io)
(require 2htdp/itunes)

; modify the following to use your chosen name
(define ITUNES-LOCATION "itunes.xml")
 
; LTracks
(define itunes-tracks
  (read-itunes-as-tracks ITUNES-LOCATION))

(define cc
  (list (create-track "one"
                "two"
                "three"
                4
                5
                (create-date 1 2 3 4 5 45)
                7
                (create-date 1 2 3 4 5 50))
        (create-track "name"
                "artist"
                "album"
                4
                5
                (create-date 1 2 3 4 5 6)
                7
                (create-date 1 2 3 4 5 16))
))

; ex200
; LTracks -> N
; 计算所有播放时间
(define (total-time l)
  (cond
    ((empty? l) 0)
    (else (+ (track-time (first l)) (total-time (rest l))))))

; ex201
; LTracks -> List-of-strings.
; album组成一个string list
(define (select-all-album-titles l)
  (cond
    ((empty? l) '())
    (else (cons (track-album (first l)) (select-all-album-titles (rest l))))))

; List-of-strings -> List-of-strings.
; It consumes a List-of-strings and constructs one that contains every String from the given list exactly once
(define (create-set l)
  (cond
    ((empty? l) '())
    (else (if (in? (first l) (create-set (rest l)))
              (create-set (rest l))
              (cons (first l) (create-set (rest l)))))))

(define (in? word ll)
  (cond
    ((empty? ll) #f)
    (else (or (string=? word (first ll)) (in? word (rest ll))))))


(define (select-album-titles/unique l)
  (create-set (select-all-album-titles l)))


; ex202
; title LTracks -> LTracks
; The function consumes the title of an album and an LTracks
(check-expect (select-album "album" cc)
              (list (create-track "name"
                "artist"
                "album"
                4
                5
                (create-date 1 2 3 4 5 6)
                7
                (create-date 1 2 3 4 5 16))))
              
(define (select-album title l)
  (cond
    ((empty? l) '())
    (else (if (string=? title (track-album (first l)))
              (cons (first l) (select-album title (rest l)))
              (select-album title (rest l))))))

; ex203
; title date LTracks -> LTracks
; It extracts from the latter the list of tracks that belong to the given album and have been played after the given date.
(check-expect (select-album-date "album" (create-date 1 2 3 4 5 6) cc)
              (list (create-track "name"
                "artist"
                "album"
                4
                5
                (create-date 1 2 3 4 5 6)
                7
                (create-date 1 2 3 4 5 16))))
              
(define (select-album-date title date l)
  (select-after-date date (select-album title l)))

; date LTracks -> LTracks
; The function consumes a date and an LTracks, return list of tracks played after date
(define (select-after-date date l)
  (cond
    ((empty? l) '())
    (else (if (= (compare (track-played (first l)) date) 1)
              (cons (first l) (select-after-date date (rest l)))
              (select-after-date date (rest l))))))

; date date -> 1 0 -1
;比较date1和date2,如果date1》date2，返回1；相等，返回0；date1《date2,返回-1
(define (compare date1 date2)
  (cond
    ((> (date-year date1) (date-year date2)) 1)
    ((= (date-year date1) (date-year date2)) (cond
                                               ((> (date-month date1) (date-month date2)) 1)
                                               ((= (date-month date1) (date-month date2)) (cond
                                                                                            ((> (date-day date1) (date-day date2)) 1)
                                                                                            ((= (date-day date1) (date-day date2)) (cond
                                                                                                                                     ((> (date-hour date1) (date-hour date2)) 1)
                                                                                                                                     ((= (date-hour date1) (date-hour date2)) (cond
                                                                                                                                                                                ((> (date-minute date1) (date-minute date2)) 1)
                                                                                                                                                                                ((= (date-minute date1) (date-minute date2)) (cond
                                                                                                                                                                                                                               ((> (date-second date1) (date-second date2)) 1)
                                                                                                                                                                                                                               ((= (date-second date1) (date-second date2)) 0)
                                                                                                                                                                                                                               (else -1)))
                                                                                                                                                                                (else -1)))
                                                                                                                                     (else -1)))
                                                                                            (else -1)))
                                               (else -1)))
    (else -1)))


; ex204
;  LTracks -> list of  LTracks.
; The function consumes an element of LTracks. It produce a list of LTracks, one per album.
(check-expect (select-albums cc)
              (list (list (create-track "name"
                "artist"
                "album"
                4
                5
                (create-date 1 2 3 4 5 6)
                7
                (create-date 1 2 3 4 5 16)))
                    (list (create-track "one"
                                        "two"
                                        "three"
                                        4
                                        5
                                        (create-date 1 2 3 4 5 45)
                                        7
                                        (create-date 1 2 3 4 5 50)))))
(define (select-albums l)
  (cond
    ((empty? l) '())
    (else (insert-into-list (first l) (select-albums (rest l))))))
; Track, list of  LTracks -> list of  LTracks.
; 将Track插入到list of  LTracks中，如果已经存在相同album，插入到相应的list中；如不存在，则新加一个list
(define (insert-into-list item l)
  (cond
    ((empty? l) (list (list item)))
    (else (if (string=? (track-album (first (first l))) (track-album item))
              (cons (cons item (first l)) (rest l))
              (cons (first l) (insert-into-list item (rest l)))))))
    