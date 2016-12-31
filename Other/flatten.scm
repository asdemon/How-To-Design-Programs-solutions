(define (flatten seq)
  (if (not (list? seq)) (list seq)
    (apply append (map flatten seq))))
