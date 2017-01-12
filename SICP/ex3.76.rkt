#lang sicp



(define (smooth s)
  (scale-stream (add-streams s (stream-cdr s)) 0.5))



(define (zero-crossings input-stream)
  (stream-map sign-change-detector
              input-stream
              (cons-stream (stream-car input-stream) input-stream)))

(define (make-zeor-crosssings input-stream smooth)
  (zero-crossings (smooth input-stream)))