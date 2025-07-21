#lang racket
(require "vector.rkt")
(provide (all-defined-out))

; frame representation
(define (make-frame origin edge1 edge2)
  (list origin edge1 edge2))
(define (origin-frame frame)
  (car frame))
(define (edge1-frame frame)
  (cadr frame))
(define (edge2-frame frame)
  (caddr frame))

; procedures which use frames
(define (frame-coord-map frame)
  (lambda (v)
    (add-vect (origin-frame frame)
              (add-vect (scale-vect (xcor-vect v) (edge1-frame frame))
                        (scale-vect (ycor-vect v) (edge2-frame frame))))))
