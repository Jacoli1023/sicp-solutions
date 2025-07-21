#lang racket
(require sicp-pict)

(define outline
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 0))
    (make-segment (make-vect 1 0) (make-vect 1 1))
    (make-segment (make-vect 1 1) (make-vect 0 1))
    (make-segment (make-vect 0 1) (make-vect 0 0)))))

(define x-painter
  (segments->painter
   (list
    (make-segment (make-vect 0 0) (make-vect 1 1))
    (make-segment (make-vect 0 1) (make-vect 1 0)))))

(define diamond
  (segments->painter
   (list
    (make-segment (make-vect 0.5 0) (make-vect 1 0.5))
    (make-segment (make-vect 1 0.5) (make-vect 0.5 1))
    (make-segment (make-vect 0.5 1) (make-vect 0 0.5))
    (make-segment (make-vect 0 0.5) (make-vect 0.5 0)))))

(define wave
  (segments->painter
   (list
    (make-segment (make-vect 0.4 0) (make-vect 0.5 0.3))
    (make-segment (make-vect 0.5 0.3) (make-vect 0.6 0))
    (make-segment (make-vect 0.65 0) (make-vect 0.55 0.55))
    (make-segment (make-vect 0.55 0.55) (make-vect 1 0.3))
    (make-segment (make-vect 1 0.4) (make-vect 0.55 0.65))
    (make-segment (make-vect 0.55 0.65) (make-vect 0.65 0.75))
    (make-segment (make-vect 0.65 0.75) (make-vect 0.55 1))
    (make-segment (make-vect 0.45 1) (make-vect 0.35 0.75))
    (make-segment (make-vect 0.35 0.75) (make-vect 0.45 0.65))
    (make-segment (make-vect 0.45 0.65) (make-vect 0.25 0.55))
    (make-segment (make-vect 0.25 0.55) (make-vect 0 0.75))
    (make-segment (make-vect 0 0.65) (make-vect 0.25 0.5))
    (make-segment (make-vect 0.25 0.5) (make-vect 0.45 0.55))
    (make-segment (make-vect 0.45 0.55) (make-vect 0.35 0)))))
