#lang racket
(require "linesegments.rkt")
(provide (all-defined-out))

(define (area rect)
  (* (length-rect rect) (width-rect rect)))
(define (perimeter rect)
  (+ (* 2 (length-rect rect))
     (* 2 (width-rect rect))))

(define (make-rectangle corner)
  (cons (make-point 0 0) corner))
(define origin car)
(define corner cdr)
(define (length-rect rect)
  (x-point (corner rect)))
(define (width-rect rect)
  (y-point (corner rect)))
