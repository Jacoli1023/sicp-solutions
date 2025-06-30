#lang racket
(require "iterativeimprove.rkt")
(provide (all-defined-out))

(define tolerance 0.00001)
(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (fixed-point f first-guess)
  ((iterative-improve
    (lambda (guess)
      (< (abs (- guess (f guess))) tolerance))
    f) 
   first-guess))
