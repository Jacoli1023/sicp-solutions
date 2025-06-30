#lang racket
(require "../1-3/iterativeimprove.rkt")
(provide (all-defined-out))

(define (average x y)
  (/ (+ x y) 2))

(define (square x) (* x x))

(define (sqrt x)
  (define (improve guess)
    (average guess (/ x guess)))
  (define (good-enough? guess)
    (< (abs (- (square guess) x)) 0.001))
  ((iterative-improve good-enough? improve) 1.0))
