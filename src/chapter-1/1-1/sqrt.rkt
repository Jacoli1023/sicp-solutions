#lang racket
(provide (all-defined-out))

(define (improve guess x)
  (average guess (/ x guess)))

(define (average x y)
  (/ (+ x y) 2))

(define (good-enough? guess prev-guess)
   (< (/ (abs (- guess prev-guess)) guess) 0.001))

(define (sqrt-iter guess x)
  (let ((better-guess (improve guess x)))
    (if (good-enough? better-guess guess)
      better-guess
      (sqrt-iter better-guess x))))

(define (sqrt x)
  (sqrt-iter 1.0 x))
