#lang racket
(provide (all-defined-out))

(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))

(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))
(define (center x)
  (/ (+ (lower-bound x) (upper-bound x)) 2))
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
