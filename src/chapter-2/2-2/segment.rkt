#lang racket
(require vector.rkt)
(provide (all-defined-out))

(define (make-segment v1 v2)
  (cons v1 v2))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
