#lang racket
(require (except-in "sequence-operations.rkt" map))
(provide (all-defined-out))

(define (dot-product v w)
  (accumulate + 0 (map * v w)))

(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-vec)
           (matrix-*-vector cols m-vec)) 
         m)))
