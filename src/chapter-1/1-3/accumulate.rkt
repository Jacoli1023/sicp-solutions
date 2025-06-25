#lang racket
(provide (all-defined-out))

(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))

(define (filtered-accumulate combiner pred? null-value term a next b)
  (define (iter a result)
    (cond ((> a b)
           result)
          ((pred? a)
           (iter (next a) (combiner result (term a))))
          (else
           (iter (next a) result))))
  (iter a null-value))

(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))
