#lang racket
(require "sequence-operations.rkt")
(provide (all-defined-out))

(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sums n s)
  (filter (lambda (triple)
            (= (+ (car triple) (cadr triple) (caddr triple)) s))
          (unique-triples n)))
