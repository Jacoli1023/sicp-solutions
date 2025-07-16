#lang racket
(require "sequence-operations.rkt")
(provide (all-defined-out))

(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ (* higher-terms x) this-coeff))
   0
   coefficient-sequence))
