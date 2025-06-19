#lang sicp

(#%require (only rnrs/control-6 when))
(#%require "primetime.rkt")
(#%provide (all-defined))

(define (search-for-primes a b)
  (define (iter a b)
    (when (<= a b)
      (timed-prime-test a)
      (iter (+ a 2) b)))
  (iter (if (odd? a) a (+ a 1)) b))
