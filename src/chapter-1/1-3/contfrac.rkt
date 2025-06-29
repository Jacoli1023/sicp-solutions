#lang racket
(provide (all-defined-out))

(define (cont-frac n d k)
  (define (iter i result)
    (if (zero? i)
      result
      (iter (- i 1) (/ (n i)
                       (+ (d i) result)))))
  (iter k 0))
