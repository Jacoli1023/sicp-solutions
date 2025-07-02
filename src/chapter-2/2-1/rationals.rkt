#lang racket
(provide make-rat)

(define (make-rat n d)
  (define (sign x)
    (cond ((positive? x) 1)
          ((zero? x) 0)
          ((negative? x) -1)))
  (let ((g (gcd n d))
        (s (* (sign n) (sign d))))
    (cons (* s (/ (abs n) g))
          (/ (abs d) g))))
