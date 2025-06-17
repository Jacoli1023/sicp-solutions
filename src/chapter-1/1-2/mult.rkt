#lang racket
(provide (all-defined-out))

(define (double x) (+ x x))
(define (halve x) (/ x 2))

(define (mult a b)
  (cond ((or (zero? a) (zero? b)) 0)
        ((even? b) (double (mult a (halve b))))
        (else (+ a (mult a (- b 1))))))

(define (mult-iter a b)
  (define (iter x a b)
    (cond ((zero? b) x)
          ((even? b) (iter x (double a) (halve b)))
          (else (iter (+ x a) a (- b 1)))))
  (iter 0 a b))
