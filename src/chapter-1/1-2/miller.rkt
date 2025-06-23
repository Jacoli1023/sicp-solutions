#lang racket
(provide (all-defined-out))

(define (square x) (* x x))

(define (square-check? a n)
  (if (and (not (or (= a 1) (= a (- n 1))))
           (= (square a) (remainder 1 n)))
           0
           (remainder (square a) n)))

(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (square-check? (expmod base (/ exp 2) m) m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (miller-test n)
  (define (try-it a)
    (= (expmod a (- n 1) n) 1))
  (try-it (+ 2 (random (- n 2)))))

(define (fast-prime? n times)
  (or (= times 0)
      (and (miller-test n)
           (fast-prime? n (- times 1)))))
