#lang racket
(provide (all-defined-out))

(define (inc x) (+ x 1))

(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))

(define (factorial n)
  (product identity 1 inc n))

(define (approx-pi n)
  (define (term k)
    (let ((r (remainder k 2)))
      (/ (- (+ k 2) r)
         (+ k r 1))))
  (* 4.0 (product term 1.0 inc n)))
