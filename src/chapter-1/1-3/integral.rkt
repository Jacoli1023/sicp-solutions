#lang racket
(require "accumulate.rkt")
(provide (all-defined-out))

(define (inc x) (+ x 1))

(define (integral f a b dx)
  (define (add-dx x) (+ x dx))
  (* (sum f (+ a (/ dx 2.0)) add-dx b) 
     dx))

(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (* (f (+ a (* k h)))
         (cond ((or (= k 0) (= k n)) 1)
               ((odd? k) 4)
               (else 2))))
    (* (/ h 3) (sum term 0.0 inc n))))
