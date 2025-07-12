#lang racket
(provide (all-defined-out))

(define (count-leaves x)
  (cond ((null? x) 0)
        ((not (pair? x)) 1)
        (else (+ (count-leaves (car x))
                 (count-leaves (cdr x))))))

(define (deep-reverse ls)
  (define (iter l1 l2)
    (cond ((null? l1) l2)
          ((not (pair? l1)) l1)
          (else (iter (cdr l1) (cons (deep-reverse (car l1)) l2)))))
  (iter ls '()))
