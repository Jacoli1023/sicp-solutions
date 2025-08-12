#lang racket
(require "hash-table.rkt")
(require (only-in "../2-2/sequence-operations.rkt" accumulate))
(provide (all-defined-out))

; --- Algebraic representation ---
;; redefining these here, since the alg-rep module from the
;; previous section is designed to handle standard algebraic
;; representation
(define (=number? expr num)
  (and (number? expr) (= expr num)))

(define (make-sum a1 a2)
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2)) (+ a1 a2))
        (else (list '+ a1 a2))))

(define (make-product m1 m2)
  (cond ((or (=number? m1 0) (=number? m2 0)) 0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2)) (* m1 m2))
        (else (list '* m1 m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        (else (list '** b e))))

; --- Data-directed derivation ---
(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))

(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 1 0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))

; --- Operator-specific packages
(define (sum-pkg)
  (define addend car)
  (define (augend sum)
    (accumulate make-sum 0 (cdr sum)))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (put 'deriv '+ deriv-sum)
  'done)

(define (product-pkg)
  (define multiplier car)
  (define (multiplicand product)
    (accumulate make-product 1 (cdr product)))
  (define (deriv-product operands var)
    (make-sum (make-product (multiplier operands)
                            (deriv (multiplicand operands) var))
              (make-product (deriv (multiplier operands) var)
                            (multiplicand operands))))
  (put 'deriv '* deriv-product)
  'done)

(define (expt-pkg)
  (define base car)
  (define expt cadr)
  (define (deriv-expt operands var)
    (make-product
     (expt operands)
     (make-product (make-exponentiation (base operands)
                                        (- (expt operands) 1))
                   (deriv (base operands) var))))
  (put 'deriv '** deriv-expt)
  'done)
