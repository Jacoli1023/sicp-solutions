#lang racket
(require (only-in "../2-2/sequence-operations.rkt" accumulate))
(provide (all-defined-out))

(define (variable? x) (symbol? x))
(define (same-variable? v1 v2)
  (and (variable? v1) (variable? v2) (eq? v1 v2)))
(define (=number? exp num)
  (and (number? exp) (= exp num)))

(define (has? op expr)
  (and (pair? expr) (memq op expr)))

(define (unwrap expr)
  (if (and (pair? expr) (null? (cdr expr)))
      (car expr)
      expr))

(define (after op ls)
  (unwrap (cdr (memq op ls))))

(define (before op expr)
  (define (helper ls)
    (if (eq? op (car ls))
        '()
        (cons (car ls) (helper (cdr ls)))))
  (unwrap (helper expr)))

(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
              (* m1 m2))
        (else (list m1 '* m2))))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        (else (list b '** e))))

(define (sum? expr) (has? '+ expr))
(define (addend s) (before '+ s))
(define (augend s) (after '+ s))

(define (product? expr) (and (not (sum? expr)) (has? '* expr)))
(define (multiplier s) (before '* s))
(define (multiplicand s) (after '* s))

(define (exponentiation? expr) (and (not (product? expr)) (has? '** expr)))
(define (base s) (before '** s))
(define (exponent s) (after '** s))
