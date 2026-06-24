#lang sicp
(#%require "tags.rkt" "table.rkt"
           "pkg-integer.rkt" "pkg-rational.rkt" "pkg-real.rkt" "pkg-complex.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Tower promotion - installs 'raise for each rung except the top.
;; The procs receive untagged contents (apply-generic strips tags),
;; so each one knows the shape of its own representation.
;; --------------------------------------------------------------------

(define (install-raises)
  ;; scheme-number is a "legacy" convenience type. Raising it places a
  ;; bare value onto the tower: exact integers go to 'integer, exact
  ;; ratios to 'rational, inexact numbers to 'real. - Exercise 2.86
  (define (scheme-number->tower n)
    (cond ((and (exact? n) (integer? n)) (make-integer n))
          ((exact? n) (make-rational (numerator n) (denominator n)))
          (else (make-real n))))
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real r)
    (make-real (exact->inexact (/ (car r) (cdr r)))))
  (define (real->complex x)
    (make-complex-from-real-imag x 0))

  (put 'raise '(scheme-number) scheme-number->tower)
  (put 'raise '(integer)       integer->rational)
  (put 'raise '(rational)      rational->real)
  (put 'raise '(real)          real->complex)
  'done)
