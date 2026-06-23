#lang sicp
(#%require "tags.rkt" "table.rkt"
           "pkg-rational.rkt" "pkg-real.rkt" "pkg-complex.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Tower promotion - installs 'raise for each rung except the top.
;; The procs receive untagged contents (apply-generic strips tags),
;; so each one knows the shape of its own representation.
;; --------------------------------------------------------------------

(define (install-raises)
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real r)
    (make-real (exact->inexact (/ (car r) (cdr r)))))
  (define (real->complex x)
    (make-complex-from-real-imag x 0))

  (put 'raise '(integer)  integer->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real)     real->complex)
  'done)
