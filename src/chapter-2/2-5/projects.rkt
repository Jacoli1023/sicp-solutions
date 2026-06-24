#lang sicp
(#%require "tags.rkt" "table.rkt" "tower.rkt" "apply-generic.rkt" "generics.rkt"
           "pkg-integer.rkt" "pkg-rational.rkt" "pkg-real.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Tower projection - installs 'project for each rung except the bottom.
;; Inverse of raises.rkt. The procs receive untagged contents (apply-generic
;; strips tags), so each one knows the shape of its own representation.
;; --------------------------------------------------------------------

(define (install-projects)
  ;; 2.86: real-part of a complex can now be a tagged tower value
  ;; (integer, rational, real). Raise it up to real before wrapping.
  ;; The pair?/type-level check falls back to make-real for raw
  ;; scheme-numbers, preserving the older test paths.
  (define (complex->real z)
    (let ((rp (real-part z)))
      (if (and (pair? rp) (type-level (type-tag rp)))
          (raise-to 'real rp)
          (make-real rp))))
  (define (real->rational x)
    (let ((y (inexact->exact x)))
      (make-rational (numerator y) (denominator y))))
  (define (rational->integer r)
    (make-integer (quotient (car r) (cdr r))))

  (put 'project '(complex)  complex->real)
  (put 'project '(real)     real->rational)
  (put 'project '(rational) rational->integer)
  'done)
