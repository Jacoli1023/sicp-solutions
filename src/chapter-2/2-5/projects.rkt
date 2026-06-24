#lang sicp
(#%require "tags.rkt" "table.rkt" "generics.rkt"
           "pkg-integer.rkt" "pkg-rational.rkt" "pkg-real.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Tower projection - installs 'project for each rung except the bottom.
;; Inverse of raises.rkt. The procs receive untagged contents (apply-generic
;; strips tags), so each one knows the shape of its own representation.
;; --------------------------------------------------------------------

(define (install-projects)
  (define (complex->real z)
    (make-real (real-part z)))
  (define (real->rational x)
    (let ((y (inexact->exact x)))
      (make-rational (numerator y) (denominator y))))
  (define (rational->integer r)
    (make-integer (quotient (car r) (cdr r))))

  (put 'project '(complex)  complex->real)
  (put 'project '(real)     real->rational)
  (put 'project '(rational) rational->integer)
  'done)
