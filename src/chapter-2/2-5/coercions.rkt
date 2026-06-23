#lang sicp
(#%require "tags.rkt" "table.rkt"
           "pkg-integer.rkt" "pkg-rational.rkt" "pkg-complex.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Cross-type coercions - populates the coercion-table from 2.5.2
;; --------------------------------------------------------------------

(define (install-coercions)
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (rational->complex r)
    (let ((nd (contents r)))
      (make-complex-from-real-imag (/ (car nd) (cdr nd)) 0)))

  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex  scheme-number->complex)
  (put-coercion 'rational      'complex  rational->complex)
  'done)
