#lang sicp
(#%require "tags.rkt" "table.rkt" "tower.rkt" "generics.rkt" "apply-generic.rkt"
           "pkg-scheme-number.rkt" "pkg-integer.rkt" "pkg-rational.rkt"
           "pkg-real.rkt" "pkg-polar.rkt" "pkg-rectangular.rkt" "pkg-complex.rkt"
           "coercions.rkt" "raises.rkt")
(#%provide (all-defined)
           (all-from "tags.rkt")
           (all-from "table.rkt")
           (all-from "tower.rkt")
           (all-from "generics.rkt")
           (all-from "apply-generic.rkt")
           (all-from "pkg-scheme-number.rkt")
           (all-from "pkg-integer.rkt")
           (all-from "pkg-rational.rkt")
           (all-from "pkg-real.rkt")
           (all-from "pkg-polar.rkt")
           (all-from "pkg-rectangular.rkt")
           (all-from "pkg-complex.rkt")
           (all-from "coercions.rkt")
           (all-from "raises.rkt"))

; procedure helps us load packages with a clean slate
(define (using . installers)
  (reset)
  (for-each (lambda (f) (f)) installers))

; bundles all arithemtic packages into a numeric package
(define (numeric-pkg)
  (install-scheme-number-package)
  (install-integer-package)
  (install-rational-package)
  (install-real-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package)
  (install-coercions)
  (install-raises))
