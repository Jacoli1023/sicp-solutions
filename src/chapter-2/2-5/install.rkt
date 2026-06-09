#lang sicp
(#%require "tags.rkt" "table.rkt" "generics.rkt" "apply-generic.rkt" "pkg-integer.rkt" "pkg-rational.rkt" "pkg-polar.rkt" "pkg-rectangular.rkt" "pkg-complex.rkt")
(#%provide (all-defined))

; procedure helps us load packages with a clean slate
(define (using . installers)
  (reset)
  (for-each (lambda (f) (f)) installers))

; bundles all arithemtic packages into a numeric package
(define (numeric-pkg)
  (install-scheme-number-package)
  (install-rational-package)
  (install-rectangular-package)
  (install-polar-package)
  (install-complex-package))

; loads each package separately
; better done in the REPL so I can load and reset at runtime
;;(using install-scheme-number-package
;;      install-rational-package
;;      install-rectangular-package
;;      install-polar-package
;;      install-complex-package)
