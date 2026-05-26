#lang sicp
(#%require "table.rkt" "tags.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Generic apply procedure taken from section 2.4.3
;; --------------------------------------------------------------------

(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (error 'apply-generic "no method for argument types" op type-tags))))
