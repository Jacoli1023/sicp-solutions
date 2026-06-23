#lang sicp
(#%require "tags.rkt" "table.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Integer package - bottom of the numeric tower
;; --------------------------------------------------------------------

(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (quotient x y))))
  (put 'equ?   '(integer integer) =)
  (put '=zero? '(integer) zero?)
  (put 'make 'integer tag)
  'done)

(define (make-integer n)
  ((get 'make 'integer) n))
