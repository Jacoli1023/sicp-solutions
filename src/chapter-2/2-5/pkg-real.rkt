#lang sicp
(#%require "tags.rkt" "table.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Real package - the rung above rational in the numeric tower
;; --------------------------------------------------------------------

(define (install-real-package)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ?   '(real real) =)
  (put '=zero? '(real) zero?)
  (put 'make 'real tag)
  'done)

(define (make-real x)
  ((get 'make 'real) x))
