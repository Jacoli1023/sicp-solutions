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
  ;; Transcendentals live here, and only here. Integer/rational
  ;; arguments are raised to real automatically by the tower-aware
  ;; dispatcher, so no other numeric package needs these methods.
  (put 'square-root '(real)      (lambda (x) (tag (sqrt x))))
  (put 'sine        '(real)      (lambda (x) (tag (sin x))))
  (put 'cosine      '(real)      (lambda (x) (tag (cos x))))
  (put 'arctan      '(real real) (lambda (y x) (tag (atan y x))))
  (put 'make 'real tag)
  'done)

(define (make-real x)
  ((get 'make 'real) x))
