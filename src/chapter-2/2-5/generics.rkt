#lang sicp
(#%require "table.rkt" "apply-generic.rkt")
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Generic arithmetic procedures from 2.5.1
;;
;; The arithmetic and predicate ops use simplifying-apply-generic so
;; that mixed-tower-type calls (e.g., from inside the complex package's
;; internal arithmetic in 2.86) raise to a common rung automatically
;; and the result is dropped as far as it can go.
;;
;; raise and project stay on the basic apply-generic: simplifying them
;; would defeat their purpose -- (raise integer) would compute a
;; rational and then drop it right back to integer.
;; --------------------------------------------------------------------
(define (add x y) (simplifying-apply-generic 'add x y))
(define (sub x y) (simplifying-apply-generic 'sub x y))
(define (mul x y) (simplifying-apply-generic 'mul x y))
(define (div x y) (simplifying-apply-generic 'div x y))

(define (equ?   x y) (simplifying-apply-generic 'equ?   x y))
(define (=zero? x)   (simplifying-apply-generic '=zero? x))

(define (raise   x) (apply-generic 'raise   x))
(define (project x) (apply-generic 'project x))

;; --------------------------------------------------------------------
;; Generic transcendentals - Exercise 2.86
;; Installed only on 'real (see pkg-real.rkt); other tower types are
;; raised to real automatically before dispatch.
;; --------------------------------------------------------------------
(define (square      x)   (mul x x))
(define (square-root x)   (simplifying-apply-generic 'square-root x))
(define (sine        x)   (simplifying-apply-generic 'sine x))
(define (cosine      x)   (simplifying-apply-generic 'cosine x))
(define (arctan      y x) (simplifying-apply-generic 'arctan y x))

;; --------------------------------------------------------------------
;; Generic complex selector procedures from 2.4.3
;; --------------------------------------------------------------------
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; --------------------------------------------------------------------
;; Generic complex constructor procedures from 2.4.3
;; --------------------------------------------------------------------
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
