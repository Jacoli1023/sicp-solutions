#lang sicp
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; The numeric type tower and navigation helpers - Exercise 2.84
;; Edit type-tower to insert a new rung; nothing else in the system
;; needs to know about the change.
;; --------------------------------------------------------------------

(define type-tower '(integer rational real complex))

(define (type-level type)
  (define (iter tower n)
    (cond ((null? tower) false)
          ((eq? type (car tower)) n)
          (else (iter (cdr tower) (+ n 1)))))
  (iter type-tower 0))

(define (highest-type types)
  ;; Non-tower types (e.g., 'scheme-number) compare as below the
  ;; lowest tower rung, so a tower type always wins when present.
  (define (level t) (or (type-level t) -1))
  (define (higher t1 t2)
    (if (> (level t1) (level t2)) t1 t2))
  (define (iter types highest)
    (if (null? types) highest
        (iter (cdr types) (higher (car types) highest))))
  (iter (cdr types) (car types)))
