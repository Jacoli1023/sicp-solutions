#lang racket
(provide (all-defined-out))

(define (make-mobile left right)
  (list left right))
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (make-branch length structure)
  (list length structure))
(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))

(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (number? struct)
        struct
        (total-weight struct))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))

(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced? mobile)
  (define (branch-balanced? branch)
    (let ((struct (branch-structure branch)))
      (or (number? struct)
          (balanced? struct))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left) (torque right))
         (branch-balanced? left)
         (branch-balanced? right))))
