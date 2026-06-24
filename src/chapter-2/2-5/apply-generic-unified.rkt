#lang sicp
(#%require "table.rkt" "tags.rkt" "tower.rkt")
(#%provide unified-apply-generic)

;; --------------------------------------------------------------------
;; Exercise 2.85, alternative — a single apply-generic that handles
;; dispatch, tower-coercion, and result-simplification in one procedure.
;;
;; This is the version that matches the book's cadence of "keep editing
;; apply-generic" instead of stacking new variants. It is archived here
;; for reference and is intentionally NOT pulled in by install.rkt; the
;; rest of the system continues to use the stacked dispatchers in
;; apply-generic.rkt. See README.md (exercise 2.85) for the comparison.
;; --------------------------------------------------------------------

(define (raise-to target arg)
  (let ((source (type-tag arg)))
    (if (eq? source target)
        arg
        (let ((up (get 'raise (list source))))
          (and up (raise-to target (up (contents arg))))))))

(define (raise-all-to target args)
  (define (iter args acc)
    (if (null? args)
        (reverse acc)
        (let ((raised (raise-to target (car args))))
          (and raised (iter (cdr args) (cons raised acc))))))
  (iter args '()))

;; drop walks the operation table by hand instead of going through
;; apply-generic; that's what prevents infinite recursion once
;; apply-generic itself starts auto-simplifying results.
(define (drop x)
  (let ((type (type-tag x)))
    (cond ((not (type-level type)) x)
          ((= (type-level type) 0) x)
          (else
           (let* ((proj  (get 'project (list type)))
                  (lower (and proj (proj (contents x))))
                  (lift  (and lower (get 'raise (list (type-tag lower)))))
                  (back  (and lift (lift (contents lower))))
                  (eq??  (and back (get 'equ? (list type type)))))
             (if (and eq?? (eq?? (contents x) (contents back)))
                 (drop lower)
                 x))))))

(define (unified-apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (cond
     (proc
      (let ((result (apply proc (map contents args))))
        ;; Simplify only when (a) the result is a tower value, and
        ;; (b) op isn't one of the tower-mechanic ops — otherwise the
        ;; user-facing (raise x) would just be dropped back to x.
        (if (and (not (memq op '(raise project equ?)))
                 (pair? result)
                 (type-level (type-tag result)))
            (drop result)
            result)))
     (else
      (let* ((target (highest-type type-tags))
             (raised (raise-all-to target args)))
        ;; same signature-changed guard as in apply-generic.rkt
        (if (and raised
                 (not (equal? (map type-tag raised) type-tags)))
            (apply unified-apply-generic op raised)
            (error "No method for these types" (list op type-tags))))))))
