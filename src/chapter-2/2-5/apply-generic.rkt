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

;; --------------------------------------------------------------------
;; Generic apply procedure with coercion taken from section 2.5.2
;; --------------------------------------------------------------------

(define (new-apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (err)
      (error "No method for these types" (list op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags)))
              (if (eq? type1 type2)
                  (err)
                  (let ((a1 (car args))
                        (a2 (cadr args))
                        (t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2 (new-apply-generic op (t1->t2 a1) a2))
                          (t2->t1 (new-apply-generic op a1 (t2->t1 a2)))
                          (else (err))))))
            (err)))))

;; --------------------------------------------------------------------
;; Generalized apply procedure with N-arg coercion - Exercise 2.82
;; --------------------------------------------------------------------

(define (coerce-arg target arg)
  (let ((source (type-tag arg)))
    (if (eq? source target)
        arg
        (let ((coerce (get-coercion source target)))
          (if coerce (coerce arg) false)))))

(define (coerce-args target args)
  (define (loop args acc)
    (if (null? args)
        (reverse acc)
        (let ((coerced (coerce-arg target (car args))))
          (if coerced
              (loop (cdr args) (cons coerced acc))
              false))))
  (loop args '()))

(define (multi-apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (try-targets candidates)
      (if (null? candidates)
          (error "No method for these types" (list op type-tags))
          (let* ((target (car candidates))
                 (coerced (coerce-args target args)))
            ;; only recurse if coercion changed the signature, else
            ;; (op T T ...) with no method would loop forever
            (if (and coerced
                     (not (equal? (map type-tag coerced) type-tags)))
                (apply multi-apply-generic op coerced)
                (try-targets (cdr candidates))))))
    (if proc
        (apply proc (map contents args))
        (try-targets type-tags))))
