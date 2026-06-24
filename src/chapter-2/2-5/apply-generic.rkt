#lang sicp
(#%require "table.rkt" "tags.rkt" "tower.rkt")
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

;; --------------------------------------------------------------------
;; Tower-aware apply procedure - Exercise 2.84, extended in 2.86
;; First normalizes arguments to a common rung (highest-type among
;; them); if no method is found there, climbs one more rung and
;; retries. The second step lets a single-arg call like (cosine integer)
;; reach the real-only 'cosine method by walking the 'raise chain.
;; --------------------------------------------------------------------

(define (raise-to target arg)
  (let ((source (type-tag arg)))
    (if (eq? source target)
        arg
        (let ((raise-proc (get 'raise (list source))))
          (if raise-proc
              (raise-to target (raise-proc (contents arg)))
              false)))))

(define (raise-all-to target args)
  (define (iter args acc)
    (if (null? args)
        (reverse acc)
        (let ((raised (raise-to target (car args))))
          (if raised
              (iter (cdr args) (cons raised acc))
              false))))
  (iter args '()))

(define (raise-once arg)
  (let ((rp (get 'raise (list (type-tag arg)))))
    (and rp (rp (contents arg)))))

(define (raise-all-once args)
  (define (iter args acc)
    (if (null? args)
        (reverse acc)
        (let ((r (raise-once (car args))))
          (if r
              (iter (cdr args) (cons r acc))
              false))))
  (iter args '()))

(define (tower-apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let* ((target (highest-type type-tags))
               (raised (raise-all-to target args)))
          (cond
           ;; Normalize succeeded and changed the signature: retry.
           ((and raised
                 (not (equal? (map type-tag raised) type-tags)))
            (apply tower-apply-generic op raised))
           ;; Same signature - try climbing one more rung. Lets a
           ;; single-arg call like (cosine integer) walk up to real.
           (else
            (let ((climbed (raise-all-once args)))
              (if (and climbed
                       (not (equal? (map type-tag climbed) type-tags)))
                  (apply tower-apply-generic op climbed)
                  (error "No method for these types" (list op type-tags))))))))))

;; --------------------------------------------------------------------
;; Simplifying apply procedure - Exercise 2.85
;; drop uses the basic apply-generic for its internal raise/project/equ?
;; calls, so a simplifying outer dispatcher won't recursively re-drop.
;; --------------------------------------------------------------------

(define (drop x)
  (let ((type (type-tag x)))
    (cond ((not (type-level type)) x)
          ((= (type-level type) 0) x)
          (else
           (let ((projected (apply-generic 'project x)))
             (if (apply-generic 'equ? x (apply-generic 'raise projected))
                 (drop projected)
                 x))))))

(define (tower-value? x)
  (and (pair? x) (type-level (type-tag x))))

(define (simplifying-apply-generic op . args)
  (let ((result (apply tower-apply-generic op args)))
    (if (tower-value? result) (drop result) result)))
