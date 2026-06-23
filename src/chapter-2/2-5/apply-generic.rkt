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
