#lang sicp
(#%provide (all-defined))

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (reset!)
      (set-cdr! local-table '()))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'reset-proc!) reset!)
            (else (error 'make-table "unknown operation" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define reset (operation-table 'reset-proc!))

; Simple coercion table, just define it in terms of `get` and `put`
(define (get-coercion type1 type2)
  (get 'coerce (list type1 type2)))
(define (put-coercion type1 type2 coerce)
  (put 'coerce (list type1 type2) coerce))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error 'type-tag "bad tagged datum" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error 'contents "bad tagged datum" datum))))

; apply-generic now works with coercion types
;; (define (apply-generic op . args)
;;   (let* ((type-tags (map type-tag args))
;;          (proc (get op type-tags)))
;;     (define (err-sig)
;;       (error "No method for these types" (list op type-tags)))
;;     (if proc
;;         (apply proc (map contents args))
;;         (if (= (length args) 2)
;;             (let* ((type1 (car type-tags))
;;                    (type2 (cadr type-tags))
;;                    (a1 (car args))
;;                    (a2 (cadr args))
;;                    (t1->t2 (get-coercion type1 type2))
;;                    (t2->t1 (get-coercion type2 type1)))
;;               (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
;;                     (t2->t1 (apply-generic op a1 (t2->t1 a2)))
;;                     (else (err-sig))))
;;             (err-sig)))))

(define (apply-generic op . args)

  (define (coercable? coerce-procs)
    (not (member #f coerce-procs)))

  (define (coerce-args coercion-procs args)
    (map (lambda (coerce-proc arg)
           (coerce-proc arg))
         coercion-procs
         args))

  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (try-coercion tags)
      (if (null? tags)
          (error "No method for these types - APPLY-GENERIC" (list op type-tags))
          (let* ((target-type (car tags))
                 (arg-coercions (map (lambda (coerce-from)
                                       (if (eq? coerce-from target-type)
                                           identity
                                           (get-coercion coerce-from target-type)))
                                     type-tags)))
            (if (coercable? arg-coercions)
                (apply apply-generic
                       op
                       (coerce-args arg-coercions args))
                (try-coercion (cdr tags))))))
    (if proc
        (apply proc (map contents args))
        (try-coercion type-tags))))


(define (apply-specific op type . args)
  (let ((proc (get op type)))
    (if proc
        (apply proc args)
        (error op "no method for type" op type))))
