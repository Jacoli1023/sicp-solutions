#lang racket
(provide (all-defined-out))

(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (for-each proc items)
  (when (not (null? items))
    (proc (car items))
    (for-each proc (cdr items))))

(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))
