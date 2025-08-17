#lang racket
(require "hash-table.rkt")

(define (get-record name file)
  (let* ((division (type-tag file))
        (record
         ((get 'get-record division) name (contents file))))
    (if record
        (attach-tag division record)
        #f)))

(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))

(define (find-employee-record name divisions)
  (if (null? divisions)
      #f
      (or (get-record name (car divisions))
          (find-employee-record name (cdr divisions)))))
