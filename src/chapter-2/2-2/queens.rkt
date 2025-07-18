#lang racket
(require "sequence-operations.rkt")
(provide (all-defined-out))

(define (make-position row col)
  (list row col))
(define (get-row pos)
  (car pos))
(define (get-col pos)
  (cadr pos))

(define empty-board '())
(define (adjoin-position row col board)
  (cons (make-position row col) board))

(define (safe? k positions)
  (define (attacks? queen1 queen2)
    (let ((row1 (get-row queen1))
          (col1 (get-col queen1))
          (row2 (get-row queen2))
          (col2 (get-col queen2)))
      (or (= row1 row2) ; same row
          (= col1 col2) ; same col
          (= (+ row1 col1)
             (+ row2 col2)) ; same upwards diag
          (= (- row1 col1)
             (- row2 col2))))) ; same downwards diag
  (let ((k-queen (car positions)))
    (define (helper rest-of-queens)
      (cond ((null? rest-of-queens) #t)
            ((attacks? k-queen (car rest-of-queens)) #f)
            (else (helper (cdr rest-of-queens)))))
    (helper (cdr positions))))

(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size))
