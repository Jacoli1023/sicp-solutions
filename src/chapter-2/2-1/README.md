# [2.1 Introduction to Data Abstraction](https://sarabander.github.io/sicp/html/2_002e1.xhtml#g_t2_002e1)

### Exercise 2.1

Solution:

```scheme
(define (make-rat n d)
  (define (sign x)
    (cond ((positive? x) 1)
          ((zero? x) 0)
          ((negative? x) -1)))
  (let ((g (gcd n d))
        (s (* (sign n) (sign d))))
    (cons (* s (/ (abs n) g))
          (/ (abs d) g))))
```

Tests:
```scheme
> (make-rat 3 4)
'(3 . 4)
> (make-rat -3 4)
'(-3 . 4)
> (make-rat 3 -4)
'(-3 . 4)
> (make-rat -3 -4)
'(3 . 4)
```

---
### Exercise 2.2

Solution ([linesegment.rkt](./linesegments.rkt)):

- line segment representation:
```scheme
(define (make-segment start end)
  (cons start end))
(define (start-segment seg)
  (car seg))
(define (end-segment seg)
  (cdr seg))
```

- point representation:
```scheme
(define (make-point x y)
  (cons x y))
(define (x-point p)
  (car p))
(define (y-point p)
  (cdr p))
```

- finding the midpoint of a line segment:
```scheme
(define (midpoint-segment seg)
  (let ((start (start-segment seg))
        (end (end-segment seg)))
    (make-point (average (x-point start) (x-point end))
                (average (y-point start) (y-point end)))))
```

Tests:
```scheme
> (define a (make-segment (make-point -1 -2) (make-point 3 4)))
> a
'((-1 . -2) 3 . 4)
> (start-segment a)
'(-1 . -2)
> (end-segment a)
'(3 . 4)
> (midpoint-segment a)
'(1 . 1)
```
