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

---
### Exercise 2.3

```scheme
(define (area rect)
  (* (length-rect rect) (width-rect rect)))
(define (perimeter rect)
  (+ (* 2 (length-rect rect))
     (* 2 (width-rect rect))))
```

Solution:

- the first representation for rectangles is rather simple: via its dimensions, length and width. This easily translates into making a rectangle:
```scheme
(define (make-rectangle l w)
  (cons l w))
(define (length-rect rect)
  (car rect))
(define (width-rect rect)
  (cdr rect))
```

This assumes we are creating the rectangle at the origin point. If this is not the case, we can also have our `make-rectangle` constructor also take in a point as an argument, along with creating a selector for the point.

Here are some tests for this representation:
```scheme
> (define r (make-rectangle 4 7))
> (area r)
28
> (perimeter r)
22
```

- another representation for rectangles is using two corner points. Here's how this representation looks:
```scheme
(define (make-rectangle corner)
  (cons (make-point 0 0) corner))
(define origin car)
(define corner cdr)
(define (length-rect rect)
  (x-point (corner rect)))
(define (width-rect rect)
  (y-point (corner rect)))
```

Once again, we are assuming one of the points is the origin. If that is not the case, we'd simply have `make-rectangle` accept another point as an argument, and then subtract the x-coords and the y-coords of each point in order to find the length and width, respectively.

Here are some tests using this representation:
```scheme
> (define r (make-rectangle (make-point 4 7)))
> (area r)
28
> (perimeter r)
22
```

We get the same answer for both representations without needing to change the `area` or `perimeter` procedures at all. Thus, we were successfully able to set up some abstraction barriers between the procedures that occupy different levels within the system.

---
### Exercise 2.4

```scheme
(define (cons x y) 
  (lambda (m) (m x y)))

(define (car z) 
  (z (lambda (p q) p)))
```

Solution:

- `cdr` representation:
```scheme
(define (cdr z)
  (z (lambda (p q) q)))
```

Tests:
```scheme
> (car (cons 4 7))
4
> (cdr (cons 4 7))
7
```

---
### Exercise 2.5

Solution:
```scheme
(define (count-div x y)
  (define (count z n)
    (let ((q (/ z y)))
      (if (integer? q)
          (count q (+ n 1))
          n)))
  (count x 0))

(define (cons a b)
  (* (expt 2 a)
     (expt 3 b)))
(define (car x)
  (count-div x 2))
(define (cdr x)
  (count-div x 3))
```

`count-div` returns the amount of times some number `x` can be successively divided by another number `y`. We can use this as a helper function for our cons representation which satisfies the result of $2^a * 3^b$. To find the `car` of the pair (represented by _a_ in the equation), we then successively divide the result of the equation by 2. The `cdr` (_b_), we succesively divide by 3.

---
### Exercise 2.6

```scheme
(define zero (lambda (f) (lambda (x) x)))

(define (add-1 n)
  (lambda (f) (lambda (x) (f ((n f) x)))))
```

Solution:
```scheme
(define one (lambda (f) (lambda (x) (f x))))
(define two (lambda (f) (lambda (x) (f (f x)))))

(define (add a b)
  (lambda (f) (lambda (x) ((a f) ((b f) x)))))
```

---
### Exercise 2.7

Solution [`interval.rkt`](./intervals.rkt):

To implement the [interval arithmetic](./interval-arithmetic.rkt) procedures, we need the appropriate set of constructors and selectors:

```scheme
(define (make-interval a b) (cons a b))
(define (lower-bound interval) (car interval))
(define (upper-bound interval) (cdr interval))
```

---
### Exercise 2.8

Solution:

For subtracting two intervals, the minimum value the difference could be is the lower-bound of one interval subtracted by the upper-bound of the other. Vice versa, the maximum value the difference could be is the upper-bound of one number subtracted by the lower-bound of the other.

```scheme
(define (sub-interval x y)
  (make-interval (- (lower-bound x) (upper-bound y))
                 (- (upper-bound x) (lower-bound y))))
```

---
### Exercise 2.9

```scheme
(define (width x)
  (/ (- (upper-bound x) (lower-bound x)) 2))
```

Solution:

To these out this hypothesis, we can create arbitrary intervals:

```scheme
(define x1 (random 50)) ; 25
(define x2 (+ (random 50) 50)) ; 95
(define y1 (random 50)) ; 38
(define y2 (+ (random 50) 50)) ; 53
(define x (make-interval x1 x2))
(define y (make-interval y1 y2))
```

Making the lower-bounds a random number between 0-50 and an upper-bound a random number between 50-100 is just to ensure that the interval we create satisfies the given condition that the lower-bound is less than or equal to the upper-bound.

Testing the actual width values:

```scheme
> (width x)
35
> (width y)
7.5
```

Now, by adding (or subtracting) the two intervals, we can confirm that the width of the sum is the sum of the widths:

```scheme
> (width (add-interval x y))
42.5 ; 35 + 7.5
> (width (sub-interval x y))
42.5 ; 35 + 7.5
```

However, the width of the product or quotient is not a function of the widths being multiplied or divided:

```scheme
> (width (mul-interval x y))
2042.5 ; != 35 * 7.5
> (width (div-interval x y))
1.0141509433962264 ; != 35 / 7.5
```

---
### Exercise 2.10

Solution:

```scheme
(define (div-interval x y)
  (if (and (negative? (lower-bound y)) (positive? (upper-bound y)))
      (error 'div-interval "can't divide by an interval that spans 0" y)
      (mul-interval x
                    (make-interval (/ 1.0 (upper-bound y))
                                   (/ 1.0 (lower-bound y))))))
```

---
### Exercise 2.11

Solution:
```scheme
(define (mul-interval x y)
  (let ((x1 (lower-bound x))
        (x2 (upper-bound x))
        (y1 (lower-bound y))
        (y2 (upper-bound y)))
    (cond ((positive? x1)
           (cond ((positive? y1) (make-interval (* x1 y1) (* x2 y2)))
                 ((negative? y2) (make-interval (* x2 y1) (* x1 y2)))
                 (else (make-interval (* x2 y1) (* x2 y2)))))
          ((negative? x2)
           (cond ((negative? y2) (make-interval (* x2 y2) (* x1 y1)))
                 ((positive? y1) (make-interval (* x1 y2) (* x2 y1)))
                 (else (make-interval (* x1 y2) (* x1 y1)))))
          (else
           (cond ((positive? y1) (make-interval (* x1 y2) (* x2 y2)))
                 ((negative? y2) (make-interval (* x2 y1) (* x1 y1)))
                 (else (make-interval (min (* x2 y1) (* x1 y2))
                                      (max (* x2 y2) (* x1 y1)))))))))
```

Doing too much.

---
### Exercise 2.12

```scheme
(define (make-center-width c w)
  (make-interval (- c w) (+ c w)))

(define (center i)
  (/ (+ (lower-bound i) (upper-bound i)) 
     2))

(define (width i)
  (/ (- (upper-bound i) (lower-bound i)) 
     2))
```

Solution ([`intervals.rkt`](./intervals.rkt)):

```scheme
(define (make-center-percent c p)
  (make-center-width c (* c (/ p 100))))
(define (percent interval)
  (* (/ (width interval) (center interval)) 100))
```

---
### Exercise 2.13

Solution:

Assuming that the tolerance percentage is small, the simple formula for approximating the percentage tolerance of the product of two intervals is simply _the sum of the tolerance percentages of the factors_.

To show this, consider two intervals _a_ and _b_ represented in center-tolerance form:

$$
\begin{aligned}
a &= c_a \pm t_a \\
b &= c_b \pm t_b
\end{aligned}
$$

The product of the two intervals:

$$
\begin{aligned}
ab &= (c_a \pm t_a)(c_b \pm t_b) \\
   &= c_ac_b \pm (c_at_b + c_bt_a) + t_at_b
\end{aligned}
$$

Since $t_a$ and $t_b$ are small, we can ignore their product. We are then left with the given center-tolerance form:

$$
\begin{aligned}
ab \approx c_ac_b \pm (c_at_b + c_bt_a)
\end{aligned}
$$

With the center representing $c_ac_b$ and the tolerance (width) representing the $\pm (c_at_b + c_bt_a)$. Since we care about the tolerance as a percentage, we can the tolerance percentage of the product, _T_, by finding the ratio of the width of interval to the center of the interval:

$$
\begin{aligned}
T_{ab} &= t_{ab}/c_{ab} \\
&= (c_at_b + c_bt_a)/c_ac_b \\
&= t_b/c_b + t_a/c_a
\end{aligned}
$$

Thus, the approximate tolerance percentage of the product of two intervals is the sum of the two intervals' respective tolerance percentages.

---
### Exercise 2.14
