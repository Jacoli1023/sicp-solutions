# [1.3 Formulating Abstractions with Higher-Order Procedures](https://sarabander.github.io/sicp/html/1_002e3.xhtml#g_t1_002e3)

### Exercise 1.29

Solution:\
From [`integral.rkt`](./integral.rkt):

```scheme
(define (simpson f a b n)
  (let ((h (/ (- b a) n)))
    (define (term k)
      (* (f (+ a (* k h)))
         (cond ((or (= k 0) (= k n)) 1)
               ((odd? k) 4)
               (else 2))))
    (* (/ h 3) (sum term 0.0 inc n))))
```

Comparing Simpson's Rule to the previous `integral` procedure:
```scheme
> (integral cube 0 1 0.01)
0.24998750000000042
> (simpson cube 0 1 100)
0.24999999999999992
> (integral cube 0 1 0.001)
0.249999875000001
> (simpson cube 0 1 1000)
0.2500000000000003
```

Simpson's Rule is, in fact, more accurate.

---
### Exercise 1.30

Solution:

```scheme
(define (sum term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (+ result (term a)))))
  (iter a 0))
```

---
### Exercise 1.31

Solution:

- recursive process:

```scheme
(define (product term a next b)
  (if (> a b)
      1
      (* (term a)
         (product term (next a) next b))))
```

- iterative process:

```scheme
(define (product term a next b)
  (define (iter a result)
    (if (> a b)
        result
        (iter (next a) (* result (term a)))))
  (iter a 1))
```

Using the `product` procedure to define a `factorial` procedure:

```scheme
(define (factorial n)
  (product identity 1 inc n))
```

Tests:
```scheme
> (factorial 7)
5040
> (factorial 10)
3628800
```

And a pi approximation procedure:

```scheme
(define (approx-pi n)
  (define (term k)
    (let ((r (remainder k 2)))
      (/ (- (+ k 2) r)
         (+ k r 1))))
  (* 4 (product term 1 inc n)))
```

Tests:
```scheme
> (approx-pi 100)
3.1570301764551654
> (approx-pi 1000)
3.1431607055322552
> (approx-pi 10000)
3.1417497057380084
> (approx-pi 100000)
3.141608361277941
```

The bigger _n_ gets, the closer the procedure comes to computing $\pi$.

---
### Exercise 1.32

Solution:

- recursive process:

```scheme
(define (accumulate combiner null-value term a next b)
  (if (> a b)
    null-value
    (combiner (term a)
              (accumulate combiner null-value term (next a) next b))))
```

- iterative process:

```scheme
(define (accumulate combiner null-value term a next b)
  (define (iter a result)
    (if (> a b)
      result
      (iter (next a) (combiner result (term a)))))
  (iter a null-value))
```

- `sum` and `product` in terms of `accumulate`:

```scheme
(define (sum term a next b)
  (accumulate + 0 term a next b))

(define (product term a next b)
  (accumulate * 1 term a next b))
```

Testing these out with the previously defined `factorial` and `integral` procedures yields the same results:

```scheme
> (factorial 5)
120
> (integral cube 0 1 0.001)
0.24999987500000073
```

---
### Exercise 1.33

Solution:

```scheme
(define (filtered-accumulate combiner pred? null-value term a next b)
  (define (iter a result)
    (cond ((> a b)
           result)
          ((pred? a)
           (iter (next a) (combiner result (term a))))
          (else
           (iter (next a) result))))
  (iter a null-value))
```

- the sum of the squares of the prime numbers in the interval _a_ to _b_ (taking `prime?` from [`prime.rkt`](../1-2/prime.rkt)):

```scheme
(define (sum-squares-prime a b)
  (filtered-accumulate + prime? 0 square a inc b))
```

Tests:

```scheme
> (sum-squares-prime 1 10)
88
> (sum-squares-prime 10 15)
290
```

- the product of all the positive integers less than _n_ that are relatively prime to _n_

```scheme
(define (product-rel-prime n)
  (define (rel-prime? i)
    (= (gcd i n) 1))
  (filtered-accumulate * rel-prime? identity 1 inc (- n 1)))
```

Testing this out:

```scheme
> (product-rel-prime 10)
189 ; 3 * 7 * 9
> (product-rel-prime 15)
896896 ; 2 * 4 * 7 * 8 * 11 * 13 * 14
```

---
### Exercise 1.34

```scheme
(define (f g) (g 2))
```

Solution:\
The problem that occurs when we try to evaluate `(f f)` is that the interpreter would then try to evaluate this as `(f (f 2))`. The interpreter then tries to evaluate applicatively `(f 2)`, which is `(2 2)`. _2_ is not a procedure, and trying to evaluate it as such would produce an error.

---
### Exercise 1.35

[`fixedpoint.rkt`](./fixedpoint.rkt):
```scheme
(define tolerance 0.00001)
(define (close-enough? x y)
  (< (abs (- x y)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))
```

Solution:

To show that the golden ratio $\varphi$ is a fixed point of the transformation $x \mapsto 1 + 1/x$, we must first remember a few things:

- $\varphi = \frac{1 + \sqrt5}{2}$
- the fixed point of a function is the number _x_ which satisfies $f(x) = x$

Thus, using algebraic rules, we can prove that the golden ratio is a fixed point of the previous stated transformation:

$$
\begin{aligned}
f(x) &= x \\
1 + 1/x &= x \\
x-1 &= 1/x \\
x^2 - x &= 1 \\
x^2 - x - 1 &= 0 \\
\dfrac{1 \pm \sqrt{1 + 4}}{2} &= x \\
\dfrac{1 \pm \sqrt5}{2} &= x \\
&= \varphi.
\end{aligned}
$$

Computing $\varphi$ by means of the `fixed-point` procedure:

```scheme
> (fixed-point
    (lambda (x) (+ 1 (/ 1.0 x)))
    1)
1.6180327868852458
> (/ (+ 1 (sqrt 5)) 2)
1.618033988749895
```

---
### Exercise 1.36

```scheme
(define (f x)
  (/ (log 1000) (log x)))

(define (f-damp x) (average x (f x)))
```

Solution:

- without average damping, the procedure takes 35 steps:

```scheme
> (fixed-point-verbose f 2)
9.965784284662087
3.004472209841214
6.279195757507157
...
4.555532270803653
4.555532270803653
```

- with average damping, the procedure only takes 10 steps:

```scheme
> (fixed-point-verbose f-damp 2)
5.9828921423310435
4.922168721308343
...
4.555537551999825
4.555537551999825
```

---
### Exercise 1.37

Solution:

- recursive process:
```scheme
(define (cont-frac n d k)
  (define (i-term i)
    (if (= i k)
      0
      (/ (n i)
         (+ (d i) (i-term (+ i 1))))))
  (i-term 1))
```

_k_ must be at least 12 to get the inverse of $\varphi$ accurate to 4 decimal places:

```scheme
> (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             12)
0.6180555555555556
```

- iterative process:
```scheme
(define (cont-frac n d k)
  (define (iter i result)
    (if (zero? i)
      result
      (iter (- i 1) (/ (n i)
                       (+ (d i) result)))))
  (iter k 0))
```

The iterative process requires _k_ to be 11 to get an approximation that is accurate to 4 decimal places:

```scheme
> (cont-frac (lambda (i) 1.0)
             (lambda (i) 1.0)
             11)
0.6180555555555556
```

---
### Exercise 1.38

Solution:

```scheme
(define (approx-e k)
  (define (n i) 1.0)
  (define (d i)
    (if (= (remainder i 3) 2)
        (- (+ i 1) (/ (+ i 1) 3))
        1))
  (+ (cont-frac n d k) 2))
```

Test:
```scheme
> (approx-e 10)
2.7182817182817183
```

---
### Exercise 1.39

Solution:

```scheme
(define (tan-cf x k)
  (define (n i)
    (if (= i 1) x (- (* x x))))
  (define (d i)
    (- (* i 2) 1))
  (cont-frac n d k))
```

Tests:

```scheme
> (tan-cf (atan 1) 5)
0.999999986526355
> (tan (atan 1))
0.9999999999999999 ; should be 1
```

---
### Exercise 1.40

```scheme
(define (cube x) (* x x x))
(define (square x) (* x x))
```

Solution:

```scheme
(define (cubic a b c)
  (lambda (x)
    (+ (cube x)
       (* a (square x))
       (* b x)
       c)))
```

An easy way to test this out is to apply the `cubic` procedure to the returned value of `newtons-method` to see if it returns (something close to) 0:

```scheme
> (define f (cubic 1 2 3))
> (f (newtons-method f 1.0))
4.935607478273596e-12
```

Due to the `fixed-point` procedure operating under a certain tolerance level, it does not return exactly 0, but it comes rather close.

---
### Exercise 1.41

Solution:
```scheme
(define (double f)
  (lambda (x) (f (f x))))
```

Test:
```scheme
> (((double (double double)) inc) 5)
21
```

---
### Exercise 1.42

Solution:
```scheme
(define (compose f g)
  (lambda (x) (f (g x))))
```

Test checks out:
```scheme
> ((compose square inc) 6)
49
```

---
### Exercise 1.43

Solution:
```scheme
(define (repeated f n)
  (if (= n 1)
      f
      (compose f (repeated f (- n 1)))))
```

Test:
```scheme
> ((repeated square 2) 5)
625
```

---
### Exercise 1.44

Solution:\
I am going to assume that _dx_ is some variable pre-defined within the environment, rather than assume it is to be passing into the procedure as an argument

```scheme
(define (smooth f)
  (lambda (x)
    (/ (+ (f x)
          (f (- x dx))
          (f (+ x dx)))
        3)))
```

To generate the _n_-fold smoothed function:

```
(((repeated smooth n) f) x)
```

Where _f_ is the function we wish to smooth, _n_ is the number of times we wish to repeatedly smooth _f_, and _x_ is the actual argument we pass to be calculated by _f_.

---
### Exercise 1.45

Solution:\
We must average damp $log_2 n$ times, rounding down.

```scheme
(define (nth-root x n)
  (fixed-point
    ((repeated average-damp (floor (/ (log n) (log 2))))
     (lambda (y) (/ x (expt y (- n 1)))))
  1.0))
```

Testing this out:
```scheme
> (nth-root 4 2)
2.000000000000002
> (nth-root 256 4)
4.000000000000006
> (nth-root 6561 8)
3.0000000000173292
```
