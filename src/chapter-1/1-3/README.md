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
