# [1.2 - Procedures and the Processes They Generate](https://sarabander.github.io/sicp/html/1_002e2.xhtml#g_t1_002e2)

### Exercise 1.9
```scheme
(define (+ a b)
  (if (= a 0) 
      b 
      (inc (+ (dec a) b))))

(define (+ a b)
  (if (= a 0) 
      b 
      (+ (dec a) (inc b))))
```

Solution:\
To determine which procedure produces a recursive process and which an iterative one, we can use the substitution model. I will simplify until we get to the recursive call (I will not show the evaluation of `inc` and `dec`)

The first procedure looks like this:
```scheme
(+ 4 5)
(inc (+ 3 5))
(inc (inc (+ 2 5)))
(inc (inc (inc (+ 1 5))))
(inc (inc (inc (inc (+ 0 5)))))
(inc (inc (inc (inc 5))))
(inc (inc (inc 6)))
(inc (inc 7))
(inc 8)
9
```
We can see that there is a period of expanding, up to 4 deferred operations, followed by a period of contracting. This is a linear recursive process.

The second procedure looks like this:
```scheme
(+ 4 5)
(+ 3 6)
(+ 2 7)
(+ 1 8)
(+ 0 9)
9
```
As we can see, there are no deferred operations, everything is performed and we are always able to tell the current state of the operation. This is a linear iterative process.

---
### Exercise 1.10
```scheme
(define (A x y)
  (cond ((= y 0) 0)
        ((= x 0) (* 2 y))
        ((= y 1) 2)
        (else (A (- x 1)
                 (A x (- y 1))))))
```

Solution:
```scheme
> (A 1 10)
1024
> (A 2 4)
65536
> (A 3 3)
65536
```

Given these procedure definitions, what are their respective mathematical notations?
```
(define (f n) (A 0 n))
(define (g n) (A 1 n))
(define (h n) (A 2 n))
(define (k n) (* 5 n n))
```

(f n) computes $2n$, according to the 2nd conditional clause.\
(g n) computes $2^n$. We can see that with `(A 1 10)`, which would evaluate to `(A 0 (A 1 9))`. This is basically the composition of the _f_ and _g_ functions: `(f (g (- n 1)))`, thus $2 * 2 * 2 \ldots$ or $2^n$.\
(h n) computes a weird function that is $2^{2^{2^{\ldots}}}$ up to the nth level. `(A 2 n)` evaluates to `(A 1 (A 2 (- n 1)))`; thus the composition is `(g (h (- n 1)))`.

---
### Exercise 1.11
Solution:\
For the recursive process:
```scheme
(define (f n)
  (if (< n 3)
      n
      (+ (f (- n 1)) (* 2 (f (- n 2))) (* 3 (f (- n 3))))))
```
As is this case for tree recursion, it is a rather straightforward implementation for a recursive process.

Making it an iterative process requires a bit more work:
```scheme
(define (f n)
  (define (iter x y z count)
    (if (= n count)
        x
        (iter y z (+ z (* 2 y) (* 3 x)) (+ count 1))))
  (iter 0 1 2 0))
```
We always need to be aware of the three previous calculations, with the base case being $f(0) = 0$, $f(1) = 1$, and $f(2) = 2$. We are then constantly shifting these answers down until `count` is equal to `n`, and by then the answer for $f(n)$ has shifted to the first parameter in the procedure.

---
### Exercise 1.12
Solution:
```scheme
(define (pascal i j)
  (cond ((zero? j) 1)
        ((eq? i j) 1)
        (else (+ (pascal (- i 1) (- j 1))
                 (pascal (- i 1) j)))))
```

Testing this out:
```scheme
> (pascal 3 0)
1
> (pascal 3 1)
3
> (pascal 3 2)
3
> (pascal 3 3)
1
```

---
### Exercise 1.13
Solution:\
Proofs are one of my weaknesses in math, but here goes nothin:

First, we'll identify the givens, which are the constants $\varphi$ and $\psi$ which satisy the golden ratio of $x^2 = x + 1$.

$$\varphi = \frac{1 + \sqrt{5}}{2},\quad \psi = \frac{1 - \sqrt{5}}{2}$$

Also, the recursive definition of the Fibonacci sequence is given as:

$$
\begin{aligned}
Fib(0) &= 0, \\
Fib(1) &= 1, \\
Fib(n) &= Fib(n-1) + Fib(n-2).
\end{aligned}
$$

To begin, we'll do as the hint says and show that $Fib(n) = \dfrac{\varphi^n - \psi^n}{\sqrt{5}}$

We'll start with the base cases of when $n=0$ and $n=1$, respectively:

$$Fib(0) = \frac{\varphi^0 - \psi^0}{\sqrt{5}} = \frac{1-1}{\sqrt{5}} = 0.$$

$$Fib(1) = \frac{\varphi - \psi}{\sqrt{5}}
= \frac{\frac{1 + \sqrt{5} - 1 + \sqrt{5}}{2}}{\sqrt{5}}
= \frac{2\sqrt{5}}{2\sqrt{5}} = 1.$$

Now for the induction phase:

$$
\begin{aligned}
Fib(n) &= Fib(n-1) + Fib(n-2) \\
&= \frac{\varphi^{n-1} - \psi^{n-1}}{\sqrt{5}} + \frac{\varphi^{n-2} - \psi^{n-2}}{\sqrt{5}} \\
&= \frac{\left(\varphi^{n-1} + \varphi^{n-2}\right) - 
         \left(\psi^{n-1} + \psi^{n-2}\right)}{\sqrt{5}} \\
&= \frac{\varphi^n \left(\varphi^{-1} + \varphi^{-2}\right) - 
         \psi^n \left(\psi^{-1} + \psi^{-2}\right)}{\sqrt{5}} \\
&= \frac{\varphi^n\varphi^{-1}\left(1 + \varphi^{-1}\right) - 
         \psi^n\psi^{-1}\left(1 + \psi^{-1}\right)}{\sqrt{5}} \\
&= \frac{\varphi^n\varphi^{-1}\left(\varphi\right) - 
         \psi^n\psi^{-1}\left(\psi\right)}{\sqrt{5}} \\
Fib(n) &= \frac{\varphi^n - \psi^n}{\sqrt{5}}
\end{aligned}
$$

Thus, by induction, we have proved that $Fib(n) = \dfrac{\varphi^n - \psi^n}{\sqrt{5}}$. Now, to show that $Fib(n)$ is the closest integer to $\varphi^n / \sqrt{5}$, it is enough to know that $\psi < 1$. Meaning that as _n_ approaches infinity, $\psi^n$ approaches 0. Thus leaving us with:

$$Fib(n) = \dfrac{\varphi^n - 0}{\sqrt{5}} = \dfrac{\varphi^n}{\sqrt{5}}.$$

---
### Exercise 1.14
```scheme
(define (count-change amount)
  (cc amount 5))

(define (cc amount kinds-of-coins)
  (cond ((= amount 0) 1)
        ((or (< amount 0) 
             (= kinds-of-coins 0)) 
         0)
        (else 
         (+ (cc amount (- kinds-of-coins 1))
            (cc (- amount (first-denomination 
                           kinds-of-coins))
                kinds-of-coins)))))

(define (first-denomination kinds-of-coins)
  (cond ((= kinds-of-coins 1) 1)
        ((= kinds-of-coins 2) 5)
        ((= kinds-of-coins 3) 10)
        ((= kinds-of-coins 4) 25)
        ((= kinds-of-coins 5) 50)))
```

Solution:\
Using the `count-change` procedure to calculate the amount of ways we can make change out of 11 cents produces a tree somewhat like this:
```scheme
(cc 11 5)
  (cc -39 5) ; 0
  (cc 11 4)
    (cc -14 4) ; 0
    (cc 11 3)
      (cc 1 3)
        (cc -9 3) ; 0
        (cc 1 2)
          (cc -4 2) ; 0
          (cc 1 1)
            (cc 0 1) ; 1
            (cc 1 0) ; 0
      (cc 11 2)
        (cc 6 2)
          (cc 1 2)
            (cc -4 2) ; 0
            (cc 1 1)
              (cc 0 1) ; 1
              (cc 1 0) ; 0
          (cc 6 1)
            (cc 5 1)
              (cc 4 1)
                (cc 3 1)
                  (cc 2 1)
                    (cc 1 1)
                      (cc 0 1) ; 1
                      (cc 1 0) ; 0
                    (cc 2 0) ; 0
                  (cc 3 0) ; 0
                (cc 4 0) ; 0
              (cc 5 0) ; 0
            (cc 6 0) ; 0
        (cc 11 1)
          (cc 10 1)
            (cc 9 1)
              (cc 8 1)
                (cc 7 1)
                  (cc 6 1)
                    (cc 5 1)
                      (cc 4 1)
                        (cc 3 1)
                          (cc 2 1)
                            (cc 1 1)
                              (cc 0 1) ; 1
                              (cc 1 0) ; 0
                            (cc 2 0) ; 0
                          (cc 3 0) ; 0
                        (cc 4 0) ; 0
                      (cc 5 0) ; 0
                    (cc 6 0) ; 0
                  (cc 7 0) ; 0
                (cc 8 0) ; 0
              (cc 9 0) ; 0
            (cc 10 0) ; 0
          (cc 11 0) ; 0
```

For tree recursion, the order of growth for space is equal to the depth of the tree, which in this case is $\Theta(n)$, since the depth grows linearly with the input. This is evident in the graph where the tree goes down 11 levels.

The order of growth for the number of steps (or the number of leaves on the tree) I believe is $\Theta(n^d)$, where _d_ is the number of coin denominations we use. Since we are going with 5 coin denominations, the order of growth is $\Theta(n^5)$.

---
### Exercise 1.15

```scheme
(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine angle)
   (if (not (> (abs angle) 0.1))
       angle
       (p (sine (/ angle 3.0)))))
```

Solution:\
1. The procedure p is applied five times:
```scheme
(sine 12.15)
(p (sine 4.05))
(p (p (sine 1.35)))
(p (p (p (sine 0.45))))
(p (p (p (p (sine 0.15)))))
(p (p (p (p (p (sine 0.05))))))
```

2. The order of growth for space and number of steps will be the same, since the depth of the stack for the interpreter (space) is dependent on the number of deferred operations in the procedure (number of steps). Both of these, then, are based on the condition for which $a < 0.1$. Thus we get the inequality $a\3^k < 0.1$, where _k_ is the number of times we must perform this operation until the inequality evaluates to true.

Solving for _k_, we get $k > \log 10a / \log 3$. Thus the order of growth is $\Theta(log n)$.

---
### Exercise 1.16

Solution:\
Using the previously define `square` function:

```scheme
(define (expt-iter b n)
  (define (iter a b n)
    (cond ((= n 0) a)
          ((even? n) (iter a (square b) (/ n 2)))
          (else (iter (* a b) b (- n 1)))))
  (iter 1 b n))
```

Testing this out:
```scheme
> (expt-iter 3 7)
2187
> (expt-iter 5 4)
625
> (expt-iter 2 5)
32
```

---
### Exercise 1.17

These are given as primitives:
```scheme
(define (double x) (+ x x))
(define (halve x) (/ x 2))
```

Solution:
```scheme
(define (mult a b)
  (cond ((or (zero? a) (zero? b)) 0)
        ((even? b) (double (mult a (halve b))))
        (else (+ a (mult a (- b 1))))))
```

Testing this out:
```scheme
> (mult 3 4)
12
> (mult 8 7)
56
> (mult 10 5)
50
```

---
### Exercise 1.18

Solution:
```scheme
(define (mult-iter a b)
  (define (iter x a b)
    (cond ((zero? b) x)
          ((even? b) (iter x (double a) (halve b)))
          (else (iter (+ x a) a (- b 1)))))
  (iter 0 a b))
```

Testing this out:
```scheme
> (mult-iter 3 4)
12
> (mult-iter 10 6)
60
> (mult-iter 5 7)
35
```

---
### Exercise 1.19

Solution:\

1. Given $T_{pq}(a,b) = (bq + aq + ap, bp + aq)$, we must apply the transformation twice to give us a way for computer $T^n$:

$$
\begin{aligned}
T_{pq}(T_{pq}(a,b)) &= T_{pq}(bq + aq + ap, bp + aq) \\
&= ((bp + aq)q + (bq + aq + ap)q + (bq + aq + ap)p, \\
&   (bp + aq)p + (bq + aq + ap)q) \\
&= (bpq + aq^2 + bq^2 + aq^2 + apq + bpq + apq + ap^2, \\
&   bp^2 + apq + bq^2 + aq^2 + apq) \\
&= (bq^2 + 2bpq + aq^2 + 2apq + aq^2 + ap^2, \\
&   bq^2 + bp^2 + aq^2 + 2apq) \\
&= (b(q^2 + 2pq) + a(q^2 + 2pq) + a(q^2 + p^2), \\
&   b(q^2 + p^2) + a(q^2 + 2pq)) \\
&= T_{p'q'}(a,b)
\end{aligned}
$$

From this, we see that $p' = q^2 + p^2$ and $q' = q^2 + 2pq$.

2. The second part of this question is essentially just plugging our findings into the procedure definition:

```scheme
(define (fib n)
  (fib-iter 1 0 0 1 n))

(define (fib-iter a b p q count)
  (cond ((= count 0) b)
        ((even? count)
         (fib-iter a 
                   b 
                   (+ (* q q) (* p p)) 
                   (+ (* q q) (* 2 p q)) 
                   (/ count 2)))
        (else (iter (+ (* b q) (* a q) (* a p))
                    (+ (* b p) (* a q))
                    p
                    q
                    (- count 1)))))
```

Which gives us a $\Theta(\log n)$ time complexity.

---
### Exercise 1.20

```scheme
(define (gcd a b)
  (if (= b 0)
      a
      (gcd b (remainder a b))))
```

Solution:\
- For applicative order, we get 4 remainder operations:
```scheme
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd 40 6)
(gcd 6 (remainder 40 6))
(gcd 6 4)
(gcd 4 (remainder 6 4))
(gcd 4 2)
(gcd 2 (remainder 4 2))
(gcd 2 0)
2
```

- For normal order, we evaluate `remainder` 18 times. Fourteen times from evaluating `(= b 0)`, and four times at the end when solving for `a`:
```scheme
(gcd 206 40)
(gcd 40 (remainder 206 40))
(gcd (remainder 206 40) (remainder 40 (remainder 206 40)))
(gcd (remainder 40 (remainder 206 40))
     (remainder (remainder 206 40) (remainder 40 (remainder 206 40))))
(gcd (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))
     (remainder (remainder 40 (remainder 206 40)) 
                (remainder (remainder 206 40) (remainder 40 (remainder 206 40)))))
(remainder (remainder 206 40) 
           (remainder 40 (remainder 206 40)))
2
```

---
### Exercise 1.21

Solution:\
Using methods from [`divisors.rkt`](./divisors.rkt):

```scheme
> (smallest-divisor 199)
199
> (smallest-divisor 1999)
1999
> (smallest-divisor 19999)
7
```

---
### Exercise 1.22

I used the following code to create my [`primetime.rkt`](./primetime.rkt) module:

```scheme
(define (timed-prime-test n)
  (newline)
  (display n)
  (start-prime-test n (runtime)))

(define (start-prime-test n start-time)
  (if (prime? n)
      (report-prime (- (runtime) 
                       start-time))))

(define (report-prime elapsed-time)
  (display " *** ")
  (display elapsed-time))
```

Solution:\
In [`primesearch.rkt`](./primesearch.rkt):

```scheme
(define (search-for-primes a b)
  (define (iter a b)
    (when (<= a b)
      (timed-prime-test a)
      (iter (+ a 2) b)))
  (iter (if (odd? a) a (+ a 1)) b))
```

Testing this out for first three primes that are...
- greater than 1,000:

```
1009 *** 1
1013 *** 1
1019 *** 1
```

- greater than 10,000:

```
10007 *** 2
10009 *** 1
10037 *** 1
```

- greater than 100,000:

```
100003 *** 4
100019 *** 4
100043 *** 4
```

- greater than 1,000,000:

```
1000003 *** 13
1000033 *** 12
1000037 *** 11
```

For the most part, the data checks out for the $\Theta(\sqrt{n})$ growth prediction, where the elapsed time roughly grows by a factor of $\sqrt{10} \approx 3.16$ as the _n_ grows by powers of 10. I believe this does not reflect in the change between 1,000 and 10,000 because the `runtime` command seems to round up.

It is safe to assume that programs on my machine run in time proportional to the number of steps required for the computation.

---
### Exercise 1.23

Solution:

```scheme
(define (next n)
  (if (= n 2)
    3
    (+ n 2)))
```

Substituting this into the `find-divisor` procedure looks like this:

```scheme
(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (next test-divisor)))))
```

Now for testing these with the 12 primes we found:
- Greater than 1,000:

```
1009 *** 1
1013 *** 1
1019 *** 1
```

- Greater than 10,000:

```
10007 *** 1
10009 *** 2
10037 *** 2
```

` Greater than 100,000:

```
100003 *** 4
100019 *** 3
100043 *** 3
```

- Greater than 1,000,000:

```
1000003 *** 8
1000033 *** 8
1000037 *** 9
```

The results do not exactly match up the expectations, though using this `next` procedure does seem to make the search go by a little faster. In the case for finding primes greater than 1 million, it seems to take about 75% of the time. I believe a lot of this fault may stem from the fact that the `(runtime)` procedure has a very low precision, and also rounds up to the nearest integer. This makes comparing results a little difficult.

As for why these results may not match our hypothesis that the search will take half the amount of time, this could be for several reasons. One is due other processes and applications running on the pc, taking up some precious resources. Another could be that the overhead of calling the `next` function, rather than just dealing with primitives (that are most likely optimized in the machine code), could cancel out the benefits that we expected.

---
### Exercise 1.24

```scheme
(define (expmod base exp m)
  (cond ((= exp 0) 1)
        ((even? exp)
         (remainder (square (expmod base (/ exp 2) m))
                    m))
        (else (remainder (* base (expmod base (- exp 1) m))
                         m))))

(define (fermat-test n)
  (define (try-it a)
    (= (expmod a n n) a))
  (try-it (+ 1 (random (- n 1)))))

(define (fast-prime? n times)
  (or (= times 0)
      (and (fermat-test n)
           (fast-prime? n (- times 1)))))
```

Solution:\

Using the `fast-prime?` predicate instead of the `prime?` predicate means defining `prime?` as:

```scheme
(define (prime? n)
  (fast-prime? n 100))
```

(Rather than changing procedure definitions to test this out, a better way would be to pass in the predicate procedure as an argument to `search-for-primes`. That way we can switch between the two predicates - `prime?` and `fast-prime?` - as we like. However, at this point in the book, using procedures as arguments has not yet been discussed, so I'll just stick to changing the procedure definition.)

These are the results for using this procedure:
- greater than 1,000:

```
1009 *** 71
1013 *** 74
1019 *** 76
```

- greater than 10,000:

```
10007 *** 93
10009 *** 91
10037 *** 89
```

- greater than 100,000:

```
100003 *** 92
100019 *** 94
100043 *** 94
```

- greater than 1,000,000:

```
1000003 *** 125
1000033 *** 121
1000037 *** 135
```

Using this procedure did not yield the results I was looking for at all! This made me lose even more faith in the `(runtime)` procedure provided by the sicp language module. My biggest problem is the lack of precision, as well as not providing units for its measurements. What exactly is the time? Milliseconds? Microseconds? Perhaps at a later date I will redo these tests using a different method for measuring runtime.

---
### Exercise 1.25

```scheme
(define (new-expmod base exp m)
  (remainder (fast-expt base exp) m))
```

Solution:\

This method does indeed work and produces the same result as our previous `expmod` procedure. Here are some tests that show this:

```scheme
> (expmod 5 9 9)
8
> (new-expmod 5 9 9)
8
> (expmod 4 7 7)
4
> (new-expmod 4 7 7)
4
> (expmod 5 11 11)
5
> (new-expmod 5 11 11)
5
```

The problem with this, however, is that it waits to take the modulo of the number _after_ the exponential has already been computed, whereas the previous method would take the modulo in between recursive calls. This means that the new procedure would have to deal with much bigger numbers than the previous procedure, which could pose a problem when working with big exponentials.

---
### Exercise 1.26

Solution:\
The difference between using the square procedure and explicitly using the multiplication operator is that in the former, `square` is called only after the `expmod` procedure has run its course and returned its value, whereas the latter calculates the `expmod` solution twice. This is just how the interpreter evaluates its processes, as it does not know that these are two of the exact same procedure calls. It has now become a tree-recursive process with a time complexity of $\Theta(n)$

---
### Exercise 1.27

Solution:

```scheme
(define (fermat-check n)
  (define (test a)
    (or (= a 0)
        (and (= (expmod a n n) a)
             (test (- a 1)))))
  (test (- n 1)))
```

First, we'll check to see if this works for some known prime numbers and known non-prime numbers:

```scheme
> (fermat-check 13)
#t
> (fermat-check 19)
#t
> (fermat-check 31)
#t
> (fermat-check 21)
#f
> (fermat-check 35)
#f
> (fermat-check 50)
#f
```

Now for the Carmichael numbers:
```scheme
> (fermat-check 561)
#t
> (fermat-check 1105)
#t
> (fermat-check 1729)
#t
> (fermat-check 2465)
#t
> (fermat-check 2821)
#t
> (fermat-check 6601)
#t
```
