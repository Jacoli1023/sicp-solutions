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
