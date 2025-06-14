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
(h n) computes a weird function that is $2^(2^(2^\ldots))$ up to the nth level. `(A 2 n)` evaluates to `(A 1 (A 2 (- n 1)))`; thus the composition is `(g (h (- n 1)))`.
