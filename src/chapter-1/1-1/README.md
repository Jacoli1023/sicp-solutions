# [1.1 - The Elements of Programming](https://sarabander.github.io/sicp/html/1_002e1.xhtml#g_t1_002e1)

### Exercise 1.1
Solution:\
This is simply putting each expression into the REPL and seeing what it evaluates as, so the format follows what it looks like when putting the expressions in the Racket CLI.

```scheme
> 10
10
> (+ 5 3 4)
12
> (- 9 1)
8
> (/ 6 2)
3
> (+ (* 2 4) (- 4 6))
6
> (define a 3)
> (define b (+ a 1))
> (+ a b (* a b))
19
> (= a b)
#f
> (if (and (> b a) (< b (* a b)))
      b
      a)
4
> (cond ((= a 4) 6)
        ((= b 4) (+ 6 7 a))
        (else 25))
16
> (+ 2 (if (> b a) b a))
6
> (* (cond ((> a b) a)
           ((< a b) b)
           (else -1))
     (+ a 1))
16
```

---
### Exercise 1.2
Solution:
```scheme
> (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7)))
-37/150
```

---
### Exercise 1.3
Solution:
```scheme
(define (square x) (* x x))

(define (func a b c)
  (cond ((and (<= a b) (<= a c))
         (+ (square b) (square c)))
        ((and (<= b a) (<= b c))
         (+ (square a) (square c)))
        (else
         (+ (square a) (square b)))))
```

Testing this out:
```scheme
> (func 1 2 3)
13
> (func 4 5 6)
61
```

Certain parts of this solution violate the DRY principle, but the book will inevitably address this and we'd be able to learn further abstraction techniques.

---
### Exercise 1.4
```scheme
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
```

Solution:\
This is essentially a function that takes in two arguments, _a_ and _b_, and then provides a conditional check on _b_ such that if _b_ is greater than 0 (is positive), then it returns the + operator, otherwise it returns the - operator. It then uses this operator on the arguments _a_ and _b_.

This function, in effect, does what its name alludes to: it returns $a + \|b\|$, since subtracting a negative number is equivalent to adding a positive number.

---
### Exercise 1.5
```scheme
(define (p) (p))
(define (test x y)
  (if (= x 0) 0 y))

(test 0 (p))
```

Solution:\
We know that in applicative-order evaluation the subexpressions are evaluated first, then the procedure is applied to the arguments. This follows the substitution model. Thus, when we run the `test` procedure with 0 and the `(p)` function as its arguments, we never get a value returned because the interpreter will constantly try to evaluate the `(p)` function, which only returns itself. Thus spawning infinite recursion.

However, in normal-order evaluation, the arguments aren't evaluated until they are needed. So in the same scenario, the interpreter will go through the `test` function call with its arguments unevaluated, will perform the conditional check which returns `#t`, and will promptly return 0 as its final value, with `(p)` never being evaluated.

---
### Exercise 1.6
```scheme
(define (new-if predicate then-clause else-clause)
  (cond (predicate then-clause)
        (else else-clause)))

(define (sqrt-iter guess x)
  (new-if (good-enough? guess x)
          guess
          (sqrt-iter (improve guess x) x)))
```

Solution:\
It is important to remember that `if` is a special operator that does not follow applicative-order evaluation. Thus, when we create `new-if`, this new operator follows applicative-order evaluation. Which means that, when we pass in the the predicate and two clauses, the scheme interpreter will try to evaluate the subexpressions before applying the `new-if` procedure. Meaning `sqrt-iter` will try to be evaluated, but will never be able to because it relies on the `new-if` function which only evaluates applicatively. Thus, infinite recursion happens.

---
### Exercise 1.7

Solution:\
When the numbers are small enough, then they will always pass the `good-enough?` check, even if the `guess` and the `x` value differ by orders of magnitude. Conversely, when the numbers are big enough, they will never pass `good-enough?`, because the fixed-tolerance of the predicate cannot represent the small changes between really big numbers.

As the question explains, a better solution to having a fixed-precision-tolerance is to measure the differences between the iterative guesses. When the change is _relatively_ small, rather than based on some arbitrary _absolute_ difference, then we can determine better if the `guess` is good enough. See the modules in [`sqrt.rkt`](./sqrt.rkt) for my solution on this.

---
### Exercise 1.8

Solution:\
Provided in [`cbrt.rkt`](./cbrt.rkt). Testing it out yields these results:

```scheme
> (cbrt 8)
2.000004911675504
> (cbrt 27)
3.0000005410641766
```
