# [1.1 - The Elements of Programming](https://sarabander.github.io/sicp/html/1_002e1.xhtml#g_t1_002e1)

### Exercise 1.1
**Below is a sequence of expressions. What is the result printed by the interpreter in response to each expression? Assume that the sequence is to be evaluated in the order in which it is presented.**

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

### Exercise 1.2
**Translate the following expression into prefix form:**

Solution:
```scheme
> (/ (+ 5 4 (- 2 (- 3 (+ 6 4/5))))
     (* 3 (- 6 2) (- 2 7)))
-37/150
```

### Exercise 1.3
**Define a procedure that takes three numbers as arguments and returns the sum of the squares of the two larger numbers.**

Solution:\
I will also be defining and using the `square` function, which was explained earlier in the chapter.

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

### Exercise 1.4
**Observe that our model of evaluation allows for combinations whose operators are compound expressions. Use this observation to describe the behavior of the following procedure:**

```scheme
(define (a-plus-abs-b a b)
  ((if (> b 0) + -) a b))
```

Solution:\
This is essentially a function that takes in two arguments, _a_ and _b_, and then provides a conditional check on _b_ such that if _b_ is greater than 0 (is positive), then it returns the + operator, otherwise it returns the - operator. It then uses this operator on the arguments _a_ and _b_.

This function, in effect, does what its name alludes to: it returns $a + |b|$, since subtracting a negative number is equivalent to adding a positive number.
