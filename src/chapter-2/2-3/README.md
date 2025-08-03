# [2.3 Symbolic Data](https://sarabander.github.io/sicp/html/2_002e3.xhtml#g_t2_002e3)

### Exercise 2.53

Solution:
```scheme
> (list 'a 'b 'c)
'(a b c)

> (list (list 'george))
'((george))

> (cdr '((x1 x2) (y1 y2)))
'((y1 y2))

> (cadr '((x1 x2) (y1 y2)))
'(y1 y2)

> (pair? (car '(a short list)))
#f

> (memq 'red '((red shoes) (blue socks)))
#f

> (memq 'red '(red shoes blue socks))
'(red shoes blue socks)
```

---
### Exercise 2.54

Solution:

Here's my slightly more complicated solution. Probably would've been easier to read using a `cond` statement but oh well.
```scheme
(define (equal? a b)
  (or (and (symbol? a) (symbol? b)
           (eq? a b))
      (and (list? a) (list? b)
           (and (equal? (car a) (car b))
                (equal? (cdr a) (cdr b))))))
```

Test:
```scheme
> (equal? '(this is a list)
          '(this is a list))
#t
> (equal? '(this is a list)
          '(this (is a) list))
#f
```

---
### Exercise 2.55

```scheme
> (car ''abracadabra)
'quote
```

Solution:

The answer to this question is alluded to by footnote 100 of the book. It states that our use of the quote symbol (`'`) "violates the general rule that all compound expressions in our language should be delimited by parentheses and look like lists". Thus, what the interpreter _actually_ does when it receives the quote symbol is substitute in the form `(quote <expression>)`. 

Thus when we pass in the expression above, and since Scheme works in applicative order, it will first evaluate the subexpressions before calling `car`. This means the actual argument that we pass in is expanded to `(quote (quote abracadabra))`. Only the first `quote` actually gets evaluated, and calling the `car` of this expression then simply returns `quote`.

---
### Exercise 2.56

$\frac{d(u^n)}{dx} = nu^{n - 1} * \frac{du}{dx}$

Solution:

- exponentiation representation ([`alg-rep.rkt`](./alg-rep.rkt)):
```scheme
(define (exponentiation? expr)
  (and (pair? expr) (eq? (car expr) '**)))
(define (base s) (cadr s))
(define (exponent s) (caddr s))

(define (make-exponentiation b e)
  (cond ((=number? e 0) 1)
        ((=number? e 1) b)
        ((=number? b 1) 1)
        (else (list '** b e))))
```

- differentiation rules ([`sym-diff.rkt`](./sym-diff.rkt)):
```scheme
(define (deriv expr var)
  (cond ((number? expr) 0)
        ((variable? expr)
         (if (same-variable? expr var) 1 0))
        ((sum? expr)
         (make-sum (deriv (addend expr) var)
                   (deriv (augend expr) var)))
        ((product? expr)
         (make-sum (make-product (multiplier expr)
                                 (deriv (multiplicand expr) var))
                   (make-product (deriv (multiplier expr) var)
                                 (multiplicand expr))))
        ((exponentiation? expr)
         (make-product 
          (exponent expr)
          (make-product (make-exponentiation (base expr)
                                             (- (exponent expr) 1))
                        (deriv (base expr) var))))
        (else (error 'deriv "unknown expr type" expr))))
```

Testing it out:
```scheme
> (deriv '(* 4 (** x 2)) 'x)
'(* 4 (* 2 x))
```

As we can see, the rules we have set up for simplifying the results of the returned expression are not quite in simplest terms. Otherwise, this would have returned `(* 8 x)`, but we'll get to that.

---
### Exercise 2.57

Solution:

This is rather simple, as our `multiplier` and `addend` selectors stay the exact same; we just need to change the `multiplicand` and the `augend` selectors.
```scheme
(define (augend s) 
  (accumulate make-sum 0 (cddr s)))

(define (multiplicand s) 
  (accumulate make-product 1 (cddr s)))

```

We must be sure to use `make-sum` and `make-product` as our accumulation procedure, and not the primitives `+` and `*`, as our user-defined procedures know how to handle creating sums of numbers as well as dealing with symbols.

---
### Exercise 2.58

Solution:

- fully parenthesized:
```scheme
(define (make-sum a1 a2) 
  (cond ((=number? a1 0) a2)
        ((=number? a2 0) a1)
        ((and (number? a1) (number? a2))
         (+ a1 a2))
        (else (list a1 '+ a2))))

(define (make-product m1 m2) 
  (cond ((or (=number? m1 0)
             (=number? m2 0))
         0)
        ((=number? m1 1) m2)
        ((=number? m2 1) m1)
        ((and (number? m1) (number? m2))
              (* m1 m2))
        (else (list m1 '* m2))))

(define (sum? expr)
  (and (pair? expr) (eq? (cadr expr) '+)))
(define (addend s) (car s))
(define (augend s) (caddr s))

(define (product? expr)
  (and (pair? expr) (eq? (cadr expr) '*)))
(define (multiplier s) (car s))
(define (multiplicand s) (caddr s))
```

Test:
```scheme
> (deriv '(x + (3 * (x + (y + 2)))) 'x)
4
```

- standard simplified algebraic notation:
```scheme
(define (has? op expr)
  (and (pair? expr) (memq op expr)))

(define (unwrap expr)
  (if (and (pair? expr) (null? (cdr expr)))
      (car expr)
      expr))

(define (after op ls)
  (unwrap (cdr (memq op ls))))

(define (before op expr)
  (define (helper ls)
    (if (eq? op (car ls))
        '()
        (cons (car ls) (helper (cdr ls)))))
  (unwrap (helper expr)))

(define (sum? expr) (has? '+ expr))
(define (addend s) (before '+ s))
(define (augend s) (after '+ s))

(define (product? expr) (and (not (sum? expr)) (has? '* expr)))
(define (multiplier s) (before '* s))
(define (multiplicand s) (after '* s))

(define (exponentiation? expr) (and (not (product? expr)) (has? '** expr)))
(define (base s) (before '** s))
(define (exponent s) (after '** s))
```

This one was rather hard for me, as after creating my `before`, `after`, and predicate functions, I would receive a string format error. I figured it was the way that our `deriv` function would read in the list, and could not further derive an expression after parsing the elements before and after the operator. This is probably because my previous attempts did not have the `unwrap` procedure, and as such my `before` and `after` procedures would return each element as its own list - such as '(x) instead of 'x.

Thankfully, a solution was found [here](https://mk12.github.io/sicp/exercise/2/3.html#ex2.58) in the form of the `unwrap` procedure, which magically solved all my problems in life.

Test:
```scheme
> (deriv '(x + 3 * (x + y + 2)) 'x)
4
```
