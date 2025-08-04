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

---
### Exercise 2.59

Solution ([`unordered-set.rkt`](./unordered-set.rkt)):
```scheme
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        ((element-of-set? (car set1) set2)
         (union-set (cdr set1) set2))
        (else (cons (car set1)
                    (union-set (cdr set1) set2)))))
```

Test:
```scheme
> (union-set '(1 2 3 4) '(4 5 6))
'(1 2 3 4 5 6)
> (union-set '(1 2 3) '())
'(1 2 3)
> (union-set '() '(1 2 3))
'(1 2 3)
```

---
### Exercise 2.60

Solution:

Our procedures `element-of-set?` and `intersection-set` work the same whether the list allows duplicates or not; we only need to change `adjoin-set` and `union-set`, as these are the procedures which would normally handle duplicates.

```scheme
(define (adjoin-set x set)
  (cons x set))

(define (union-set set1 set2)
  (append set1 set2))
```

As far as efficiency goes, `adjoin-set` runs in constant time ($\theta(1)$) now, since it always performs just a single operation. `union-set` will now run in $\theta(n)$ time instead of $\theta(n^2)$ time, since it simply appends each element of one set to the other, without checking if that element is already apart of the other set.

`element-of-set?` and `intersection-set` still run in basically the same time, though with the possibility of duplicates, there is now a _duplicate scalar factor k_ introduced into the runtime. Thus `element-of-set?` will run in $\theta(kn)$ time, and `intersection-set` will run in $\theta(kn^2)$ time.

As for which representation is more efficient, it depends on the duplicate factor, as well as the size of the set. If _k_ is a very high number, meaning there are potentially lots of duplicates, then it might be wiser to use the non-duplicate representation. Otherwise, the duplicate set representation would be more efficient, especially if you'll be using the `adjoin-set` and `union-set` procedures frequently.

And of course, you'd always use the duplicate representation when it's required to do so, such as when measuring the frequency of certain outcomes, etc.

---
### Exercise 2.61

Solution ([`ordered-set.rkt`](./ordered-set.rkt)):
```scheme
(define (adjoin-set x set)
  (cond ((null? set) (list x))
        ((= x (car set)) set)
        ((> x (car set))
         (cons (car set) (adjoin-set x (cdr set))))
        (else (cons x set))))
```

Tests:
```scheme
> (adjoin-set 7 '(2 4 6 8 10))
'(2 4 6 7 8 10)
> (adjoin-set 1 '(2 3 4 5))
'(1 2 3 4 5)
> (adjoin-set 10 '(2 4 6 8))
'(2 4 6 8 10)
```

---
### Exercise 2.62

Solution:
```scheme
(define (union-set set1 set2)
  (cond ((null? set1) set2)
        ((null? set2) set1)
        (else
         (let ((x1 (car set1))
               (x2 (car set2)))
           (cond ((= x1 x2)
                  (cons x1 (union-set (cdr set1) (cdr set2))))
                 ((< x1 x2)
                  (cons x1 (union-set (cdr set1) set2)))
                 (else
                  (cons x2 (union-set set1 (cdr set2)))))))))
```

Tests:
```scheme
> (union-set '(1 2 3 4) '(3 4 5 6))
'(1 2 3 4 5 6)
> (union-set '() '(1 2 3))
'(1 2 3)
> (union-set '(1 2 3) '())
'(1 2 3)
```

---

