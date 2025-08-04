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
### Exercise 2.63

```scheme
(define (tree->list-1 tree)
  (if (null? tree)
      '()
      (append (tree->list-1 (left-branch tree))
              (cons (entry tree)
                    (tree->list-1 (right-branch tree))))))

(define (tree->list-2 tree)
  (define (copy-to-list tree result-list)
    (if (null? tree)
        result-list
        (copy-to-list (left-branch tree)
                      (cons (entry tree)
                            (copy-to-list (right-branch tree)
                                          result-list)))))
  (copy-to-list tree '()))
```

Solution:

1. First, we'll go ahead and define the trees from figure 2.16, which represent the set {1, 3, 5, 7, 9, 11}. Excuse the data abstraction violation, but I don't want to write `make-tree` a thousand times over.

```scheme
(define t1 '(7 (3 (1 () ()) (5 () ())) (9 () (11 () ()))))
(define t2 '(3 (1 () ()) (7 (5 () ()) (9 () (11 () ())))))
(define t3 '(5 (3 (1 () ()) ()) (9 (7 () ()) (11 () ()))))
```

Now, to test out the procedures using these trees:
```scheme
> (tree->list-1 t1)
'(1 3 5 7 9 11)
> (tree->list-2 t1)
'(1 3 5 7 9 11)
> (tree->list-1 t2)
'(1 3 5 7 9 11)
> (tree->list-2 t2)
'(1 3 5 7 9 11)
> (tree->list-1 t3)
'(1 3 5 7 9 11)
> (tree->list-2 t3)
'(1 3 5 7 9 11)
```

Yes, the two procedures produce the same result for every tree. The main difference is in the processes each procedure generates. The `tree->list-1` procedure produce a recursive process, while the `tree->list-2` procedure forms an iterative process.

2. I'm not too familiar with order of growth rules quite yet, however, I can judge it based on the mechanisms each procedure uses to generate the list.

Both procedures visit each node in the tree, however the first procedure uses `append` as its method for generating the list, whereas the second procedure uses `cons`. We're assuming the trees we used are balanced, thus `append` will be working with halved trees on every call (with `left-branch` and `right-branch` being the divided trees).

Thus, the first procedure works in $\theta(n \log(n))$ steps, and the second procedure works in $\theta(n)$ steps. Specifically, the $\log(n)$ is the order of growth produced by the `append` procedure, and since `cons` works in constant time ($\theta(1)$), we implicitly multiply the second procedure's order of growth by 1. And since both procedures must visit every leaf in the tree, there is always going to be a linear growth factor to the tree (where the _n_ comes from in each order of growth).

I'll learn how to explain order of growth better in the future...

---
### Exercise 2.64

```scheme
(define (list->tree elements)
  (car (partial-tree elements (length elements))))

(define (partial-tree elts n)
  (if (= n 0)
      (cons '() elts)
      (let* ((left-size (quotient (- n 1) 2))
             (left-result (partial-tree elts left-size))
             (left-tree (car left-result))
             (non-left-elts (cdr left-result))
             (this-entry (car non-left-elts))
             (right-size (- n (+ left-size 1)))
             (right-result (partial-tree (cdr non-left-elts) right-size))
             (right-tree (car right-result))
             (remaining-elts (cdr right-result)))
        (cons (make-tree this-entry left-tree right-tree)
              remaining-elts))))
```

1. `partial-tree` works by recursively partitioning and dividing the list around a central element within the list. We call `partial-tree` when generating each sub-tree's left and right branches. The result returned by each call to this procedure is a list, where the first element in the list is the generated subtree (which will form the left branch of the tree), and the rest of the elements are those that have not been generated into a tree yet. The first element of those that have not been generated into a tree yet (that is the `this-entry` local variable), is the central entry point into the tree/subtrees. The recursive call then gets passed the rest of the elements within the list to form the right branch of the tree.

The tree produced by the procedure when given the list `'(1 3 5 7 9 11)` looks like this:

```
'(5 (1 () (3 () ())) (9 (7 () ()) (11 () ())))

           5
          / \
         /   \
        1     9
         \   / \
          3 7   11
```

2. The procedure only visits each leaf in the tree once, and only uses operations that run in constant time (`cons`, `car`, `cdr`, `make-tree`, arithmetic, etc.). Thus the order of growth is $\theta(n)$.

---
### Exercise 2.65

Solution:

Excellent time to employ the DRY principle! We already have all the procedures we need to perform this. We can convert each tree into an ordered list (using `list->tree-2`), call `union-set` or `intersection-set` from [`ordered-set.rkt`](./ordered-set.rkt), and then convert the result of those procedures into a balanced binary tree using the `list->tree` procedure from before! All of these procedures run in linear time, thus our resulting procedure will also run in linear time.

```scheme
(define (union-tree tree1 tree2)
  (list->tree (union-set (tree->list-2 tree1) (tree->list-2 tree2))))

(define (intersection-tree tree1 tree2)
  (list->tree (intersection-set (tree->list-2 tree1) (tree->list-2 tree2))))
```

For my tests, I'll be using the balanced binary trees formed from these respective lists:
```scheme
> (define l1 '(1 2 3 4 5))
> (define l2 '(4 5 6 7 8))
> (define t1 (list->tree l1))
> (define t2 (list->tree l2))
```

I'll be converting each result back into an ordered list to make it easier to read and identify if I got the expected result or not.

Tests:
```scheme
> (tree->list-2 (union-tree '() t1))
'(1 2 3 4 5)
> (tree->list-2 (union-tree t1 '()))
'(1 2 3 4 5)
> (tree->list-2 (union-tree t1 t2))
'(1 2 3 4 5 6 7 8)

> (tree->list-2 (intersection-tree '() t2))
'()
> (tree->list-2 (intersection-tree t2 '()))
'()
> (tree->list-2 (intersection-tree t1 t2))
'(4 5)
```

---
### Exercise 2.66

Solution:

Since the set of records is organized by a numerical set of keys, our `lookup` procedure will be very similar to the `element-of-set?` procedure in our [`tree-set.rkt`](./tree-set.rkt) module. The main difference simply being that we must now use a key selector, instead of directly comparing numerical values.

```scheme
(define (lookup given-key set-of-records)
  (if (null? set-of-records)
      false
      (let* ((record (entry set-of-records))
             (record-key (key record)))
        (cond ((= given-key record-key) record)
              ((< given-key record-key)
               (lookup given-key (left-branch set-of-records)))
              (else
               (lookup given-key (right-branch set-of-records)))))))
```

For testing purposes, we'll create a scenario in which we're looking through a record of personnel files within a company. For simplicity's sake, each record will simply be the name of the employee as well as a numerical value representing the employee's ID (this will be our key). It will be in the form of: (<ID> <Name>). Thus, our `key` selector is just a simple `car`.

```scheme
> (define personnel '((2 Jacob) ((1 John) () ()) ((3 Jane) () ())))
> (lookup 3 personnel)
'(3 Jane)
```

---
### Exercise 2.67

```scheme
(define sample-tree
  (make-code-tree 
   (make-leaf 'A 4)
   (make-code-tree
    (make-leaf 'B 2)
    (make-code-tree 
     (make-leaf 'D 1)
     (make-leaf 'C 1)))))

(define sample-message 
  '(0 1 1 0 0 1 0 1 0 1 1 1 0))
```

Solution:

```scheme
> (decode sample-message sample-tree)
'(A D A B B C A)
```

---
### Exercise 2.68
