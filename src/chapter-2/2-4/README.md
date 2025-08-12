# [2.4 Multiple Representations for Abstract Data](https://sarabander.github.io/sicp/html/2_002e4.xhtml#g_t2_002e4)

### Exercise 2.73

```scheme
(define (deriv exp var)
   (cond ((number? exp) 0)
         ((variable? exp) 
           (if (same-variable? exp var) 
               1 
               0))
         (else ((get 'deriv (operator exp)) 
                (operands exp) 
                var))))

(define (operator exp) (car exp))
(define (operands exp) (cdr exp))
```

Solution ([`deriv-pkgs.rkt`](./deriv-pkgs.rkt)):

1. We changed the `deriv` procedure from the previous section into one that operates in a more data-directed approach. This procedure now dispatches on type, where the type of the `deriv` procedure we wish to perform is based on the arithmetic operator (`+`, `*`, etc.), which serves as the "type tag". When the procedure encounters an operator, it will look up the corresponding procedure in a table via the `get` procedure call, and then call that procedure with the subsequent operands and variable symbol as its arguments.

We cannot assimilate the predicates `number?` and `variable?` because those are not user-defined types, and we cannot attach a corresponding tag to them.

2. One thing to address about this new `deriv` procedure is that we are calling a package-specific derivation procedure to be operated on the given list of operands. This means that our selectors must be a little different than before, since the expression's operator is considered the type tag, and is stripped off before being sent to the appropriate procedure.

- The sum package:
```scheme
(define (install-sum-package)
  (define addend car)
  (define (augend sum)
    (accumulate make-sum 0 (cdr sum)))
  (define (deriv-sum operands var)
    (make-sum (deriv (addend operands) var)
              (deriv (augend operands) var)))
  (put 'deriv '+ deriv-sum)
  'done)
```

- The product package:
```scheme
(define (install-product-package)
  (define multiplier car)
  (define (multiplicand product)
    (accumulate make-product 1 (cdr product)))
  (define (deriv-product operands var)
    (make-sum (make-product (multiplier operands)
                            (deriv (multiplicand operands) var))
              (make-product (deriv (multiplier operands) var)
                            (multiplicand operands))))
  (put 'deriv '* deriv-product)
  'done)
```

Names like `install-product-package` are too long; they shall be shortened to names that are akin to `product-pkg`.

3. The exponentiation package:
```scheme
(define (expt-pkg)
  (define base car)
  (define expt cadr)
  (define (deriv-expt operands var)
    (make-product
     (expt operands)
     (make-product (make-exponentiation (base operands)
                                        (- (expt operands) 1))
                   (deriv (base operands) var))))
  (put 'deriv '** deriv-expt)
  'done)
```

With this approach, not a single thing needed to be changed in the `deriv` procedure. In fact, to further make this even more data-directed, I should also include the constructors and selectors within the scope of the respective packages, and then `put` them into the data table. However, that is beyond what the exercise requires.

4. This would just mean we'd also have to change the order for how we `put` our corresponding procedures into the data table. Otherwise, the interpreter would try to look for the type-tag, and all it would find are `\`deriv' symbols.

---
### Exercise 2.74
