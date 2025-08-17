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

4. This would just mean we'd also have to change the order for how we `put` our corresponding procedures into the data table. Otherwise, the interpreter would try to look for the type-tag, and all it would find are `'deriv` symbols.

---
### Exercise 2.74

Solution ([`personnel-records.rkt`](./personnel-records.rkt)):

1. `get-record` must receive a type tag specifying which division file to retrieve; thus, each division's personnel file should be tagged with their respective type: `'business`, `'operations`, etc. 

Also, each division must incorporate their own `get-record` procedure that is able to navigate their file structure properly, and, of course, `put` this procedure into the procedure table.

```scheme
(define (get-record name file)
  (let* ((division (type-tag file))
        (record
         (apply-generic 'get-record division name (contents file))))
    (if record
        (attach-tag division record)
        #f)))
```

The ending conditional is dependent on how HQ wishes to integrate each division's records. Here, we are reattaching the division's type-tag to the record, which allows for easier passing of this record to other generic procedures. However, if we were not to follow the discipline described earlier in the chapter - that of "stripping off and attaching tags as data objects are passed from level to level" - then each division should also attach their type tag to every record file as well. This requires a lot more work, and violates the KISS principle anyway.

2. Because we reattached the division's type tag to the record, as discussed in the previous part of the exercise, then no changes need to be made to the record files themselves. All the divisions need to do is to implement their own version of the `get-salary` procedure and `put` it in the data table.

```scheme
(define (get-salary record)
  ((get 'get-salary (type-tag record)) (contents record)))
```

3. Our `get-record` procedure returns false if the record cannot be found, which makes the conditional easy to check:

```scheme
(define (find-employee-record name divisions)
  (if (null? divisions)
      #f
      (or (get-record name (car divisions))
          (find-employee-record name (cdr divisions)))))
```

4. This new company must tag their records file with a unique type-tag, and interface their procedures into the HQ's data table. Since we only generic procedures for `get-record` and `get-salary`, the installation process is rather simple.


