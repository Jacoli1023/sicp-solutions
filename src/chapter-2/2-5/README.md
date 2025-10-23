# [2.5 Systems with Generic Operations](https://sarabander.github.io/sicp/html/2_002e5.xhtml#g_t2_002e5)

---
### Short disclaimer!

This chapter requires the use of mutable pairs, which the Racket language does not handle too well. Since our operator tables rely on the use of procedures such as `set-car!` and `set-cdr!`, I needed to create a module using the sicp (a subset of the Scheme language) language, which still supports mutable pairs.

The Racket command line interpreter also does not play very well with the SICP language (automatically turns `cons` to `mcons`, which makes it hard to dispatch on type), so I've also started using the Dr. Racket IDE for these solutions. The rest of this chapter - and possibly future ones - will be written in the SICP language or in Scheme, and will be run through Dr. Racket.

This chapter will rely heavily on these two files: [`table-operations.rkt`](./table-operations.rkt) and [`arithmetic-pkg.rkt`](./arithmetic-pkg.rkt). As I update this section, they will probably go through lots of changes.

---
### Exercise 2.77

This is the error we receive whenever we try to do what Louis Reasoner did:
```scheme
> (define z (make-complex-from-real-imag 3 4))
> (magnitude z)
. . table-operations.rkt:54:8: error: format string requires 0 arguments, given 2; arguments were: magnitude (complex)
```

Solution:

If we put the following lines into the complex number package, then our previous expressions will work:
```scheme
(put 'real-part '(complex) real-part)
(put 'imag-part '(complex) imag-part)
(put 'magnitude '(complex) magnitude)
(put 'angle '(complex) angle)
```

Test:
```scheme
> (magnitude z)
5
```

The reason this works is because up until this point, the complex number package - as we installed it - did not have those selectors defined and installed for it. Only through its sub-packages were they available. When we install or `put` in the generic selectors into the complex number package, what this essentially does is it strips off the `complex` type tag, and tries the selector procedure again (though this time, only the `polar` or `rectangular` type tag is left).

Here's how the procedure trace would look like:
```scheme
(magnitude z)
> (apply-generic 'magnitude '(complex rectangular 3 . 4))
> (apply (get 'magnitude '(complex)) '((rectangular 3 . 4)))
> (magnitude '(rectangular 3 . 4))
> (apply-generic 'magnitude '((rectangular 3 . 4)))
> (apply (get 'magnitude '(rectangular)) '((3 . 4)))
> (sqrt (+ (square 3) (square 4)))
> (sqrt (+ 9 16))
> (sqrt 25)
5
```

As we can see here, `apply-generic` gets invoked twice. It dispatches once to the `magnitude` procedure in the complex package (which just calls the generic `magnitude` selector again), and a second time to the `magnitude` procedure within the rectangular complex number sub-package. This is an example of stripping off multiple layers of type tags before reaching the correct procedure.

---
### Exercise 2.78

Solution:

The following changes were made to the type tagging system:
```scheme
(define (attach-tag type-tag contents)
  (if (eq? type-tag 'scheme-number)
    contents
    (cons type-tag contents)))
(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'scheme-number)
        (else (error 'type-tag "bad tagged datum" datum))))
(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error 'contents "bad tagged datum" datum))))
```

Specifically, we just added another conditional clause to each procedure, for checking if the data type is a built-in primitive number or not. This makes working with explicit numbers much easier, since we no longer have to call the `make-scheme-number` procedure:
```scheme
> (scheme-number-pkg)
done
> (add 1 2)
3
> (sub 3 4)
-1
> (mul 5 6)
30
> (div 8 4)
2
```

---
### Exercise 2.79

Solution:

I put the following 'equ?` prcoedures in their respective packages:
```scheme
(put 'equ? '(scheme-number scheme-number) =)
(put 'equ? '(rational rational)
    (lambda (x y)
        (and (= (numer x) (numer y))
            (= (denom x) (denom y)))))
(put 'equ? '(complex complex)
    (lambda (z1 z2)
        (and (= (real-part z1) (real-part z2))
            (= (imag-part z1) (imag-part z2)))))

...

(define (equ? x y) (apply-generic 'equ? x y))
```

Test:
```scheme
> (equ? 1 1)
#t
> (equ? 1 2)
#f
> (equ? (make-rational 1 2) (make-rational 1 2))
#t
> (equ? (make-rational 1 2) (make-rational 3 4))
#f
> (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2))
#t
> (equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 3 4))
#f
```

---
### Exercise 2.80

Put the following predicate procedures in their respective number packages:
```
(put '=zero? '(scheme-number) zero?)
(put '=zero? '(rational)
    (lambda (x) (zero? (numer x))))
(put '=zero? '(complex)
    (lambda (x) (and (zero? (real-part x))
                     (zero? (imag-part x)))))

...

(define (=zero? n) (apply-generic '=zero? n))
```

Tests:
```scheme
> (=zero? 0)
#t
> (=zero? (make-scheme-number 0))
#t
> (=zero? 1)
#f
> (=zero? (make-rational 0 1))
#t
> (=zero? (make-rational 1 1))
#f
> (=zero? (make-complex-from-real-imag 0 0))
#t
> (=zero? (make-complex-from-real-imag 0 1))
#f
```

---
### Exercise 2.81

```scheme
(define (scheme-number->scheme-number n) n)
(define (complex->complex z) z)

(put-coercion 'scheme-number 'scheme-number
              scheme-number->scheme-number)

(put-coercion 'complex 'complex 
              complex->complex)
```

Solution:\
1. If `apply-generic` is called with two arguments of the same type to search for an operation that does not exist for those types, then infinite recursion will occur. It will keep trying to coerce one argument into the second over and over, resulting in an infinite loop.

2. Louis is incorrect (as per usual, smh) that something had to be done, because `apply-generic` already returns an error after not finding the correct operation or failing to find a coercion for the given types.

3. To prevent coercion from happening with two arguments of the same type, we simply need to have an equality check within the body of the `let` that checks if `type1` is equal to `type2`. If that is the case, our `apply-generic` procedure should return an error, since there is no operation to be found for those types:

```scheme
(define (apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (err-sig)
      (error "No method for these types" (list op type-tags)))
    (if proc
        (apply proc (map contents args))
        (if (= (length args) 2)
            (let ((type1 (car type-tags))
                  (type2 (cadr type-tags)))
              (if (eq? type1 type2) ; equality check here
                  (err-sig)         ; signals an error if true
                  (let ((a1 (car args))
                        (a2 (cadr args))
                        (t1->t2 (get-coercion type1 type2))
                        (t2->t1 (get-coercion type2 type1)))
                    (cond (t1->t2 (apply-generic op (t1->t2 a1) a2))
                          (t2->t1 (apply-generic op a1 (t2->t1 a2)))
                          (else (err-sig))))
            (err-sig)))))
```

---
### Exercise 2.82

Solution:

This one was a bit of a pain in the butt, and could definitely be rewritten to be more pretty, but it works and I'm not complaining.
```scheme
(define (apply-generic op . args)

  (define (coercable? coerce-procs)
    (not (member #f coerce-procs)))

  (define (coerce-args coercion-procs args)
    (map (lambda (coerce-proc arg)
           (coerce-proc arg))
         coercion-procs
         args))

  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (try-coercion tags)
      (if (null? tags)
          (error "No method for these types - APPLY-GENERIC" (list op type-tags))
          (let* ((target-type (car tags))
                 (arg-coercions (map (lambda (coerce-from)
                                       (if (eq? coerce-from target-type)
                                           identity
                                           (get-coercion coerce-from target-type)))
                                     type-tags)))
            (if (coercable? arg-coercions)
                (apply apply-generic
                       op
                       (coerce-args arg-coercions args))
                (try-coercion (cdr tags))))))
    (if proc
        (apply proc (map contents args))
        (try-coercion type-tags))))
```

Be sure to include the proper coercion procedures within each relevant arithmetic package. 

Some tests:
```scheme
> (define sn (make-scheme-number 5))
> (define rn (make-rational 3 4))
> (define cn (make-complex-from-real-imag 1 2))

> (apply-generic 'add sn sn)
10
> (apply-generic 'add sn rn)
(rational 23 . 4)
> (apply-generic 'add rn rn)
(rational 3 . 2)
> (apply-generic 'add rn sn)
(rational 23 . 4)
> (apply-generic 'add rn cn)
(complex rectangular 1 3/4 . 2)
> (apply-generic 'add sn cn)
(complex rectangular 6 . 2)
```

---
### Exercise 2.83

Solution:

The first step we must do in order to make our system follow the tower design is we must break up our scheme number package into two distinct packages: one for integers and one for real numbers.

```scheme
(define (integer-pkg)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) 
       (lambda (x y)
         (let ((z (/ x y)))
           (if (integer? z) (tag z) (make-rational x y)))))
  (put 'make 'integer tag)
  'done)

(define (real-pkg)
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'make 'real tag)
  'done)
```

Now then, to implement our raise procedures, we'll need to define a procedure that raise one numeric data type to the next within each corresponding package. Then, we put each raise procedure into the operation-lookup table, and create a generic `raise` procedure that applies the correct raise operation:

```scheme
(define (raise x) (apply-generic 'raise x)) ; generic raise procedure
...
; in the newly created integer package
  (define (integer->rational n)
    (make-rational n 1))
  (put 'raise '(integer) integer->rational)
...
; in the rational package
  (define (rational->real n)
    (make-real (exact->inexact (/ (numer n) (denom n)))))
  (put 'raise '(rational) rational->real)
...
; in the newly created real package
  (define (real->complex n)
    (make-complex-from-real-imag n 0))
  (put 'raise '(real) real->complex)
```

Some tests:
```scheme
> (define int_n (make-integer 5))
> (define rat_n (make-rational 3 4))
> (define real_n (make-real 1.2))
> (raise int_n)
(rational 5 . 1)
> (raise rat_n)
(real . 0.75)
> (raise real_n)
(complex rectangular 1.2 . 0)
```

---
### Exercise 2.84

Solution:

The first step to modifying the `apply-generic` procedure to use the new `raise` procedure is create a hierarchy structure for our data types. The simplest method for doing so is (of course) creating a list, and then `cdr`ing down to find the correct data type in the hierarchy.

```solution
(define type-tower
  '(integer rational real complex))
```

Now for some helper functions that we'll need. For our `apply-generic` procedure to work with a data type hierarchy, we'll need to be able to find the type level of a given argument, find the highest type level present in an argument list, and be able to raise an argument's type to a specific type level. Here are the following helper procedures designed to give us this functionality:

```scheme
  ; helper function to find the numeric level of the data type within
  ; the given tower hierarchy
  (define (find-type-level type)
    (define (iter tower n)
      (cond ((null? tower) #f)
            ((eq? type (car tower)) n)
            (else (iter (cdr tower) (+ n 1)))))
    (iter type-tower 0))

  ; finds the highest data type level in a given list of args
  (define (find-highest-type-level)
    (define (iter arg-list highest)
      (if (null? arg-list)
          highest
          (let ((type-level (find-type-level (type-tag (car arg-list)))))
            (if (> type-level highest)
                (iter (cdr arg-list) type-level)
                (iter (cdr arg-list) highest)))))
    (iter args (find-type-level (type-tag (car args)))))

  ; raises given argument to the target type-level in the hierarchy
  (define (raise-to arg target-type-level)
    (let ((type-level (find-type-level (type-tag arg))))
      (cond ((= type-level target-type-level) arg)
            ((< type-level target-type-level)
             (raise-to (raise arg) target-type-level))
            (else (error "Cannot raise argument to a lower type-level"
                         arg target-type-level)))))
```

Now to plug these into our `apply-generic` procedure. I'm using these as internal definitions, however it can be reasonable to define them outside of the `apply-generic` procedure as well, with a few modifications:

```scheme
(define (apply-generic op . args)

  ; helper function to find the numeric level of the data type within
  ; the given tower hierarchy
  (define (find-type-level type)
    (define (iter tower n)
      (cond ((null? tower) #f)
            ((eq? type (car tower)) n)
            (else (iter (cdr tower) (+ n 1)))))
    (iter type-tower 0))

  ; finds the highest data type level in a given list of args
  (define (find-highest-type-level)
    (define (iter arg-list highest)
      (if (null? arg-list)
          highest
          (let ((type-level (find-type-level (type-tag (car arg-list)))))
            (if (> type-level highest)
                (iter (cdr arg-list) type-level)
                (iter (cdr arg-list) highest)))))
    (iter args (find-type-level (type-tag (car args)))))

  ; raises given argument to the target type-level in the hierarchy
  (define (raise-to arg target-type-level)
    (let ((type-level (find-type-level (type-tag arg))))
      (cond ((= type-level target-type-level) arg)
            ((< type-level target-type-level)
             (raise-to (raise arg) target-type-level))
            (else (error "Cannot raise argument to a lower type-level"
                         arg target-type-level)))))

  ; apply-generic
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let ((target-type-level (find-highest-type-level)))
          (apply apply-generic op (map (lambda (arg)
                                         (raise-to arg target-type-level))
                                       args))))))
```

Now for a few tests:
```scheme
> (add int_n rat_n)
(rational 5 . 3)
> (add rat_n int_n)
(rational 5 . 3)
> (mul int_n (add rat_n real_n))
(real . 6.266666666666667)
> (add rat_n complex_n)
(complex rectangular 7.666666666666667 . 8)
```

---
### Exercise 2.85

Solution:

The first thing we must do in order to implement this method of simplifying our data objects is to, like how the exercise states, define the generic operation `project`. This means that within each data package we should define how to lower the data type into the type that's one level down in the hierarchy (basically the opposite of `raise`). We can also make use of some primitive procedures provided by the SICP/Scheme language to make our lives a little easier:

```scheme
; generic operator
(define (project x)
  (apply-generic 'project x))
...
...
; in the complex package
  (define (complex->real z)
    (make-real (real-part z)))
  (put 'project '(complex) complex->real)
...
...
; in the real package
  (define (real->rational x)
    (let ((y (inexact->exact x)))
      (make-rational (numerator y) (denominator y))))
  (put 'project '(real) real->rational)
...
...
; in the rational package
  (define (rational->integer x)
    (make-integer (quotient (numer x) (denom x))))
  (put 'project '(rational) rational->integer)
```

Now then, as the exercise states, to see if we are able to perform the drop, we can project a data object and then raise it up again, then test if it is equivalent to the original data object's value. We can make use of our `equ?` procedures from before (while we're at it, make sure that `equ?` is implemented for our real and integer packages):

```scheme
(define (drop x)
  (let ((type (type-tag x)))
    (if (= (find-type-level type) 0)
        x
        (let* ((projected (project x))
               (raised (raise projected)))
          (if (equ? x raised)
              (drop projected)
              x)))))
```

Let's go ahead and test these out before implementing them in our `apply-generic` procedure:

```scheme
> (project (raise (raise int_n)))
(rational 1 . 1)
> (project (project (raise (raise int_n))))
1
> (drop (raise int_n))
1
> (drop (raise (raise int_n)))
1
> (drop (raise (raise (raise int_n))))
1
```

As we can see, no matter what level our data object is in the type hierarchy, `drop` and `project` work as intended.

Now then, in order to implement this new `drop` procedure into our `apply-generic` procedure, we first must establish the rules for _when_ it makes sense to try to drop the result. Otherwise, we may run into trouble if we try dropping the result of certain generic procedures (such as our predicate `equ?` procedure that is used in `drop` itself). The only such times we really care about dropping the result is when arithmetic is involved. So then, we can implement a predicate procedure that checks if the given generic operation is one of `add`, `sub`, `mul`, or `div`, and if it is, we can attempt to drop the result of our generic `apply` procedure:

```scheme
; helper procedure for identifying when to drop the result
  (define (reduce x)
    (cond ((eq? op 'add) (drop x))
          ((eq? op 'sub) (drop x))
          ((eq? op 'mul) (drop x))
          ((eq? op 'div) (drop x))
          (else x)))
...
...
; drop result returned from our generic apply
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (reduce (apply proc (map contents args)))
        (let ((target-type-level (find-highest-type-level)))
          (apply apply-generic op (map (lambda (arg)
                                         (raise-to arg target-type-level))
                                       args))))))
```

And for some final tests:

```scheme
> (drop (make-complex-from-real-imag 4 0))
4
> (add (make-complex-from-real-imag 4 0) (make-integer 8))
12
> (sub (make-real 3.5) (make-real 1.5))
2
> (div (make-rational 3 2) (make-integer 9))
(rational 1 . 6)
```

---
### Exercise 2.86

Solution:

In order for our complex numbers to work with our own user-defined data types, we must generalize the procedures within the complex number package. Those such as `add-complex` and `mul-complex` must now use our generic procedures `add` and `mul` instead of Scheme's `+` and `*`, respectively.

```scheme
; internal definitions of the complex number package
  (define (add-complex z1 z2)
    (make-from-real-imag (add (real-part z1) (real-part z2))
                         (add (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (sub (real-part z1) (real-part z2))
                         (sub (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (mul (magnitude z1) (magnitude z2))
                       (add (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (div (magnitude z1) (magnitude z2))
                       (sub (angle z1) (angle z2))))
```

We now also need to ensure the procedures within the polar and rectangular sub-packages are using generic arithmetic procedures. This means, as the problem statement suggests, we must create generic procedures for the square root, sine, cosine, and inverse tangent functions. We'll go ahead and implement these into our respective number packages:

```scheme
; integer package
  (put 'square-root '(integer) (lambda (x) (make-real (sqrt x))))
  (put 'sine' (integer) (lambda (x) (make-real (sin x))))
  (put 'cosine '(integer) (lambda (x) (make-real (cos x))))
  (put 'arctan '(integer integer) (lambda (x y) (make-real (atan x y))))
...
...
; rational package
  (define (ratio x) (/ (numer x) (denom x)))
  (put 'square-root '(rational) (lambda (x) (make-real (sqrt (ratio x)))))
  (put 'sine '(rational) (lambda (x) (make-real (sin (ratio x)))))
  (put 'cosine '(rational) (lambda (x) (make-real (cos (ratio x)))))
  (put 'arctan '(rational) 
       (lambda (y x) (make-real (atan (ratio y) (ratio x)))))
...
...
; real package
  (put 'square-root '(real) (lambda (x) (tag (sqrt x))))
  (put 'sine '(real) (lambda (x) (tag (sin x))))
  (put 'cosine '(real) (lambda (x) (tag (cos x))))
  (put 'arctan '(real real) (lambda (y x) (tag (atan y x))))
...
...
; generic procedures
(define (square x) (mul x x))
(define (square-root x) (apply-generic 'square-root x))
(define (sine x) (apply-generic 'sine x))
(define (cosine x) (apply-generic 'cosine x))
(define (arctan x y) (apply-generic 'arctan x y))
```

With these defined and implemented, we can now rewrite the procedures in the rectangular and polar complex number packages:

```scheme
(define (rectangular-pkg)
  ;; Internal procedures
  (define real-part car)
  (define imag-part cdr)
  (define make-from-real-imag cons)
  (define (magnitude z)
    (square-root (add (square (real-part z))
                      (square (imag-part z)))))
  (define (angle z)
    (arctan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (mul r (cosine a)) (mul r (sine a))))

  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a)))))

(define (polar-pkg)
  ;; Internal procedures
  (define magnitude car)
  (define angle cdr)
  (define make-from-mag-ang cons)
  (define (real-part z)
    (mul (magnitude z) (cosine (angle z))))
  (define (imag-part z)
    (mul (magnitude z) (sine (angle z))))
  (define (make-from-real-imag x y)
    (cons (square-root (add (square x) (square y)))
          (arctan y x)))

  ;; Interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a)))))
```

Now then, we just need to fix a few more operations now that the components of the complex numbers could be different types, such as `equ?`. Now that our complex number's real part and imaginary part could be different data types, we'd need to be able to call `equ?` on the components themselves.

We'll also need to update the list of possible reduceable operations in our `apply-generic` procedure, in order to include our new set of trigonomic functions:

```scheme
; within the complex number package, equ?
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (and (equ? (real-part z1) (real-part z2))
              (equ? (imag-part z1) (imag-part z2)))))
...
...
; within our apply-generic procedure
  (define (reduce x)
    (cond ((eq? op 'add) (drop x))
          ((eq? op 'sub) (drop x))
          ((eq? op 'mul) (drop x))
          ((eq? op 'div) (drop x))
          ((eq? op 'square-root) (drop x))
          ((eq? op 'sine) (drop x))
          ((eq? op 'cosine) (drop x))
          ((eq? op 'arctan) (drop x))
          (else x)))
```

Now for a few tests, using the following numbers:

```scheme
(define int_n (make-integer 1))
(define rat_n (make-rational 2 3))
(define real_n (make-real 5.6))

(define z1 (make-complex-from-real-imag rat_n int_n))
(define z2 (make-complex-from-mag-ang real_n rat_n))
```

```scheme
> (equ? z1 z1)
#t
> (equ? z1 z2)
#f
> (add z1 z2)
(complex
 rectangular
 (rational 4279237606951109 . 844424930131968)
 rational
 157023310231171
 .
 35184372088832)
```

This set of exercises was rather difficult, and it looks like some generic procedures do not work anymore after making these changes. I do not believe that my `apply-generic`, `raise` and `projection` methods are the cleanest nor smoothest, yet these things have gotten quite entangled that it's a little hard to debug. I think my problem stems from me constantly modifying and altering the base arithmetic packages themselves, instead of creating new packages to add/install on top when the exercise problem asks for it. This is a better example of modularity and data additivity that the section was preaching for. At some point I would like to redo this section but with better programming principles so that these changes I implement do not break the previous set of packages.
