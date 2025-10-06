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
