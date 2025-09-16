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
