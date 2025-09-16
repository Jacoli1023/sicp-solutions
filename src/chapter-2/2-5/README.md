# [2.5 Systems with Generic Operations](https://sarabander.github.io/sicp/html/2_002e5.xhtml#g_t2_002e5)

---
### Short disclaimer!
This chapter requires the use of mutable pairs, which the Racket language does not handle too well. Since our operator tables rely on the use of procedures such as `set-car!` and `set-cdr!`, I needed to create a module using the sicp (a subset of the Scheme language) language, which still supports mutable pairs.

The Racket command line interpreter also does not play very well with the SICP language (automatically turns `cons` to `mcons`, which makes it hard to dispatch on type), so I've also started using the Dr. Racket IDE for these solutions. The rest of this chapter - and possibly future ones - will be written in the SICP language or in Scheme, and will be run through Dr. Racket.
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
