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

The answer to this question is alluded to by footnote 100 of the book. It states that our use of the quote symbol (`'`) "violates the general rule that all compound expressions in our language should be delimited by parentheses and look like lists". Thus, what the interpreter _actually_ does when it receives the quote symbol is substitute in the form `(quote <expression>). 

Thus when we pass in the expression above, and since Scheme works in applicative order, it will first evaluate the subexpressions before calling `car`. This means the actual argument that we pass in is expanded to `(quote (quote abracadabra))`. Only the first `quote` actually gets evaluated, and calling the `car of this expression then simply returns `quote`.

---
