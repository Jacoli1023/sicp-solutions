# [2.2 Hierarchical Data and the Closure Property](https://sarabander.github.io/sicp/html/2_002e2.xhtml#g_t2_002e2)

### Exercise 2.17

Solution:
```scheme
(define (last-pair items)
  (if (null? (cdr items))
      (car items)
      (last-pair (cdr items))))
```

Test:
```scheme
> (last-pair (list 23 72 149 34))
34
```

---
### Exercise 2.18

Solution:
```scheme
(define (reverse items)
  (define (iter l1 l2)
    (if (null? l1)
        l2
        (iter (cdr l1) (cons (car l1) l2))))
  (iter items '()))
```

Test:
```scheme
> (reverse (list 1 2 3 4))
'(4 3 2 1)
> (reverse (list 1 4 9 16 25))
'(25 16 9 4 1)
```

---
### Exercise 2.19
