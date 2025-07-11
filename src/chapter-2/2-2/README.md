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

```scheme
(define us-coins 
  (list 50 25 10 5 1))
(define uk-coins 
  (list 100 50 20 10 5 2 1 0.5))

(define (cc amount coin-values)
  (cond ((= amount 0) 1)
        ((or (< amount 0) (no-more? coin-values)) 0)
        (else
         (+ (cc amount (except-first-denomination coin-values))
            (cc (- amount (first-denomination coin-values))
             coin-values)))))
```

Solution ([`count-change.rkt`](./count-change.rkt)):
```scheme
(define (first-denomination coins)
  (car coins))
(define (except-first-denomination coins)
  (cdr coins))
(define (no-more? coins)
  (null? coins))
```

The order of the list `coin-values` does not affect the answer produced by `cc`, as shown below where the lists are reversed:
```scheme
> (cc 100 us-coins)
292
> (cc 100 uk-coins)
104561
> (cc 100 (reverse us-coins))
292
> (cc 100 (reverse uk-coins))
104561
```

This is because we are still traversing through the same list and exploring every possible combination with each coin denomination. Regardless of the method we use - smaller coins first, larger coins first, or something in between - we're still going through and using every coin denomination.

---
### Exercise 2.20

Solution:
```scheme
(define (same-parity x . y)
  (let ((even-odd (if (even? x) #t #f)))
    (define (iter ls)
      (cond ((null? ls) ls)
            ((eq? (even? (car ls)) even-odd)
             (cons (car ls) (iter (cdr ls))))
            (else (iter (cdr ls)))))
    (iter (cons x y))))
```

Tests:
```scheme
> (same-parity 1 2 3 4 5 6 7)
'(1 3 5 7)
> (same-parity 2 3 4 5 6 7)
'(2 4 6)
```

---
### Exercise 2.21

```scheme
(define (map proc items)
  (if (null? items)
      '()
      (cons (proc (car items))
            (map proc (cdr items)))))

(define (square x) (* x x))
```

Solution:
```scheme
(define (square-list items)
  (if (null? items)
      `() ; nil
      (cons (square (car items)) (square-list (cdr items)))))

(define (square-list items)
  (map square items))
```

Both produce the same results, since the computer is performing the same processes. The only difference between the two versions is the `map` abstraction barrier:
```scheme
> (square-list (list 1 2 3 4))
(1 4 9 16)
```

---
### Exercise 2.22

Solution:

The reason that the answer list is produced in the reverse order is because in his iterative process, he is first cdr'ing through the list before he begin cons'ing anything. So by the time he starts building the list, he is starting at the end of the list, and as the recursive stack unwinds, it cons's each previous element, resulting in a list that is in the reverse order of what he wants.

Interchanging the arguments to `cons` does not work either, and produces the following result:
```scheme
> (square-list (list 1 2 3 4))
'((((() . 1) . 4) . 9) . 16)
```

Following the process of this, we find that `answer` starts out as `nil`. When we then `cons` `nil` with each individual squared number in the list, and is then printed as a sequence of `car`s and `cdr`s, rather than an actual list.

---
### Exercise 2.23

Solution ([`map.rkt`](./map.rkt)):
```scheme
(define (for-each proc items)
  (when (not (null? items))
    (proc (car items))
    (for-each proc (cdr items))))
```

Test:
```scheme
> (for-each
   (lambda (x) (newline) (display x))
   (list 57 321 88))

57
321
88
```

---
