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
### Exercise 2.24

Solution:
```scheme
> (list 1 (list 2 (list 3 4)))
'(1 (2 (3 4)))
```

Box-and-pointer diagram:
```diagram
     .---+---.    .---+--+.
---->| * | *-+--->| * | / +
     '-|-+---'    '-|-++--'
       |            |
       v            v
     .---.      .---+---.    .---+--+.
     | 1 |      | * | *-+--->| * | / |
     '---'      '-|-+---'    '-|-++--'
                  |            |
                  v            v
                .---.      .---+---.    .---+--+.
                | 2 |      | * | *-+--->| * | / |
                '---'      '-|-+---'    '-|-++--'
                             |            |
                             v            v
                           .---.        .---.
                           | 3 |        | 4 |
                           '---'        '---'
```

Tree representation:
```diagram
 "(1 (2 (3 4)))"
       +
      / \
     /   \ "(2 (3 4))"
          +
    1    / \
        /   \ "(3 4)"
             +
       2    / \
           /   \

          3     4
```

---
### Exercise 2.25

```scheme
(define a (list 1 3 (list 5 7) 9))
(define b (list (list 7)))
(define c (list 1 (list 2 (list 3 (list 4 (list 5 (list 6 7)))))))

> a
'(1 3 (5 7) 9)
> b
'((7))
> c
'(1 (2 (3 (4 (5 (6 7))))))
```

Solution:

```scheme
> (car (cdr (car (cdr (cdr a)))))
7
> (car (car b))
7
> (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr (car (cdr c))))))))))))
7
```

---
### Exercise 2.26

```scheme
(define x (list 1 2 3))
(define y (list 4 5 6))
```

Solution:

Without putting each expression into the interpreter, I hypothesize that:
- `append` will create a flat list of each element in `x` and `y` in order - `'(1 2 3 4 5 6)`
- `cons` will simply put the list `x` as the first element of the list `y` - `'((1 2 3) 4 5 6)`
- `list` will create a list of two elements: the list `x` and the list `y` - `'((1 2 3) (4 5 6))`

Testing my hypotheses:
```scheme
> (append x y)
'(1 2 3 4 5 6)
> (cons x y)
'((1 2 3) 4 5 6)
> (list x y)
'((1 2 3) (4 5 6))
```

My hypotheses were proven to be correct.

---
### Exercise 2.27

```scheme
(define x (list (list 1 2) (list 3 4)))
```

Solution ([`tree-operations.rkt`](./tree-operations.rkt)):\
There are a couple of methods for constructing the `deep-reverse` procedure. The first is using the `reverse` procedure we built in exercise 2.18 as a guide, and modifying it so also performs the `reverse` on each of the subtrees, recursively. This is what the exercise says we should do:
```scheme
(define (deep-reverse ls)
  (define (iter l1 l2)
    (cond ((null? l1) l2)
          ((not (pair? l1)) l1)
          (else (iter (cdr l1) (cons (deep-reverse (car l1)) l2)))))
  (iter ls '()))
```

Another way, and a simpler one to implement, is using the `map` procedure defined earlier in the chapter ([`map.rkt`](./map.rkt)):
```scheme
(define (deep-reverse x)
  (if (pair? x)
      (map deep-reverse (reverse x))
      x))
```

The difference between the two is that the latter method (that uses the `map` procedure) actually reverses the tree as we go down its branches, while the former method travels down the tree's branches, and _then_ starts building up the deep-reversed list. Both, however, return the correct answer:
```scheme
> x
'((1 2) (3 4))
> (reverse x)
'((3 4) (1 2))
> (deep-reverse x)
'((4 3) (2 1))
```

---
### Exercise 2.28

