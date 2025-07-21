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

Solution:
```scheme
(define (fringe ls)
  (cond ((null? ls) ls)
        ((not (pair? ls))
         (list ls))
        (else
         (append (fringe (car ls)) (fringe (cdr ls))))))
```

Test:
```scheme
> (fringe x)
'(1 2 3 4)
> (fringe (list x x))
'(1 2 3 4 1 2 3 4)
```

---
### Exercise 2.29

Solution ([`binary-mobile.rkt`](./binary-mobile.rkt)):

1. selectors `left-branch`, `right-branch`, `branch-length`, and `branch-structure`:
```scheme
(define (left-branch mobile)
  (car mobile))
(define (right-branch mobile)
  (cadr mobile))

(define (branch-length branch)
  (car branch))
(define (branch-structure branch)
  (cadr branch))
```

2. `total-weight` procedure (`branch-weight` will also be used in the next part of this problem, hence why it is not an internal definition):
```scheme
(define (branch-weight branch)
  (let ((struct (branch-structure branch)))
    (if (number? struct)
        struct
        (total-weight struct))))

(define (total-weight mobile)
  (+ (branch-weight (left-branch mobile))
     (branch-weight (right-branch mobile))))
```

3. `balanced?` predicate procedure:
```scheme
(define (torque branch)
  (* (branch-length branch)
     (branch-weight branch)))

(define (balanced? mobile)
  (define (branch-balanced? branch)
    (let ((struct (branch-structure branch)))
      (or (number? struct)
          (balanced? struct))))
  (let ((left (left-branch mobile))
        (right (right-branch mobile)))
    (and (= (torque left) (torque right))
         (branch-balanced? left)
         (branch-balanced? right))))
```

4. changing the representation of mobiles as:
```scheme
(define (make-mobile left right)
  (cons left right))

(define (make-branch length structure)
  (cons length structure))
```
We would not have to change much if we were using `cons` to construct our mobiles as opposed to `list`. We would just have to change our `branch-structure` selector and our `right-branch` selector to be a `cdr`, as opposed to a `cadr`:
```scheme
(define (branch-structure branch)
  (cdr branch))

(define (right-branch mobile)
  (cdr mobile))
```

---
### Exercise 2.30

Solution ([`tree-operations.rkt`](./tree-operations.rkt)):

- `square-tree` without higher-order procedures:
```scheme
(define (square-tree tree)
  (cond ((null? tree) tree)
        ((not (pair? tree))
         (square tree))
        (else
         (cons (square-tree (car tree))
               (square-tree (cdr tree))))))
```

- `square-tree` using `map`:
```scheme
(define (square-tree tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (square-tree sub-tree)
             (square sub-tree)))
       tree))
```

Both return the same results for the test:
```scheme
> (square-tree
   (list 1
         (list 2 (list 3 4) 5)
         (list 6 7)))
'(1 (4 (9 16) 25) (36 49))
```

---
### Exercise 2.31

Solution ([`map.rkt`](./map.rkt)):

- direct definition:
```scheme
(define (tree-map proc tree)
  (cond ((null? tree) tree)
        ((not (pair? tree))
         (proc tree))
        (else
         (cons (tree-map proc (car tree))
               (tree-map proc (cdr tree))))))

(define (square-tree tree)
  (tree-map square tree))
```

- using `map`
```scheme
(define (tree-map proc tree)
  (map (lambda (sub-tree)
         (if (pair? sub-tree)
             (tree-map proc sub-tree)
             (proc sub-tree)))
       tree))

(define (square-tree tree)
  (tree-map square tree))
```

---
### Exercise 2.32

Solution ([`list-operations.rkt`](./list-operations.rkt)):
```scheme
(define (subsets s)
  (if (null? s)
      (list '())
      (let ((rest (subsets (cdr s))))
        (append rest 
                (map (lambda (x)
                       (cons (car s) x))
                     rest)))))
```

This problem is somewhat analogous to the `count-change` procedure from before, where we first find the powerset of a set including all but the first element, and union that with the same powerset but where each element is expanded with the first element.

This works in the procedure by first recursively `cdr`ing down the original set until we are left with an empty list ('(), which is actually a part of the powerset). We begin building our powerset by following a couple rules: first we include every element that was part of the returned list in the recursive call, and then we append unique elements by combining (`cons`ing) each of those previous elements with the next element in the sublist.

Thus with the list `'(1 2 3)`, we first `cdr` down the list until we reach the empty list `'()`. Then the powerset is built like so:
```scheme
; start with the empty list
(())

; for each element in the constructed list, expand it with
; an element not included in the powerset so far.
; this will end up being 3 because of recursive call unwinding,
; include each newly expanded element as a unique element to the list
(() (3))

; once again, expand each element with another element of the original set,
; this will be 2 due to recursive unwinding
(() (3) (2) (2 3))

; continue until you've exhausted each element of the original set
...
```

---
### Exercise 2.33

[`accumulate`](./sequence-operations.rkt):
```scheme
(define (accumulate op initial sequence)
  (if (null? sequence)
      initial
      (op (car sequence)
          (accumulate op initial (cdr sequence)))))
```

Solution:
```scheme
(define (map p sequence)
  (accumulate 
   (lambda (x y) (cons (p x) y))
   '()
   sequence))

(define (append seq1 seq2)
  (accumulate cons seq2 seq1))

(define (length sequence)
  (accumulate
   (lambda (x n) (+ n 1))
   0
   sequence))
```

---
### Exercise 2.34

Solution ([`horner.rkt`](./horner.rkt):
```scheme
(define (horner-eval x coefficient-sequence)
  (accumulate
   (lambda (this-coeff higher-terms)
     (+ (* higher-terms x) this-coeff))
   0
   coefficient-sequence))
```

Test:
```scheme
> (horner-eval 2 (list 1 3 0 5 0 1))
79
```

---
### Exercise 2.35

Solution ([`sequence-operations.rkt`](./sequence-operations.rkt)):
```scheme
(define (count-leaves t)
  (accumulate
   +
   0
   (map (lambda (x) 1)
        (enumerate-tree t))))
```

This was a rather roundabout way of counting the leaves of a tree, simply because the exercise forced us to use the `map` procedure. A much simpler way of performing this operation is as follows:
```scheme
(define (count-leaves t)
  (length (enumerate-tree t)))
```

Which intrinsically still uses the `accumulate` procedure.

Test:
```scheme
> (count-leaves (list (list 1 (list 2 3) 4) (list 5 6 7 (list 8 9))))
9
```

---
### Exercise 2.36

Solution:
```scheme
(define (accumulate-n op init seqs)
  (if (null? (car seqs))
      '()
      (cons (accumulate op init (map car seqs))
            (accumulate-n op init (map cdr seqs)))))
```

Test:
```scheme
> (define s '((1 2 3) (4 5 6) (7 8 9) (10 11 12)))
> (accumulate-n + 0 s)
'(22 26 30)
```

---
### Exercise 2.37

Solution ([`matrix.rkt`](./matrix.rkt)):
```scheme
(define (matrix-*-vector m v)
  (map (lambda (w) (dot-product w v)) m))

(define (transpose mat)
  (accumulate-n cons '() mat))

(define (matrix-*-matrix m n)
  (let ((cols (transpose n)))
    (map (lambda (m-vec)
           (matrix-*-vector cols m-vec)) 
         m)))
```

---
### Exercise 2.38

```scheme
(define (fold-left op initial sequence)
  (define (iter result rest)
    (if (null? rest)
        result
        (iter (op result (car rest))
              (cdr rest))))
  (iter initial sequence))

(define fold-right accumulate)
```

Solution:
```scheme
> (fold-right / 1 (list 1 2 3))
3/2
> (fold-left / 1 (list 1 2 3))
1/6
> (fold-right list '() (list 1 2 3))
'(1 (2 (3 ())))
> (fold-left list '() (list 1 2 3))
'(((() 1) 2) 3)
```

Since `fold-left` works in the opposite direction, it accumulates and applies operations on the elements of a list in the reverse order of the `fold-right`, or `accumulate`, procedure. To guarantee that the two procedures will produce the same values for any sequence, the `op` needs to satisfy the _commutative_ and the _associative_ properties.

---
### Exercise 2.39

Solution:

- using `fold-right` (traditional `accumulate`):
```scheme
(define (reverse sequence)
  (fold-right
   (lambda (x y) (append y (list x)))
   '()
   sequence))
```

test:
```scheme
> (reverse (list 1 2 3))
'(3 2 1)
```

- using `fold-left` (iterative, reversed `accumulate`):
```scheme
(define (reverse sequence)
  (fold-left
   (lambda (x y) (cons y x))
   '()
   sequence))
```

test:
```scheme
> (reverse (list 1 2 3))
'(3 2 1)
```

---
### Exercise 2.40

Solution ([`prime-sum-pairs.rkt`](./prime-sum-pairs.rkt)):
```scheme
(define (unique-pairs n)
  (flatmap (lambda (i) 
            (map (lambda (j) (list i j))
                 (enumerate-interval 1 (- i 1))))
            (enumerate-interval 1 n)))
```

Thus, the simplified version of `prime-sum-pairs` is:
```scheme
(define (prime-sum-pairs n)
  (map make-pair-sum (filter prime-sum? (unique-pairs n))))
```

Tests:
```scheme
> (prime-sum-pairs 5)
'((2 1 3) (3 2 5) (4 1 5) (4 3 7) (5 2 7))
```

---
### Exercise 2.41

Solution ([`triple-sums.rkt`](./triple-sums.rkts)):
```scheme
(define (unique-triples n)
  (flatmap (lambda (i)
             (flatmap (lambda (j)
                        (map (lambda (k) (list i j k))
                             (enumerate-interval 1 (- j 1))))
                      (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

(define (triple-sums n s)
  (filter (lambda (triple)
            (= (+ (car triple) (cadr triple) (caddr triple)) s))
          (unique-triples n)))
```

Test:
```scheme
> (triple-sums 8 10)
'((5 3 2) (5 4 1) (6 3 1) (7 2 1))
```

---
### Exercise 2.42

```scheme
(define (queens board-size)
  (define (queen-cols k)
    (if (= k 0)
        (list empty-board)
        (filter (lambda (positions) (safe? k positions))
                (flatmap
                 (lambda (rest-of-queens)
                   (map (lambda (new-row)
                          (adjoin-position new-row k rest-of-queens))
                        (enumerate-interval 1 board-size)))
                 (queen-cols (- k 1))))))
  (queen-cols board-size board-size))
```

Solution:

There are multiple parts to this exercise, so we'll break it down into smaller pieces. Starting with the easiest, yet most open-ended, portion, we need to determine how we're going to represent a chess board and the queens' positions. I've decided that a set of board positions will be represented as a list of lists, where the sublists represent the coordinates of each queen's position (along an x- and y-axis):
```scheme
(define (make-position row col)
  (list row col))
(define (get-row pos)
  (car pos))
(define (get-col pos)
  (cadr pos))
```

That naturally means that an empty board, a board without any queens in position, is simply just an empty list:
```scheme
(define empty-board '())
```

Now, to adjoin a new row-column position to the board, to make things easy to work with, we will just simply create the new position - using our `make-position` constructor - and put that new position at the front of the list using `cons`:
```scheme
(define (adjoin-position row col board)
  (cons (make-position row col) board))
```

This makes things simpler for us as we go through the list of positions since the newest queen we wish to add will always be at the front of the list of positions. Then we can check if that new queens is put in check by any of the previous queens already on the board.

That leads us to the next, and most difficult portion of the problem - the logic behind if one queen is put in check by any other. We use this procedure to check if the newly added queen is in a safe position on the board. My solution to this is a bit ugly, went a little too ham on the internal definitions and variables, but screw it we ball:
```scheme
(define (safe? k positions)
  (define (attacks? queen1 queen2)
    (let ((row1 (get-row queen1))
          (col1 (get-col queen1))
          (row2 (get-row queen2))
          (col2 (get-col queen2)))
      (or (= row1 row2) ; same row
          (= col1 col2) ; same col
          (= (+ row1 col1)
             (+ row2 col2)) ; same upwards diag
          (= (- row1 col1)
             (- row2 col2))))) ; same downwards diag
  (let ((k-queen (car positions)))
    (define (helper rest-of-queens)
      (cond ((null? rest-of-queens) #t)
            ((attacks? k-queen (car rest-of-queens)) #f)
            (else (helper (cdr rest-of-queens)))))
    (helper (cdr positions))))
```

The main two pieces of logic behind this mega-procedure is checking if the new queen (defined as `k-queen` in the `let` body) is "attacking" or put in check by the queen in the next column over. If not, it checks the next queen, and the queen after, etc. until we reach the end of the list of positions. Once we reach the end of the board and the new queen has not been put in check by any of the previous queens, we know that this new queen is safe. 

The majority of this logic comes from the `attacks?` predicate procedure. However, using some simple arithmetic, we can deduce if two given queens are in the same column, the same row, or on the same diagonal.

To make things prettier, I think it would be better if I brought the internal `attacks?` procedure to the top level of the program, and if I got rid of the `helper` procedure. To get rid of the `helper` procedure, I would need to rework the logic for traversing down the set of positions, so that instead of needing to define the `k-queen` in the let, I think I would just create a `remove` procedure which removes the given queen's position from the list of positions, using the `filter` procedure in our [sequence operations module](./sequence-operations.rkt).

Also, as it stands, my `safe?` procedure does not use the passed in `k` parameter at all. I think this would be used if our `adjoin-position` stuck our new queen any where else in the list. However, since we know our new queen is always at the front, we have no need for the `k` variable to determine where our queen is. I left it in the argument list anyway, however, since that's what the original exercise uses.

Anyway, without further exposition, here we can check one of the solutions given to us when we test our procedure on a 4x4 and an 8x8 board size:
```scheme
> (car (queens 4))
'((3 4) (1 3) (4 2) (2 1))
> (car (queens 8))
'((4 8) (2 7) (7 6) (3 5) (6 4) (8 3) (5 2) (1 1))
```

To visualize this on a chess board would allow our monkey-brains to better understand if the procedure is working or not:

- 4x4 chess board:
```
'((3 4) (1 3) (4 2) (2 1))
|---|---|---|---|
|   |   | X |   |
|---|---|---|---|
| X |   |   |   |
|---|---|---|---|
|   |   |   | X |
|---|---|---|---|
|   | X |   |   |
|---|---|---|---|
```

- 8x8 chess board:
```
'((4 8) (2 7) (7 6) (3 5) (6 4) (8 3) (5 2) (1 1))
|---|---|---|---|---|---|---|---|
| X |   |   |   |   |   |   |   |
|---|---|---|---|---|---|---|---|
|   |   |   |   |   |   | X |   |
|---|---|---|---|---|---|---|---|
|   |   |   |   | X |   |   |   |
|---|---|---|---|---|---|---|---|
|   |   |   |   |   |   |   | X |
|---|---|---|---|---|---|---|---|
|   | X |   |   |   |   |   |   |
|---|---|---|---|---|---|---|---|
|   |   |   | X |   |   |   |   |
|---|---|---|---|---|---|---|---|
|   |   |   |   |   | X |   |   |
|---|---|---|---|---|---|---|---|
|   |   | X |   |   |   |   |   |
|---|---|---|---|---|---|---|---|
```

We can thus see that no queens put each other in check. We can also check to see if our procedure is correct for _every_ solution it creates by enumerating through our procedure with the interval [0, 8], and checking to see if the number of our solutions matches that of the [A000170's](https://oeis.org/A000170) number of solutions:
```scheme
> (map (lambda (n) (length (queens n))) (enumerate-interval 0 8))
'(1 1 0 0 2 10 4 40 92)
```

And indeed it does!

---
### Exercise 2.43

```scheme
(flatmap
 (lambda (new-row)
   (map (lambda (rest-of-queens)
          (adjoin-position 
           new-row k rest-of-queens))
        (queen-cols (- k 1))))
 (enumerate-interval 1 board-size))
```

Solution:\
The reason that this solution takes so much longer is because for _every single new row added_ (when returning our `enumerate-interval` result), the procedure runs through the `queen-cols` procedure all over again, in its entirety. Our original procedure, only has to run `queen-cols` _n_ times, not including the base case.

Our original run-time is $O(n^2)$, yet Louis's run-time is gonna be more like $O(n^n)$. If our run-time takes _T_ time, then his will take about _T^n_ time.

---

For the rest of the solutions in the chapter - those that concern the picture language example in the book - I will be using the [SICP picture langauge](https://docs.racket-lang.org/sicp-manual/SICP_Picture_Language.html) included within the Racket language library. This language works slightly differently than the examples in the book, mainly through explicitly calling the `paint` procedure in order to print the picture. But since we are still implementing painter operations as Scheme procedures, then the solutions will be no different than those that are expected from the book.

Of course, since I cannot easily show my test results to prove that these procedures return what's expected, you'll either have to test them out yourself or just trust me! :)

---
### Exercise 2.44

Solution ([`pict-split.rkt`](./pict-split.rkt)):
```scheme
(define (up-split painter n)
  (if (= n 0)
      painter
      (let ((smaller (up-split painter (- n 1))))
        (below painter (beside smaller smaller)))))
```

---
### Exercise 2.45

```scheme
(define right-split (split beside below))
(define up-split (split below beside))
```

Solution:
```scheme
(define (split op op-split)
  (define (splitter painter n)
    (if (= n 0)
        painter
        (let ((smaller (splitter painter (- n 1))))
          (op painter (op-split smaller smaller)))))
  splitter)
```
