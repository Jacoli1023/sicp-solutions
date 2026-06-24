# [2.5 Systems with Generic Operations](https://sarabander.github.io/sicp/html/2_002e5.xhtml#g_t2_002e5)

---
### Disclaimer

Coming back to this section, I've realized a few mistakes I've made or inadequacies I've produced that do not really follow the idea of data-additivity that the book is trying to produce. Previously, I was incorporating every new piece of functionality directly into the corresponding arithmetic packages, creating _fat objects_. While this made things seem more cohesive, it actually violated the modular principles that SICP was striving to instill in me.

Instead, I should be aiming for something more similar to _protocols_ or _type classes_ (like Haskell's type classes or Rust's traits) that define behavior externally. I will thus begin vastly reworking this section, which may produce some irregular-looking files. Either way, this is my learning journey so deal with it.

My planned file structure will now look like this:
```
2-5/
  table.rkt          ← make-table, get, put, operation-table (shared state)
  tags.rkt           ← attach-tag, type-tag, contents
  apply-generic.rkt  ← apply-generic, type-tower, raise, drop, find-type-level
  generics.rkt       ← add, sub, mul, div, equ?, raise, drop, square-root, etc.
  pkg-integer.rkt    ← integer package only
  pkg-rational.rkt   ← rational package only
  pkg-real.rkt       ← real package only
  pkg-rectangular.rkt
  pkg-polar.rkt
  pkg-complex.rkt    ← requires rectangular + polar
  install.rkt        ← (require) all packages, calls install functions, runs tests
```

This module structure will help me maintain the abstraction barriers that the book recommends, with each arithmetic package living in its own file as a module, and then generic arithmetic that operate on these data types also being implemented and stored within their own modules. This is made easy to perform with Dr. Racket's module functionality.

I will begin implementing these files as the exercises requires them. Here are the attached links: [table.rkt](./table.rkt), [tags.rkt](./tags.rkt), [apply-generic.rkt](./apply-generic.rkt), [generics.rkt](./generics.rkt), [pkg-integer.rkt](./pkg-integer.rkt), [pkg-rational.rkt](./pkg-rational.rkt), [pkg-real.rkt](./pkg-real.rkt), [pkg-rectangular.rkt](./pkg-rectangular.rkt), [pkg-polar.rkt](./pkg-polar.rkt), [pkg-complex.rkt](./pkg-complex.rkt), [install.rkt](./install.rkt). I will also rework any exercises that need to be following these changes.

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

Coming back to this, I want to refactor my earlier attempt. The previous version nested every helper inside `apply-generic`, used `(member #f ...)` as the "no coercion found" signal, and dropped the same-type guard I had in 2.81 — which leaves a path to infinite recursion when the table has no method for the given signature.

The cleaner shape is three top-level pieces, each doing one job. The coercion helpers are pure and reusable; only the dispatcher knows about the operation table:

```scheme
(define (coerce-arg target arg)
  (let ((source (type-tag arg)))
    (if (eq? source target)
        arg
        (let ((coerce (get-coercion source target)))
          (if coerce (coerce arg) false)))))

(define (coerce-args target args)
  (define (loop args acc)
    (if (null? args)
        (reverse acc)
        (let ((coerced (coerce-arg target (car args))))
          (if coerced
              (loop (cdr args) (cons coerced acc))
              false))))
  (loop args '()))

(define (multi-apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (define (try-targets candidates)
      (if (null? candidates)
          (error "No method for these types" (list op type-tags))
          (let* ((target (car candidates))
                 (coerced (coerce-args target args)))
            ;; only recurse if coercion changed the signature, else
            ;; (op T T ...) with no method would loop forever
            (if (and coerced
                     (not (equal? (map type-tag coerced) type-tags)))
                (apply multi-apply-generic op coerced)
                (try-targets (cdr candidates))))))
    (if proc
        (apply proc (map contents args))
        (try-targets type-tags))))
```

To actually exercise the dispatcher we need cross-type coercions in the coercion table. Rather than scattering `put-coercion` calls through each numeric package — which would couple `pkg-integer.rkt` to the existence of rationals and complexes — I put them in a new [`coercions.rkt`](./coercions.rkt) module and have `numeric-pkg` install it from [`install.rkt`](./install.rkt):

```scheme
(define (install-coercions)
  (define (scheme-number->rational n)
    (make-rational (contents n) 1))
  (define (scheme-number->complex n)
    (make-complex-from-real-imag (contents n) 0))
  (define (rational->complex r)
    (let ((nd (contents r)))
      (make-complex-from-real-imag (/ (car nd) (cdr nd)) 0)))

  (put-coercion 'scheme-number 'rational scheme-number->rational)
  (put-coercion 'scheme-number 'complex  scheme-number->complex)
  (put-coercion 'rational      'complex  rational->complex)
  'done)
```

Some tests:

```scheme
> (multi-apply-generic 'add 1 2)
3
> (multi-apply-generic 'add 5 (make-rational 3 4))
(rational 23 . 4)
> (multi-apply-generic 'add (make-rational 3 4) 5)
(rational 23 . 4)
> (multi-apply-generic 'add 5 (make-complex-from-real-imag 1 2))
(complex rectangular 6 . 2)
> (multi-apply-generic 'add (make-rational 1 2) (make-complex-from-real-imag 1 2))
(complex rectangular 3/2 . 2)
```

#### Where this strategy is not sufficiently general

Suppose the table holds a directly-installed mixed-type operation, say `(op rational complex)`, and we call `(op some-integer some-complex)`. The strategy tries to coerce both args to `integer` (fails: there is no `complex→integer`), then both to `complex` (succeeds via `integer→complex`), and dispatches to `(op complex complex)` — stepping right over the more specific `(op rational complex)` that needed only an `integer→rational` coercion. The strategy *insists* on collapsing every argument to a single type, so any directly-installed mixed-type method whose signature doesn't already match the input is invisible to it.

---
### Exercise 2.83

Solution:

The exercise asks for a tower of types — `integer -> rational -> real -> complex` — and a generic `raise` that climbs one rung. Following the same modular spirit as 2.82, I want the tower to be a *property of the system*, not something each numeric package carries around. Concretely that means three structural moves:

1. The existing `pkg-integer.rkt` was actually holding the scheme-number package (misnamed from earlier). I lifted that content into a new [`pkg-scheme-number.rkt`](./pkg-scheme-number.rkt) so it stays available unchanged, and let `pkg-integer.rkt` become the actual integer-tagged package. The two now coexist: bare numbers continue to default to `'scheme-number` (so `(add 1 2)` keeps working as before), and `(make-integer 5)` enters the tower explicitly.
2. Added a new [`pkg-real.rkt`](./pkg-real.rkt) for `'real`.
3. The `raise` procedures themselves live in a new [`raises.rkt`](./raises.rkt) module, parallel to how `coercions.rkt` is organized. None of `pkg-integer.rkt` / `pkg-rational.rkt` / `pkg-real.rkt` know anything about the rung above them.

#### Integer and real packages

```scheme
(define (install-integer-package)
  (define (tag x) (attach-tag 'integer x))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) (lambda (x y) (tag (quotient x y))))
  (put 'equ?   '(integer integer) =)
  (put '=zero? '(integer) zero?)
  (put 'make 'integer tag)
  'done)
```

Notice that `div` uses `quotient`, not `(/ ...)` with a fallback to `make-rational` as my earlier draft had. Conflating "divide two integers" with "promote the result up the tower" couples the integer package to the rational package; if a caller wants a fractional result, they should `raise` first. The real package mirrors this shape with `'real` tags and ordinary `/`.

#### The raise module

```scheme
(define (install-raises)
  (define (integer->rational n)
    (make-rational n 1))
  (define (rational->real r)
    (make-real (exact->inexact (/ (car r) (cdr r)))))
  (define (real->complex x)
    (make-complex-from-real-imag x 0))

  (put 'raise '(integer)  integer->rational)
  (put 'raise '(rational) rational->real)
  (put 'raise '(real)     real->complex)
  'done)
```

`apply-generic` strips the type tag before calling the proc, so each `raise` procedure receives just the contents and uses its sibling-package constructors to produce the next rung. Adding (or inserting) a new tower level is now a one-file edit to `raises.rkt`.

The generic dispatcher in [`generics.rkt`](./generics.rkt) is the one line you'd expect:

```scheme
(define (raise x) (apply-generic 'raise x))
```

#### How this relates to the book

The book frames `raise` as the elementary upward coercion that the next two exercises build on: 2.84 makes `apply-generic` raise mixed-type arguments along the tower to a common rung (a much more principled strategy than 2.82's "try every type as a target"), and 2.85 introduces `project` as `raise`'s inverse so results can be `drop`ped back down. So 2.83 is really laying down the *protocol*: every type below the top must declare "here is how I become the next type up." Putting that declaration in `raises.rkt` makes the tower a single, explicit object in the system, the same way `coercions.rkt` makes the coercion graph explicit.

Some tests:
```scheme
> (raise (make-integer 5))
(rational 5 . 1)
> (raise (make-rational 3 4))
(real . 0.75)
> (raise (make-real 1.2))
(complex rectangular 1.2 . 0)
> (raise (raise (make-integer 5)))
(real . 5.0)
> (raise (raise (raise (make-integer 5))))
(complex rectangular 5.0 . 0)
```

This is all easy architectural stuff though. Now we need to incorporate this logic into our `apply-generic` procedures!

---
### Exercise 2.84

Solution:

The exercise asks for two things at once: a tower-aware `apply-generic`, and a *way of testing which type is higher* that won't have to be rewritten every time a new rung is added. My earlier attempt put a `type-tower` list directly into the body of `apply-generic` and nested all of the helpers (`find-type-level`, `find-highest-type-level`, `raise-to`) as internal definitions — `find-highest-type-level` even closed over `args` from the enclosing scope, which made it impossible to reuse for `drop` in 2.85. It also inherited the same infinite-recursion trap as my 2.82 attempt: if no method is found and every argument is already at the top of the tower, it would re-raise to the same types and recurse forever.

The cleaner shape factors the tower out of the dispatcher entirely.

#### The tower module

The hierarchy is a system-wide property, not something `apply-generic` owns. It lives in a small leaf module, [`tower.rkt`](./tower.rkt), with no dependencies — so anything else (the dispatcher today, `drop` tomorrow) can use it freely.

```scheme
(define type-tower '(integer rational real complex))

(define (type-level type)
  (define (iter tower n)
    (cond ((null? tower) false)
          ((eq? type (car tower)) n)
          (else (iter (cdr tower) (+ n 1)))))
  (iter type-tower 0))

(define (highest-type types)
  (define (higher t1 t2)
    (if (> (type-level t1) (type-level t2)) t1 t2))
  (define (iter types highest)
    (if (null? types) highest
        (iter (cdr types) (higher (car types) highest))))
  (iter (cdr types) (car types)))
```

Inserting a new rung — say `bignum` between `integer` and `rational` — is now a one-symbol edit to `type-tower`. Nothing else changes.

#### The dispatcher

`apply-generic.rkt` gains two small top-level helpers and the new dispatcher. Notice that `raise-to` walks the operation table's `'raise` chain directly; it never needs to know the tower's structure, only that the chain eventually reaches the target type or runs out:

```scheme
(define (raise-to target arg)
  (let ((source (type-tag arg)))
    (if (eq? source target)
        arg
        (let ((raise-proc (get 'raise (list source))))
          (if raise-proc
              (raise-to target (raise-proc (contents arg)))
              false)))))

(define (raise-all-to target args)
  (define (iter args acc)
    (if (null? args)
        (reverse acc)
        (let ((raised (raise-to target (car args))))
          (if raised
              (iter (cdr args) (cons raised acc))
              false))))
  (iter args '()))

(define (tower-apply-generic op . args)
  (let* ((type-tags (map type-tag args))
         (proc (get op type-tags)))
    (if proc
        (apply proc (map contents args))
        (let* ((target (highest-type type-tags))
               (raised (raise-all-to target args)))
          ;; same guard as 2.82: only recurse if the signature changed,
          ;; else (op T T ...) with no method would loop forever
          (if (and raised
                   (not (equal? (map type-tag raised) type-tags)))
              (apply tower-apply-generic op raised)
              (error "No method for these types" (list op type-tags)))))))
```

The signature-changed guard is the fix for the infinite-recursion case. It mirrors the same trick used in `multi-apply-generic` for 2.82.

#### How this relates to the book

2.82 treated coercion as a flat graph: try every type as a target and see what sticks. 2.84 replaces that with the tower's *hierarchy*: pick the highest type among the arguments, raise everyone there, dispatch. This is fundamentally more principled because the tower encodes the *meaning* of the type relationships — `integer -> rational -> real -> complex` — instead of treating each conversion as an opaque entry. The book's hint — "do this in a manner that is compatible with the rest of the system and will not lead to problems in adding new levels to the tower" — is asking you to make the hierarchy a single explicit object that one edit can extend, rather than scattering "is X above Y?" knowledge into every dispatcher. Putting the tower in its own module (and walking the existing `'raise` table to climb it) does exactly that.

Some tests:
```scheme
> (tower-apply-generic 'add (make-integer 5) (make-rational 1 3))
(rational 16 . 3)
> (tower-apply-generic 'add (make-rational 1 3) (make-integer 5))
(rational 16 . 3)
> (tower-apply-generic 'add (make-integer 1) (make-real 2.5))
(real . 3.5)
> (tower-apply-generic 'add (make-rational 1 2) (make-complex-from-real-imag 1 2))
(complex rectangular 1.5 . 2)
> (tower-apply-generic 'add (make-integer 5) (make-integer 7))
(integer . 12)
```

---
### Exercise 2.85

Solution:

This was the messiest of the section the first time around, mostly because `drop` is a generic procedure that *uses* generic procedures, and a too-eager simplification rule turns the recursion into a loop. The rework cleans up three things at once: where the `project` declarations live, how to decide when to simplify, and how to layer `drop` so it doesn't fight the dispatcher that wraps it.

#### The projects module

`project` is the dual of `raise`, so its declarations live in [`projects.rkt`](./projects.rkt) — exactly parallel to `raises.rkt`. None of the source packages know how to project themselves; the projection chain is one explicit object you can extend in one place.

```scheme
(define (install-projects)
  (define (complex->real z)
    (make-real (real-part z)))
  (define (real->rational x)
    (let ((y (inexact->exact x)))
      (make-rational (numerator y) (denominator y))))
  (define (rational->integer r)
    (make-integer (quotient (car r) (cdr r))))

  (put 'project '(complex)  complex->real)
  (put 'project '(real)     real->rational)
  (put 'project '(rational) rational->integer)
  'done)
```

A matching `(define (project x) (apply-generic 'project x))` goes into [`generics.rkt`](./generics.rkt), next to `raise`.

#### `drop` and the simplifying dispatcher

`drop` and `simplifying-apply-generic` both live in [`apply-generic.rkt`](./apply-generic.rkt). Two key choices keep this honest:

1. **`drop` calls the basic `apply-generic` directly** for its internal `project` / `raise` / `equ?` lookups. That breaks the loop my earlier version stepped into: if `drop`'s internal `raise` went through the simplifying dispatcher, the dispatcher would call `drop` again, which would call `raise` again, and so on. By using the un-simplifying dispatcher inside `drop`, the recursion terminates at the value level instead of the dispatcher level.
2. **The wrapper drops only *tower-typed* results**, not based on the op name. Earlier I had `reduce` switching on `'add` / `'sub` / `'mul` / `'div` — brittle and op-by-op. The cleaner rule is structural: a result is droppable iff its type tag is in the tower. Booleans (from `equ?` / `=zero?`) aren't pairs, bare scheme-numbers have a non-tower tag, and complex internals (`'rectangular` / `'polar`) aren't in the tower either. One predicate, no enumeration:

```scheme
(define (drop x)
  (let ((type (type-tag x)))
    (cond ((not (type-level type)) x)
          ((= (type-level type) 0) x)
          (else
           (let ((projected (apply-generic 'project x)))
             (if (apply-generic 'equ? x (apply-generic 'raise projected))
                 (drop projected)
                 x))))))

(define (tower-value? x)
  (and (pair? x) (type-level (type-tag x))))

(define (simplifying-apply-generic op . args)
  (let ((result (apply tower-apply-generic op args)))
    (if (tower-value? result) (drop result) result)))
```

#### How this relates to the book

2.83 declared the tower, 2.84 used it to dispatch upward, and 2.85 closes the loop by collapsing back down. `project` and `raise` form an adjoint pair, so "can we simplify?" becomes a single structural question: does projecting and re-raising land you back where you started? The book is teaching that the simplification rule itself is *not operation-specific* — it is a property of the tower. The op-name `reduce` switch in my earlier draft hid that lesson; the `tower-value?` predicate exposes it. Architecturally, `projects.rkt` mirrors `raises.rkt` so the tower's two structural primitives sit side by side, and `drop` is layered *below* the simplifying dispatcher (it uses the basic `apply-generic`) so the dispatcher's wrap is non-reentrant.

Some tests:

```scheme
> (drop (make-complex-from-real-imag 4 0))
(integer . 4)
> (drop (make-complex-from-real-imag 1.5 0))
(rational 3 . 2)
> (drop (make-complex-from-real-imag 2 3))
(complex rectangular 2 . 3)
> (drop (make-real 5.0))
(integer . 5)

> (simplifying-apply-generic 'add (make-complex-from-real-imag 4 0) (make-integer 8))
(integer . 12)
> (simplifying-apply-generic 'sub (make-real 3.5) (make-real 1.5))
(integer . 2)
> (simplifying-apply-generic 'div (make-rational 3 2) (make-integer 9))
(rational 1 . 6)
> (simplifying-apply-generic 'add (make-rational 1 3) (make-rational 2 3))
(integer . 1)
```

#### Why `drop` never stops at `real`

A subtle but important consequence of this implementation: with the `project` / `raise` pair we wrote, `drop` will **never** return a `(real . _)` value. The real rung is, in practice, a pass-through.

The cause is that `real→rational` is implemented via `inexact->exact`, which preserves every bit of an IEEE 754 float:

```scheme
(inexact->exact 1.5)  ; => 3/2
(inexact->exact 0.1)  ; => 3602879701896397/36028797018963968
```

…and the matching `rational→real` raise is `(exact->inexact (/ numer denom))`. The round-trip is lossless by construction: the float's exact rational is, by definition, the exact value of *that* float, so re-converting rounds back to the same float. `equ?` always says yes, and `drop` always continues past real.

```scheme
> (drop (make-real 1.5))
(rational 3 . 2)
> (drop (make-real 0.1))
(rational 3602879701896397 . 36028797018963968)
> (drop (make-real 5.0))
(integer . 5)
```

So with this `project` / `raise` pair, the possible final destinations of `drop` are `integer`, `rational`, or `complex` (only when imag-part ≠ 0). The `real` rung still earns its keep elsewhere — it's where inexact arithmetic happens, where `(add 1.5 2.5)` lives — but `drop`'s output set just doesn't include it.

To make values like `0.1` actually stop at `real`, we'd need a *lossy* `real→rational` (e.g., one that rounds to a small-denominator rational), so the round-trip would fail `equ?` for fragile floats. That's an aesthetic call, not the book's algorithm; the book's criterion is exactly the lossless round-trip, so the algorithm — applied faithfully — agrees with us. The exercise's prose ("1.5 + 0i can be lowered as far as real") is just loose phrasing of an example.

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
