#lang sicp
(#%require "install.rkt")
(using numeric-pkg)

;; base functionality of packages
;; EO: expected output
(display "Base functionality")
(newline)
(add (make-scheme-number 1) (make-scheme-number 2)) ; EO: (scheme-number . 3)
(sub (make-rational 3 4) (make-rational 1 2)) ; EO: (rational 1 . 4)
(mul (make-rational 1 2) (make-rational 1 3)) ; EO: (rational 1 . 6)
(add (make-complex-from-mag-ang 1 0) (make-complex-from-real-imag 1 1))
;; EO: (complex rectangular (integer . 2) integer . 1)
;; Components are tagged after 2.86 -- the polar real-part path is now
;; generic, so cosine(0) returns (integer . 1) instead of the raw 1.
(newline)

;; following 2.78, scheme numbers are just integers
(display "EX 2.78")
(newline)
(add 1 2) ; EO: 3
(sub 3 4) ; EO: -1
(mul 5 6) ; EO: 30
(div 8 4) ; EO: 2
(newline)

;; 2.79, equ? tests
(display "EX 2.79")
(newline)
(equ? 1 1) ; EO: #t
(equ? 1 2) ; EO: #f
(equ? (make-rational 1 2) (make-rational 1 2)) ; EO: #t
(equ? (make-rational 1 2) (make-rational 3 4)) ; EO: #f
(equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 1 2)) ; EO: #t
(equ? (make-complex-from-real-imag 1 2) (make-complex-from-real-imag 3 4)) ; EO: #f
(newline)

;; 2.80, =zero? tests
(display "EX 2.80")
(newline)
(=zero? 0) ; EO: #t
(=zero? (make-scheme-number 0)) ; EO: #t
(=zero? 1) ; EO: #f
(=zero? (make-rational 0 1)) ; EO: #t
(=zero? (make-rational 1 1)) ; EO: #f
(=zero? (make-complex-from-real-imag 0 0)) ; EO: #t
(=zero? (make-complex-from-real-imag 0 1)) ; EO: #f
(newline)

;; 2.83, raise climbs one rung of the tower
(display "EX 2.83")
(newline)
(raise (make-integer 5))      ; EO: (rational 5 . 1)
(raise (make-rational 3 4))   ; EO: (real . 0.75)
(raise (make-real 1.2))       ; EO: (complex rectangular 1.2 . 0)
(raise (raise (make-integer 5)))           ; EO: (real . 5.0)
(raise (raise (raise (make-integer 5))))   ; EO: (complex rectangular 5.0 . 0)
(newline)

;; 2.84, tower-apply-generic raises mixed args to a common rung
(display "EX 2.84")
(newline)
(tower-apply-generic 'add (make-integer 5) (make-rational 1 3))   ; EO: (rational 16 . 3)
(tower-apply-generic 'add (make-rational 1 3) (make-integer 5))   ; EO: (rational 16 . 3)
(tower-apply-generic 'add (make-integer 1) (make-real 2.5))       ; EO: (real . 3.5)
(tower-apply-generic 'add (make-rational 1 2) (make-complex-from-real-imag 1 2))
;; EO: (complex rectangular 1.5 . 2)
(tower-apply-generic 'add (make-integer 5) (make-integer 7))      ; EO: (integer . 12)
(newline)

;; 2.85, drop lowers a value as far as it can go
(display "EX 2.85 / drop")
(newline)
(drop (make-complex-from-real-imag 4 0))      ; EO: (integer . 4)
(drop (make-complex-from-real-imag 1.5 0))    ; EO: (rational 3 . 2)  [book says real; 1.5 = 3/2 exactly]
(drop (make-complex-from-real-imag 2 3))      ; EO: (complex rectangular 2 . 3)
(drop (make-rational 4 1))                    ; EO: (integer . 4)
(drop (make-rational 3 2))                    ; EO: (rational 3 . 2)
(drop (make-real 5.0))                        ; EO: (integer . 5)
(newline)

;; 2.85, simplifying-apply-generic does arithmetic then simplifies
(display "EX 2.85 / simplifying-apply-generic")
(newline)
(simplifying-apply-generic 'add (make-complex-from-real-imag 4 0) (make-integer 8))
;; EO: (integer . 12)
(simplifying-apply-generic 'sub (make-real 3.5) (make-real 1.5))
;; EO: (integer . 2)
(simplifying-apply-generic 'div (make-rational 3 2) (make-integer 9))
;; EO: (rational 1 . 6)
(simplifying-apply-generic 'add (make-rational 1 3) (make-rational 2 3))
;; EO: (integer . 1)
(newline)

;; 2.86, generic transcendentals work on any tower type
(display "EX 2.86 / generic transcendentals")
(newline)
(square-root (make-integer 9))   ; EO: (integer . 3)
(sine (make-integer 0))          ; EO: (integer . 0)
(cosine (make-integer 0))        ; EO: (integer . 1)
(square-root (make-real 2.0))    ; EO: rational approx of sqrt(2)
(arctan (make-integer 1) (make-integer 1)) ; EO: rational approx of pi/4
(newline)

;; 2.86, complex numbers with heterogeneous tagged components
(display "EX 2.86 / heterogeneous complex")
(newline)
(define z1 (make-complex-from-real-imag (make-rational 1 2) (make-integer 1)))
(define z2 (make-complex-from-real-imag (make-integer 3) (make-rational 1 4)))
z1
;; EO: (complex rectangular (rational 1 . 2) integer . 1)
(add z1 z2)
;; EO: (complex rectangular (rational 7 . 2) rational 5 . 4)
(equ? z1 z1) ; EO: #t
(equ? z1 z2) ; EO: #f
(drop (make-complex-from-real-imag (make-rational 3 2) (make-integer 0)))
;; EO: (rational 3 . 2) -- complex with imag=0 and tagged real-part
(newline)
