#lang sicp
(#%provide (all-defined))

;; --------------------------------------------------------------------
;; Table operations
;; --------------------------------------------------------------------

(define (make-table)
  (let ((local-table (list '*table*)))
    (define (lookup key-1 key-2)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record (cdr record) #f))
            #f)))
    (define (insert! key-1 key-2 value)
      (let ((subtable (assoc key-1 (cdr local-table))))
        (if subtable
            (let ((record (assoc key-2 (cdr subtable))))
              (if record
                  (set-cdr! record value)
                  (set-cdr! subtable
                            (cons (cons key-2 value)
                                  (cdr subtable)))))
            (set-cdr! local-table
                      (cons (list key-1 (cons key-2 value))
                            (cdr local-table))))))
    (define (reset!)
      (set-cdr! local-table '()))
    (define (dispatch m)
      (cond ((eq? m 'lookup-proc) lookup)
            ((eq? m 'insert-proc!) insert!)
            ((eq? m 'reset-proc!) reset!)
            (else (error 'make-table "unknown operation" m))))
    dispatch))

(define operation-table (make-table))
(define get (operation-table 'lookup-proc))
(define put (operation-table 'insert-proc!))
(define reset (operation-table 'reset-proc!))

(define (square x) (* x x))

(define (attach-tag type-tag contents)
  (if (eq? type-tag 'integer)
    contents
    (cons type-tag contents)))

(define (type-tag datum)
  (cond ((pair? datum) (car datum))
        ((number? datum) 'integer)
        (else (error 'type-tag "bad tagged datum" datum))))

(define (contents datum)
  (cond ((pair? datum) (cdr datum))
        ((number? datum) datum)
        (else (error 'contents "bad tagged datum" datum))))

;; --------------------------------------------------------------------
;; Apply-generic
;; --------------------------------------------------------------------

(define type-tower
  '(integer rational real complex))

; helper function to find the numeric level of the data type within
; the given tower hierarchy
(define (find-type-level type)
  (define (iter tower n)
    (cond ((null? tower) #f)
          ((eq? type (car tower)) n)
          (else (iter (cdr tower) (+ n 1)))))
  (iter type-tower 0))

(define (apply-generic op . args)

  (define (reduce x)
    (cond ((eq? op 'add) (drop x))
          ((eq? op 'sub) (drop x))
          ((eq? op 'mul) (drop x))
          ((eq? op 'div) (drop x))
          (else x)))

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
        (reduce (apply proc (map contents args)))
        (let ((target-type-level (find-highest-type-level)))
          (apply apply-generic op (map (lambda (arg)
                                         (raise-to arg target-type-level))
                                       args))))))

;; --------------------------------------------------------------------
;; Complex number package
;; --------------------------------------------------------------------

(define (complex-pkg)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))
  ;; internal procedures
  (define (add-complex z1 z2)
    (make-from-real-imag (+ (real-part z1) (real-part z2))
                         (+ (imag-part z1) (imag-part z2))))
  (define (sub-complex z1 z2)
    (make-from-real-imag (- (real-part z1) (real-part z2))
                         (- (imag-part z1) (imag-part z2))))
  (define (mul-complex z1 z2)
    (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                       (+ (angle z1) (angle z2))))
  (define (div-complex z1 z2)
    (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                       (- (angle z1) (angle z2))))
  ;; interface to rest of the system
  (define (tag z) (attach-tag 'complex z))
  (define (complex->real z)
    (make-real (real-part z)))
  (complex-comp-pkg)
  (put 'add '(complex complex)
       (lambda (z1 z2) (tag (add-complex z1 z2))))
  (put 'sub '(complex complex)
       (lambda (z1 z2) (tag (sub-complex z1 z2))))
  (put 'mul '(complex complex)
       (lambda (z1 z2) (tag (mul-complex z1 z2))))
  (put 'div '(complex complex)
       (lambda (z1 z2) (tag (div-complex z1 z2))))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  (put 'equ? '(complex complex)
       (lambda (z1 z2)
         (and (= (real-part z1) (real-part z2))
              (= (imag-part z1) (imag-part z2)))))
  (put '=zero? '(complex)
       (lambda (z)
         (and (zero? (real-part z))
              (zero? (imag-part z)))))
  (put 'project '(complex) complex->real)
  'done)

(define (complex-comp-pkg)
  (put 'real-part '(complex) real-part)
  (put 'imag-part '(complex) imag-part)
  (put 'magnitude '(complex) magnitude)
  (put 'angle '(complex) angle))

;; --------------------------------------------------------------------
;; Scheme number package
;; --------------------------------------------------------------------

(define (scheme-number-pkg)

  ;; internal procedures
  (define (scheme-number->rational n) (make-rational (contents n) 1))
  (define (scheme-number->complex n) (make-complex-from-real-imag (contents n) 0))
  (define (tag x)
    (attach-tag 'scheme-number x))

  ;; interface to system
  (put 'add '(scheme-number scheme-number)
       (lambda (x y) (tag (+ x y))))
  (put 'sub '(scheme-number scheme-number)
       (lambda (x y) (tag (- x y))))
  (put 'mul '(scheme-number scheme-number)
       (lambda (x y) (tag (* x y))))
  (put 'div '(scheme-number scheme-number)
       (lambda (x y) (tag (/ x y))))
  (put 'make 'scheme-number
       (lambda (x) (tag x)))
  (put 'equ? '(scheme-number scheme-number) =)
  (put '=zero? '(scheme-number) zero?)
  'done)

(define (integer-pkg)
  (define (tag x) (attach-tag 'integer x))
  (define (integer->rational n)
    (make-rational n 1))
  (put 'add '(integer integer) (lambda (x y) (tag (+ x y))))
  (put 'sub '(integer integer) (lambda (x y) (tag (- x y))))
  (put 'mul '(integer integer) (lambda (x y) (tag (* x y))))
  (put 'div '(integer integer) 
       (lambda (x y)
         (let ((z (/ x y)))
           (if (integer? z) (tag z) (make-rational x y)))))
  (put 'equ? '(integer integer) =)
  (put 'make 'integer tag)
  (put 'raise '(integer) integer->rational)
  'done)

(define (real-pkg)
  (define (real->complex n)
    (make-complex-from-real-imag n 0))
  (define (real->rational x)
    (let ((y (inexact->exact x)))
      (make-rational (numerator y) (denominator y))))
  (define (tag x) (attach-tag 'real x))
  (put 'add '(real real) (lambda (x y) (tag (+ x y))))
  (put 'sub '(real real) (lambda (x y) (tag (- x y))))
  (put 'mul '(real real) (lambda (x y) (tag (* x y))))
  (put 'div '(real real) (lambda (x y) (tag (/ x y))))
  (put 'equ? '(real real) =)
  (put 'make 'real tag)
  (put 'raise '(real) real->complex)
  (put 'project '(real) real->rational)
  'done)

;; --------------------------------------------------------------------
;; Rectangular & Polar number packages
;; --------------------------------------------------------------------

(define (rectangular-pkg)
  ;; Internal procedures
  (define real-part car)
  (define imag-part cdr)
  (define make-from-real-imag cons)
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

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
    (* (magnitude z) (cos (angle z))))
  (define (imag-part z)
    (* (magnitude z) (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))

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

;; --------------------------------------------------------------------
;; Rational number package
;; --------------------------------------------------------------------

(define (rational-pkg)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let ((g (gcd n d)))
      (cons (/ n g) (/ d g))))
  (define (add-rat x y)
    (make-rat (+ (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (sub-rat x y)
    (make-rat (- (* (numer x) (denom y)) (* (numer y) (denom x)))
              (* (denom x) (denom y))))
  (define (mul-rat x y)
    (make-rat (* (numer x) (numer y)) (* (denom x) (denom y))))
  (define (div-rat x y)
    (make-rat (* (numer x) (denom y)) (* (denom x) (numer y))))
  (define (rational->complex r)
    (let ((untagged (contents r)))
      (make-complex-from-real-imag (/ (numer untagged) (denom untagged))
                                   0)))
  (define (rational->real n)
    (make-real (exact->inexact (/ (numer n) (denom n)))))
  (define (rational->integer x)
    (make-integer (quotient (numer x) (denom x))))
  
  ;; interface to rest of the system
  (define (tag x) (attach-tag 'rational x))
  (put 'add '(rational rational)
       (lambda (x y) (tag (add-rat x y))))
  (put 'sub '(rational rational)
       (lambda (x y) (tag (sub-rat x y))))
  (put 'mul '(rational rational)
       (lambda (x y) (tag (mul-rat x y))))
  (put 'div '(rational rational)
       (lambda (x y) (tag (div-rat x y))))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  (put 'equ? '(rational rational)
       (lambda (x y)
         (and (= (numer x) (numer y))
              (= (denom x) (denom y)))))
  (put '=zero? '(rational)
       (lambda (x) (zero? (numer x))))
  (put 'raise '(rational) rational->real)
  (put 'project '(rational) rational->integer)
  'done)

;; --------------------------------------------------------------------
;; Generic procedures
;; --------------------------------------------------------------------

(define (add x y) (apply-generic 'add x y))
(define (sub x y) (apply-generic 'sub x y))
(define (mul x y) (apply-generic 'mul x y))
(define (div x y) (apply-generic 'div x y))
(define (equ? x y) (apply-generic 'equ? x y))
(define (=zero? n) (apply-generic '=zero? n))

(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

(define (make-scheme-number n)
  ((get 'make 'scheme-number) n))
(define (make-integer n)
  ((get 'make 'integer) n))
(define (make-real n)
  ((get 'make 'real) n))
(define (make-rational n d)
  ((get 'make 'rational) n d))
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

(define (raise x)
  (apply-generic 'raise x))
(define (project x)
  (apply-generic 'project x))

(define (drop x)
  (let ((type (type-tag x)))
    (if (= (find-type-level type) 0)
        x
        (let* ((projected (project x))
               (raised (raise projected)))
          (if (equ? x raised)
              (drop projected)
              x)))))

;; --------------------------------------------------------------------
;; EZ Package installation
;; --------------------------------------------------------------------

(define (install-num-pkgs)
  (integer-pkg)
  (real-pkg)
  (rational-pkg)
  (polar-pkg)
  (rectangular-pkg)
  (complex-pkg))

;; --------------------------------------------------------------------
;; Testing zone
;; --------------------------------------------------------------------
(install-num-pkgs)
(define int_n (make-integer 1))
(define rat_n (make-rational 2 3))
(define real_n (make-real 5.6))
(define complex_n (make-complex-from-real-imag 7 8))
