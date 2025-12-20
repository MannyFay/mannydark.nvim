;;;; ============================================================================
;;;; Comprehensive Scheme Sample - Syntax Highlighting Demonstration
;;;; ============================================================================

;;;; This file demonstrates all major Scheme (R7RS) language features
;;;; for syntax highlighting purposes.

;;; ============================================================================
;;; Numeric Literals
;;; ============================================================================

;; Integer literals
(define integer-decimal 42)
(define integer-negative -17)
(define integer-hex #xFF)
(define integer-octal #o755)
(define integer-binary #b101010)

;; Exact vs inexact
(define exact-number #e3.14)
(define inexact-number #i22/7)

;; Rational numbers
(define ratio 22/7)
(define ratio-negative -3/4)

;; Floating point literals
(define float-simple 3.14159)
(define scientific 6.022e23)
(define scientific-negative 1.0e-10)

;; Complex numbers
(define complex-rect 3+4i)
(define complex-neg 3-4i)
(define complex-pure +2i)

;;; ============================================================================
;;; Boolean and Special Values
;;; ============================================================================

(define true-value #t)
(define false-value #f)
(define true-alt #true)
(define false-alt #false)

;;; ============================================================================
;;; Character Literals
;;; ============================================================================

(define char-simple #\a)
(define char-upper #\A)
(define char-space #\space)
(define char-newline #\newline)
(define char-tab #\tab)
(define char-return #\return)
(define char-lambda #\λ)
(define char-hex #\x03BB)

;;; ============================================================================
;;; String Literals
;;; ============================================================================

(define string-simple "Hello, Scheme!")
(define string-escaped "Line 1\nLine 2\tTabbed")
(define string-quotes "She said \"Hello\"")
(define string-unicode "Hello, 世界!")
(define string-continuation "This is a \
very long string that \
continues on multiple lines")

;;; ============================================================================
;;; Symbols and Keywords
;;; ============================================================================

(define symbol-simple 'hello)
(define symbol-with-special 'hello-world!)
(define symbol-question 'empty?)
(define symbol-bang 'set!)

;;; ============================================================================
;;; Pairs and Lists
;;; ============================================================================

;; Pairs (cons cells)
(define pair (cons 1 2))
(define dotted-pair '(a . b))

;; Lists
(define empty-list '())
(define simple-list '(1 2 3 4 5))
(define nested-list '((1 2) (3 4) (5 6)))
(define mixed-list '(1 "two" #\3 4.0))

;; List construction
(define constructed-list (list 1 2 3 4 5))
(define consed-list (cons 0 '(1 2 3)))

;; Quasiquote
(define x 10)
(define quasiquote-example `(1 2 ,x ,(+ x 1)))
(define splice-example `(0 ,@'(1 2 3) 4))
(define nested-quasi `(a `(b ,(+ 1 2) ,',x)))

;;; ============================================================================
;;; Vectors
;;; ============================================================================

(define vector-literal #(1 2 3 4 5))
(define vector-constructed (vector 1 2 3 4 5))
(define vector-make (make-vector 5 0))

;;; ============================================================================
;;; Bytevectors (R7RS)
;;; ============================================================================

(define bytevector-literal #u8(0 127 255))
(define bytevector-make (make-bytevector 4 0))

;;; ============================================================================
;;; Basic Functions
;;; ============================================================================

;; Simple function definition
(define (add a b)
  (+ a b))

;; Lambda expression
(define multiply
  (lambda (a b)
    (* a b)))

;; Function with documentation (implementation-dependent)
(define (factorial n)
  "Calculate factorial of n"
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

;; Tail-recursive version
(define (factorial-tail n)
  (define (iter n acc)
    (if (<= n 1)
        acc
        (iter (- n 1) (* n acc))))
  (iter n 1))

;; Multiple return values
(define (divide-and-remainder dividend divisor)
  (values (quotient dividend divisor)
          (remainder dividend divisor)))

;;; ============================================================================
;;; Variable Binding Forms
;;; ============================================================================

;; let - parallel binding
(define (let-example x)
  (let ((a (+ x 1))
        (b (+ x 2)))
    (+ a b)))

;; let* - sequential binding
(define (let*-example x)
  (let* ((a (+ x 1))
         (b (+ a 1))
         (c (+ b 1)))
    (list a b c)))

;; letrec - recursive binding
(define (letrec-example n)
  (letrec ((even? (lambda (n)
                    (if (= n 0) #t (odd? (- n 1)))))
           (odd? (lambda (n)
                   (if (= n 0) #f (even? (- n 1))))))
    (even? n)))

;; letrec* - sequential recursive binding (R7RS)
(define (letrec*-example)
  (letrec* ((a 1)
            (b (+ a 1))
            (c (+ b 1)))
    (list a b c)))

;; Named let for loops
(define (sum-to n)
  (let loop ((i n) (acc 0))
    (if (= i 0)
        acc
        (loop (- i 1) (+ acc i)))))

;; let-values for multiple values (R7RS)
(define (let-values-example)
  (let-values (((q r) (divide-and-remainder 17 5)))
    (list q r)))

;;; ============================================================================
;;; Control Flow
;;; ============================================================================

;; if expression
(define (classify-sign n)
  (if (< n 0)
      'negative
      (if (= n 0)
          'zero
          'positive)))

;; cond expression
(define (classify-number n)
  (cond
    ((< n 0) 'negative)
    ((= n 0) 'zero)
    ((< n 10) 'small)
    ((< n 100) 'medium)
    (else 'large)))

;; case expression
(define (day-type day)
  (case day
    ((monday tuesday wednesday thursday friday) 'weekday)
    ((saturday sunday) 'weekend)
    (else 'unknown)))

;; when and unless (R7RS)
(define (when-example x)
  (when (> x 0)
    (display "positive")
    (newline)))

(define (unless-example x)
  (unless (= x 0)
    (display "non-zero")
    (newline)))

;; and/or as control flow
(define (and-example x y)
  (and (> x 0)
       (> y 0)
       (+ x y)))

(define (or-example x y)
  (or (and (> x 0) x)
      (and (> y 0) y)
      0))

;;; ============================================================================
;;; Higher-Order Functions
;;; ============================================================================

;; Map
(define (my-map f lst)
  (if (null? lst)
      '()
      (cons (f (car lst))
            (my-map f (cdr lst)))))

;; Filter
(define (my-filter pred lst)
  (cond
    ((null? lst) '())
    ((pred (car lst))
     (cons (car lst) (my-filter pred (cdr lst))))
    (else (my-filter pred (cdr lst)))))

;; Fold (reduce)
(define (fold-left f init lst)
  (if (null? lst)
      init
      (fold-left f (f init (car lst)) (cdr lst))))

(define (fold-right f init lst)
  (if (null? lst)
      init
      (f (car lst) (fold-right f init (cdr lst)))))

;; Using higher-order functions
(define (sum-list lst)
  (fold-left + 0 lst))

(define (product-list lst)
  (fold-left * 1 lst))

;; Function composition
(define (compose f g)
  (lambda (x) (f (g x))))

;; Currying
(define (curry2 f)
  (lambda (x)
    (lambda (y)
      (f x y))))

(define (uncurry2 f)
  (lambda (x y)
    ((f x) y)))

;;; ============================================================================
;;; Closures and Lexical Scope
;;; ============================================================================

;; Counter with closure
(define (make-counter)
  (let ((count 0))
    (lambda ()
      (set! count (+ count 1))
      count)))

;; Bank account example
(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (get-balance)
    balance)
  (lambda (message . args)
    (case message
      ((withdraw) (apply withdraw args))
      ((deposit) (apply deposit args))
      ((balance) (get-balance))
      (else (error "Unknown message" message)))))

;;; ============================================================================
;;; Continuations
;;; ============================================================================

;; call/cc examples
(define (call/cc-example)
  (call-with-current-continuation
    (lambda (return)
      (for-each
        (lambda (x)
          (if (negative? x)
              (return x)))
        '(1 2 -3 4 5))
      #f)))

;; Escape continuation for early exit
(define (find-first pred lst)
  (call/cc
    (lambda (return)
      (for-each
        (lambda (x)
          (if (pred x)
              (return x)))
        lst)
      #f)))

;; Coroutines with continuations
(define (make-coroutine proc)
  (let ((saved-cont #f)
        (first-time #t))
    (lambda ()
      (call/cc
        (lambda (caller-cont)
          (if first-time
              (begin
                (set! first-time #f)
                (proc (lambda ()
                        (call/cc
                          (lambda (my-cont)
                            (set! saved-cont my-cont)
                            (caller-cont 'yield))))))
              (saved-cont 'resume)))))))

;;; ============================================================================
;;; Macros (syntax-rules)
;;; ============================================================================

;; Simple macro
(define-syntax my-when
  (syntax-rules ()
    ((my-when test body ...)
     (if test
         (begin body ...)))))

;; Multiple patterns
(define-syntax my-cond
  (syntax-rules (else)
    ((my-cond (else result ...))
     (begin result ...))
    ((my-cond (test result ...))
     (if test (begin result ...)))
    ((my-cond (test result ...) clause ...)
     (if test
         (begin result ...)
         (my-cond clause ...)))))

;; Macro with ellipsis
(define-syntax my-let
  (syntax-rules ()
    ((my-let ((var val) ...) body ...)
     ((lambda (var ...) body ...) val ...))))

;; Pattern with literals
(define-syntax my-case
  (syntax-rules (else)
    ((my-case expr (else result ...))
     (begin result ...))
    ((my-case expr ((datum ...) result ...) clause ...)
     (if (memv expr '(datum ...))
         (begin result ...)
         (my-case expr clause ...)))))

;; Anaphoric macro
(define-syntax aif
  (syntax-rules ()
    ((aif test then else)
     (let ((it test))
       (if it then else)))))

;;; ============================================================================
;;; Records (R7RS)
;;; ============================================================================

;; Define record type
(define-record-type <person>
  (make-person name age email)
  person?
  (name person-name)
  (age person-age person-set-age!)
  (email person-email))

;; Using records
(define (person-example)
  (let ((alice (make-person "Alice" 30 "alice@example.com")))
    (display (person-name alice))
    (newline)
    (person-set-age! alice 31)
    (person-age alice)))

;;; ============================================================================
;;; Exceptions (R7RS)
;;; ============================================================================

;; Guard expression
(define (safe-divide x y)
  (guard (exn
          ((eq? exn 'division-by-zero) 0)
          (else (raise exn)))
    (if (= y 0)
        (raise 'division-by-zero)
        (/ x y))))

;; With handler
(define (with-exception-handler-example)
  (with-exception-handler
    (lambda (exn)
      (display "Caught exception: ")
      (display exn)
      (newline))
    (lambda ()
      (raise "test exception"))))

;;; ============================================================================
;;; Dynamic Wind
;;; ============================================================================

(define (with-resource acquire release thunk)
  (let ((resource (acquire)))
    (dynamic-wind
      (lambda () #f)  ; before
      (lambda () (thunk resource))  ; thunk
      (lambda () (release resource)))))  ; after

;;; ============================================================================
;;; Parameters (R7RS)
;;; ============================================================================

(define current-debug (make-parameter #f))

(define (debug-print msg)
  (when (current-debug)
    (display "DEBUG: ")
    (display msg)
    (newline)))

(define (parameter-example)
  (parameterize ((current-debug #t))
    (debug-print "This will be printed")
    42))

;;; ============================================================================
;;; Promises and Delay/Force (R7RS)
;;; ============================================================================

;; Lazy evaluation
(define lazy-value (delay (begin (display "Computing...") (+ 1 2))))

(define (force-example)
  (display "Before force\n")
  (let ((result (force lazy-value)))
    (display "After force\n")
    result))

;; Lazy streams
(define (integers-from n)
  (delay (cons n (integers-from (+ n 1)))))

(define integers (integers-from 0))

(define (stream-car stream)
  (car (force stream)))

(define (stream-cdr stream)
  (cdr (force stream)))

(define (stream-take n stream)
  (if (= n 0)
      '()
      (cons (stream-car stream)
            (stream-take (- n 1) (stream-cdr stream)))))

;;; ============================================================================
;;; I/O
;;; ============================================================================

;; Output
(define (output-example)
  (display "Hello, World!")
  (newline)
  (write '(1 2 3))
  (newline)
  (write-char #\A)
  (newline))

;; Input
(define (input-example)
  (display "Enter a number: ")
  (let ((input (read)))
    (display "You entered: ")
    (write input)
    (newline)))

;; File I/O
(define (file-io-example)
  ;; Write to file
  (call-with-output-file "/tmp/test.txt"
    (lambda (port)
      (display "Hello, File!" port)
      (newline port)))

  ;; Read from file
  (call-with-input-file "/tmp/test.txt"
    (lambda (port)
      (read-line port))))

;; String ports
(define (string-port-example)
  (let ((out (open-output-string)))
    (display "Hello" out)
    (display ", " out)
    (display "World!" out)
    (get-output-string out)))

;;; ============================================================================
;;; List Processing Examples
;;; ============================================================================

;; Quicksort
(define (quicksort lst)
  (if (null? lst)
      '()
      (let* ((pivot (car lst))
             (rest (cdr lst))
             (smaller (filter (lambda (x) (<= x pivot)) rest))
             (larger (filter (lambda (x) (> x pivot)) rest)))
        (append (quicksort smaller)
                (list pivot)
                (quicksort larger)))))

;; Fibonacci
(define (fibonacci n)
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(define (fibonacci-iter n)
  (let loop ((i n) (a 0) (b 1))
    (if (= i 0)
        a
        (loop (- i 1) b (+ a b)))))

;; Tree operations
(define (tree-map f tree)
  (cond
    ((null? tree) '())
    ((pair? tree)
     (cons (tree-map f (car tree))
           (tree-map f (cdr tree))))
    (else (f tree))))

(define (tree-fold f init tree)
  (cond
    ((null? tree) init)
    ((pair? tree)
     (tree-fold f
                (tree-fold f init (car tree))
                (cdr tree)))
    (else (f tree init))))

;;; ============================================================================
;;; Module System (R7RS)
;;; ============================================================================

;; Library definition (would be in separate file)
;; (define-library (sample utils)
;;   (export factorial fibonacci)
;;   (import (scheme base))
;;   (begin
;;     (define (factorial n)
;;       (if (<= n 1) 1 (* n (factorial (- n 1)))))
;;     (define (fibonacci n)
;;       (if (< n 2) n (+ (fibonacci (- n 1)) (fibonacci (- n 2)))))))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(define (main)
  (display "=== Scheme Sample Program ===\n")

  ;; Basic functions
  (display "Factorial 10: ")
  (display (factorial-tail 10))
  (newline)

  (display "Fibonacci 20: ")
  (display (fibonacci-iter 20))
  (newline)

  (display "Quicksort: ")
  (write (quicksort '(3 1 4 1 5 9 2 6)))
  (newline)

  ;; Higher-order functions
  (display "Map double: ")
  (write (map (lambda (x) (* x 2)) '(1 2 3 4 5)))
  (newline)

  (display "Filter even: ")
  (write (filter even? '(1 2 3 4 5 6 7 8 9 10)))
  (newline)

  ;; Closures
  (let ((counter (make-counter)))
    (display "Counter: ")
    (display (counter))
    (display " ")
    (display (counter))
    (display " ")
    (display (counter))
    (newline))

  ;; Streams
  (display "First 10 integers: ")
  (write (stream-take 10 integers))
  (newline)

  (display "Done!\n"))

;; Run main
(main)
