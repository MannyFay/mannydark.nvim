#lang racket/base

;;;; ============================================================================
;;;; Comprehensive Racket Sample - Syntax Highlighting Demonstration
;;;; ============================================================================

;;;; This file demonstrates all major Racket language features
;;;; for syntax highlighting purposes.

(require racket/match
         racket/class
         racket/contract
         racket/async-channel
         racket/string
         racket/list
         racket/function
         racket/promise
         racket/generator
         (for-syntax racket/base
                     syntax/parse))

;;; ============================================================================
;;; Numeric Literals
;;; ============================================================================

;; Integer literals
(define integer-decimal 42)
(define integer-negative -17)
(define integer-hex #xFF)
(define integer-octal #o755)
(define integer-binary #b101010)

;; Exactness
(define exact-number #e3.14)
(define inexact-number #i22/7)

;; Rational numbers
(define ratio 22/7)
(define ratio-negative -3/4)

;; Floating point
(define float-simple 3.14159)
(define float-scientific 6.022e23)
(define float-single 3.14f0)

;; Complex numbers
(define complex-rect 3+4i)
(define complex-neg 3-4i)
(define complex-pure +2i)

;; Extended numbers
(define infinity +inf.0)
(define neg-infinity -inf.0)
(define not-a-number +nan.0)

;;; ============================================================================
;;; Boolean and Special Values
;;; ============================================================================

(define true-value #t)
(define false-value #f)
(define true-alt #true)
(define false-alt #false)
(define void-value (void))

;;; ============================================================================
;;; Character Literals
;;; ============================================================================

(define char-simple #\a)
(define char-space #\space)
(define char-newline #\newline)
(define char-tab #\tab)
(define char-lambda #\λ)
(define char-unicode #\u03BB)
(define char-nul #\nul)

;;; ============================================================================
;;; String Literals
;;; ============================================================================

(define string-simple "Hello, Racket!")
(define string-escaped "Line 1\nLine 2\tTabbed")
(define string-unicode "Hello, 世界!")

;; Here strings
(define here-string #<<END
This is a here string.
It can contain "quotes" and 'special' characters.
No escaping needed.
END
  )

;; Byte strings
(define byte-string #"Hello")
(define byte-regex #rx"pattern")
(define byte-pregex #px"pattern")

;;; ============================================================================
;;; Regular Expressions
;;; ============================================================================

(define regex-simple #rx"[a-z]+")
(define regex-extended #px"\\w+\\s*=\\s*\\d+")
(define regex-case-insensitive #rx"(?i:hello)")

;;; ============================================================================
;;; Symbols and Keywords
;;; ============================================================================

(define symbol-simple 'hello)
(define symbol-pipe '|hello world|)
(define symbol-special 'λ)

;; Keywords (different from symbols)
(define keyword-example '#:keyword)
(define keyword-list '(#:name "Alice" #:age 30))

;;; ============================================================================
;;; Pairs, Lists, and Vectors
;;; ============================================================================

;; Pairs
(define pair (cons 1 2))
(define dotted-pair '(a . b))

;; Lists
(define empty-list '())
(define simple-list '(1 2 3 4 5))
(define nested-list '((1 2) (3 4)))

;; Mutable pairs
(define mutable-pair (mcons 1 2))

;; Vectors
(define vector-immutable #(1 2 3 4 5))
(define vector-mutable (vector 1 2 3))

;; Hash tables
(define hash-immutable #hash((a . 1) (b . 2)))
(define hash-mutable (make-hash '((a . 1) (b . 2))))
(define hasheq-example #hasheq((a . 1)))
(define hasheqv-example #hasheqv((1 . "one")))

;; Sets
(define set-example (set 1 2 3 4 5))
(define seteq-example (seteq 'a 'b 'c))

;; Boxes
(define box-example (box 42))

;;; ============================================================================
;;; Quasiquotation
;;; ============================================================================

(define x 10)
(define quasiquote-example `(1 2 ,x ,(+ x 1)))
(define splice-example `(0 ,@'(1 2 3) 4))
(define syntax-quasiquote #`(1 2 #,x))

;;; ============================================================================
;;; Structs
;;; ============================================================================

;; Basic struct
(struct point (x y) #:transparent)

;; Struct with options
(struct person (name age email)
  #:transparent
  #:mutable
  #:guard (λ (name age email type-name)
            (unless (string? name)
              (error type-name "name must be a string"))
            (values name age email)))

;; Struct inheritance
(struct employee person (department salary)
  #:transparent)

;; Prefab structs
(struct prefab-point (x y) #:prefab)

;; Struct with methods
(struct counter (value)
  #:transparent
  #:mutable
  #:methods gen:custom-write
  [(define (write-proc c port mode)
     (fprintf port "#<counter:~a>" (counter-value c)))])

;;; ============================================================================
;;; Classes and Objects
;;; ============================================================================

;; Class definition
(define animal%
  (class object%
    (super-new)
    (init-field [name "Unknown"])
    (field [age 0])

    (define/public (speak)
      (format "~a makes a sound" name))

    (define/public (birthday)
      (set! age (add1 age)))

    (define/public (get-age) age)))

;; Inheritance
(define dog%
  (class animal%
    (super-new)
    (init-field [breed "Mixed"])

    (define/override (speak)
      (format "~a says: Woof!" (get-field name this)))))

(define cat%
  (class animal%
    (super-new)
    (init-field [indoor #t])

    (define/override (speak)
      (format "~a says: Meow!" (get-field name this)))))

;; Mixin
(define trainable<%>
  (interface ()
    train
    perform-trick))

(define trained-dog%
  (class* dog% (trainable<%>)
    (super-new)
    (field [tricks '()])

    (define/public (train trick)
      (set! tricks (cons trick tricks)))

    (define/public (perform-trick)
      (if (null? tricks)
          "No tricks learned"
          (format "Performing: ~a" (car tricks))))))

;; Using objects
(define (class-example)
  (define rex (new dog% [name "Rex"] [breed "German Shepherd"]))
  (send rex speak))

;;; ============================================================================
;;; Contracts
;;; ============================================================================

;; Function with contract
(define/contract (safe-divide x y)
  (-> number? (and/c number? (not/c zero?)) number?)
  (/ x y))

;; Custom contract
(define positive-integer/c
  (and/c integer? positive?))

;; Contract for struct
(define/contract (make-validated-person name age)
  (-> string? positive-integer/c person?)
  (person name age #f))

;; Contract out
(provide
 (contract-out
  [safe-divide (-> number? (and/c number? (not/c zero?)) number?)]
  [struct point ([x real?] [y real?])]))

;;; ============================================================================
;;; Pattern Matching
;;; ============================================================================

(define (match-example value)
  (match value
    ;; Literal patterns
    [0 'zero]
    [1 'one]

    ;; Predicate patterns
    [(? negative?) 'negative]
    [(? positive? n) `(positive ,n)]

    ;; List patterns
    ['() 'empty-list]
    [(list a) `(singleton ,a)]
    [(list a b) `(pair ,a ,b)]
    [(list a b ...) `(list-with-rest ,a ,b)]
    [(cons h t) `(cons ,h ,t)]

    ;; Struct patterns
    [(point x y) `(point ,x ,y)]
    [(person name age _) `(person ,name ,age)]

    ;; Hash patterns
    [(hash-table ('name n) ('age a)) `(hash ,n ,a)]

    ;; Or patterns
    [(or 'yes 'y #t) 'affirmative]

    ;; And patterns
    [(and (? string?) (app string-length (? (curry < 10)))) 'short-string]

    ;; Quasiquote patterns
    [`(add ,a ,b) (+ a b)]
    [`(mul ,a ,b) (* a b)]

    ;; With guard
    [(? number? n) #:when (> n 100) 'large-number]

    ;; Default
    [_ 'unknown]))

;; Match-let
(define (match-let-example p)
  (match-let ([(point x y) p])
    (sqrt (+ (* x x) (* y y)))))

;; Match-define
(match-define (list a b c) '(1 2 3))

;;; ============================================================================
;;; Macros
;;; ============================================================================

;; Simple syntax-rules macro
(define-syntax my-when
  (syntax-rules ()
    [(_ test body ...)
     (if test
         (begin body ...)
         (void))]))

;; Syntax-case macro
(define-syntax (my-unless stx)
  (syntax-case stx ()
    [(_ test body ...)
     #'(if (not test)
           (begin body ...)
           (void))]))

;; Syntax-parse macro (modern style)
(define-syntax (define-struct-like stx)
  (syntax-parse stx
    [(_ name:id (field:id ...))
     #'(struct name (field ...) #:transparent)]))

;; Macro with syntax classes
(begin-for-syntax
  (define-syntax-class binding
    (pattern [name:id value:expr])))

(define-syntax (my-let stx)
  (syntax-parse stx
    [(_ (b:binding ...) body:expr ...+)
     #'((λ (b.name ...) body ...) b.value ...)]))

;; Phase 1 helper
(begin-for-syntax
  (define (generate-name base)
    (datum->syntax base (string->symbol
                         (format "~a-generated" (syntax-e base))))))

;;; ============================================================================
;;; Continuations
;;; ============================================================================

;; call/cc
(define (call/cc-example)
  (call/cc
   (λ (return)
     (for ([x '(1 2 -3 4 5)])
       (when (negative? x)
         (return x)))
     #f)))

;; Prompts and delimited continuations
(define (prompt-example)
  (+ 1 (call-with-continuation-prompt
        (λ ()
          (+ 2 (abort-current-continuation
                (default-continuation-prompt-tag)
                10))))))

;; Composable continuations
(define (composable-example)
  (call-with-composable-continuation
   (λ (k)
     (+ 1 (k 2)))))

;;; ============================================================================
;;; Concurrency
;;; ============================================================================

;; Threads
(define (thread-example)
  (define t
    (thread
     (λ ()
       (displayln "Hello from thread!")
       (sleep 1)
       (displayln "Thread done"))))
  (thread-wait t))

;; Channels
(define (channel-example)
  (define ch (make-channel))
  (thread (λ () (channel-put ch 42)))
  (channel-get ch))

;; Async channels
(define (async-channel-example)
  (define ach (make-async-channel))
  (async-channel-put ach 'message)
  (async-channel-get ach))

;; Sync and events
(define (sync-example)
  (define ch1 (make-channel))
  (define ch2 (make-channel))
  (thread (λ () (sleep 0.1) (channel-put ch1 'first)))
  (thread (λ () (sleep 0.2) (channel-put ch2 'second)))
  (sync ch1 ch2))

;; Semaphores
(define (semaphore-example)
  (define sem (make-semaphore 1))
  (call-with-semaphore sem
    (λ () (displayln "Critical section"))))

;; Futures
(define (future-example)
  (define f (future (λ () (+ 1 2 3 4 5))))
  (touch f))

;; Places (parallel computation)
;; (define (place-example)
;;   (define p
;;     (place ch
;;       (place-channel-put ch (+ 1 1))))
;;   (place-channel-get p))

;;; ============================================================================
;;; Generators and Sequences
;;; ============================================================================

;; Generator
(define (counter-generator)
  (generator ()
    (let loop ([n 0])
      (yield n)
      (loop (add1 n)))))

(define (generator-example)
  (define gen (counter-generator))
  (list (gen) (gen) (gen)))

;; Sequences
(define (sequence-example)
  (for/list ([x (in-range 10)])
    (* x x)))

;; Custom sequence
(define (in-fibonacci n)
  (make-do-sequence
   (λ ()
     (values
      (λ (pos) (car pos))
      (λ (pos) (cons (cadr pos) (+ (car pos) (cadr pos))))
      '(0 1)
      (λ (pos) (< (car pos) n))
      #f
      #f))))

;;; ============================================================================
;;; For Loops
;;; ============================================================================

(define (for-loop-examples)
  ;; Basic for
  (for ([i (in-range 5)])
    (displayln i))

  ;; for/list
  (define squares
    (for/list ([i (in-range 10)])
      (* i i)))

  ;; for/vector
  (define vec
    (for/vector ([i 5])
      (* i 2)))

  ;; for/hash
  (define h
    (for/hash ([k '(a b c)]
               [v '(1 2 3)])
      (values k v)))

  ;; for/fold
  (define sum
    (for/fold ([acc 0])
              ([i (in-range 100)])
      (+ acc i)))

  ;; for* (nested)
  (define pairs
    (for*/list ([i (in-range 3)]
                [j (in-range 3)])
      (cons i j)))

  ;; for with #:when and #:unless
  (define evens
    (for/list ([i (in-range 20)]
               #:when (even? i))
      i))

  ;; for with #:break
  (for/list ([i (in-naturals)]
             #:break (> i 10))
    i)

  (list squares vec h sum pairs evens))

;;; ============================================================================
;;; Units (Module System)
;;; ============================================================================

;; Signature
(define-signature arithmetic^
  (add subtract multiply divide))

;; Unit
(define arithmetic@
  (unit
   (import)
   (export arithmetic^)

   (define (add a b) (+ a b))
   (define (subtract a b) (- a b))
   (define (multiply a b) (* a b))
   (define (divide a b) (/ a b))))

;;; ============================================================================
;;; Lazy Evaluation
;;; ============================================================================

;; Promises
(define lazy-computation
  (delay
    (displayln "Computing...")
    (* 6 7)))

(define (lazy-example)
  (displayln "Before force")
  (define result (force lazy-computation))
  (displayln "After force")
  result)

;; Lazy language features (would use #lang lazy)
(define (stream-cons-example)
  (define ones (stream-cons 1 ones))
  (stream-first ones))

;;; ============================================================================
;;; Parameters
;;; ============================================================================

(define current-debug-mode (make-parameter #f))
(define current-output-port (make-parameter (current-output-port)))

(define (parameter-example)
  (parameterize ([current-debug-mode #t])
    (when (current-debug-mode)
      (displayln "Debug mode is on"))
    42))

;;; ============================================================================
;;; Exceptions
;;; ============================================================================

;; Custom exception
(struct exn:fail:my-error exn:fail (code)
  #:transparent)

(define (raise-my-error msg code)
  (raise (exn:fail:my-error msg (current-continuation-marks) code)))

;; With handlers
(define (exception-example)
  (with-handlers
    ([exn:fail:my-error?
      (λ (e) (format "Error ~a: ~a" (exn:fail:my-error-code e) (exn-message e)))]
     [exn:fail?
      (λ (e) (format "General error: ~a" (exn-message e)))])
    (raise-my-error "Something went wrong" 42)))

;; Dynamic wind
(define (dynamic-wind-example)
  (dynamic-wind
    (λ () (displayln "Setup"))
    (λ () (displayln "Body") 42)
    (λ () (displayln "Cleanup"))))

;;; ============================================================================
;;; Functional Programming Utilities
;;; ============================================================================

(define (fp-examples)
  ;; Curry
  (define add3 (curry + 3))

  ;; Compose
  (define add1-then-double (compose (curry * 2) add1))

  ;; Negate
  (define not-zero? (negate zero?))

  ;; Conjoin and disjoin
  (define positive-even? (conjoin positive? even?))
  (define zero-or-one? (disjoin zero? (curry = 1)))

  ;; Identity and const
  (define id identity)
  (define always-42 (const 42))

  ;; Thunk
  (define delayed (thunk (displayln "Hello")))

  (list (add3 5) (add1-then-double 10) (positive-even? 4)))

;;; ============================================================================
;;; Comprehensions (Racket style)
;;; ============================================================================

(define (comprehension-examples)
  ;; List comprehension style with for/list
  (for/list ([x (in-range 10)]
             #:when (odd? x))
    (* x x))

  ;; Parallel iteration
  (for/list ([x '(1 2 3)]
             [y '(a b c)])
    (list x y))

  ;; Nested with for*
  (for*/list ([x (in-range 3)]
              [y (in-range 3)]
              #:when (not (= x y)))
    (list x y)))

;;; ============================================================================
;;; Quicksort and Fibonacci
;;; ============================================================================

(define (quicksort lst)
  (match lst
    ['() '()]
    [(cons pivot rest)
     (define-values (smaller larger)
       (partition (λ (x) (<= x pivot)) rest))
     (append (quicksort smaller)
             (list pivot)
             (quicksort larger))]))

(define (fibonacci n)
  (match n
    [0 0]
    [1 1]
    [_ (+ (fibonacci (- n 1))
          (fibonacci (- n 2)))]))

(define (fibonacci-iter n)
  (for/fold ([a 0] [b 1])
            ([_ (in-range n)])
    (values b (+ a b))))

;;; ============================================================================
;;; Testing
;;; ============================================================================

(module+ test
  (require rackunit)

  (test-case "factorial"
    (check-equal? (factorial 0) 1)
    (check-equal? (factorial 5) 120))

  (test-case "fibonacci"
    (check-equal? (fibonacci 0) 0)
    (check-equal? (fibonacci 10) 55))

  (test-case "quicksort"
    (check-equal? (quicksort '()) '())
    (check-equal? (quicksort '(3 1 4 1 5)) '(1 1 3 4 5))))

;;; ============================================================================
;;; Main
;;; ============================================================================

(define (factorial n)
  (if (<= n 1)
      1
      (* n (factorial (- n 1)))))

(module+ main
  (displayln "=== Racket Sample Program ===")

  ;; Basic functions
  (printf "Factorial 10: ~a\n" (factorial 10))
  (printf "Fibonacci 20: ~a\n" (let-values ([(a b) (fibonacci-iter 20)]) a))
  (printf "Quicksort: ~a\n" (quicksort '(3 1 4 1 5 9 2 6)))

  ;; Structs
  (define p (point 3 4))
  (printf "Point: (~a, ~a)\n" (point-x p) (point-y p))

  ;; Classes
  (define dog (new dog% [name "Rex"] [breed "German Shepherd"]))
  (printf "Dog says: ~a\n" (send dog speak))

  ;; Pattern matching
  (printf "Match: ~a\n" (match-example 42))

  ;; For loops
  (printf "Squares: ~a\n" (for/list ([i 10]) (* i i)))

  ;; Higher-order
  (printf "Map: ~a\n" (map add1 '(1 2 3 4 5)))
  (printf "Filter: ~a\n" (filter even? '(1 2 3 4 5 6 7 8 9 10)))

  (displayln "Done!"))
