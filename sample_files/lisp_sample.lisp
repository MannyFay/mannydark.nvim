;;;; ============================================================================
;;;; Comprehensive Common Lisp Sample - Syntax Highlighting Demonstration
;;;; ============================================================================

;;;; This file demonstrates all major Common Lisp language features
;;;; for syntax highlighting purposes.

;;; Package definition
(defpackage #:sample
  (:use #:cl)
  (:export #:main
           #:factorial
           #:fibonacci
           #:person
           #:make-person))

(in-package #:sample)

;;; ============================================================================
;;; Numeric Literals
;;; ============================================================================

;; Integer literals
(defparameter *integer-decimal* 42)
(defparameter *integer-negative* -17)
(defparameter *integer-hex* #xFF)
(defparameter *integer-octal* #o755)
(defparameter *integer-binary* #b101010)
(defparameter *integer-radix* #36rHELLO)  ; Base-36

;; Ratio (rational numbers)
(defparameter *ratio* 22/7)
(defparameter *ratio-negative* -3/4)

;; Floating point literals
(defparameter *float-single* 3.14159s0)
(defparameter *float-double* 2.71828d0)
(defparameter *float-long* 1.41421356l0)
(defparameter *float-short* 1.0e0)
(defparameter *scientific* 6.022d23)

;; Complex numbers
(defparameter *complex-rect* #C(3 4))
(defparameter *complex-float* #C(1.0 2.0))

;;; ============================================================================
;;; Character and String Literals
;;; ============================================================================

;; Character literals
(defparameter *char-simple* #\A)
(defparameter *char-newline* #\Newline)
(defparameter *char-space* #\Space)
(defparameter *char-tab* #\Tab)
(defparameter *char-unicode* #\λ)
(defparameter *char-code* #\U+03BB)

;; String literals
(defparameter *string-simple* "Hello, Common Lisp!")
(defparameter *string-escaped* "Line 1~%Line 2~TTabbed")
(defparameter *string-quotes* "She said \"Hello\"")
(defparameter *string-unicode* "Hello, 世界!")

;;; ============================================================================
;;; Symbols and Keywords
;;; ============================================================================

;; Symbols
(defparameter *symbol-simple* 'hello)
(defparameter *symbol-qualified* 'cl:car)
(defparameter *symbol-uninterned* '#:uninterned)
(defparameter *symbol-special* '*special-variable*)

;; Keywords
(defparameter *keyword* :keyword)
(defparameter *keyword-list* '(:name "Alice" :age 30))

;; Boolean and nil
(defparameter *true-value* t)
(defparameter *false-value* nil)
(defparameter *empty-list* '())

;;; ============================================================================
;;; List and Cons Structures
;;; ============================================================================

;; Lists
(defparameter *simple-list* '(1 2 3 4 5))
(defparameter *nested-list* '((1 2) (3 4) (5 6)))
(defparameter *mixed-list* '(1 "two" :three 4.0))
(defparameter *dotted-pair* '(a . b))
(defparameter *improper-list* '(1 2 3 . 4))

;; Quasiquote and unquote
(defparameter *x* 10)
(defparameter *quasiquote-example* `(1 2 ,*x* ,(+ *x* 1)))
(defparameter *splice-example* `(0 ,@'(1 2 3) 4))

;;; ============================================================================
;;; Arrays and Vectors
;;; ============================================================================

;; Vectors
(defparameter *vector* #(1 2 3 4 5))
(defparameter *adjustable-vector* (make-array 5 :adjustable t :fill-pointer 0))

;; Multi-dimensional arrays
(defparameter *2d-array* #2A((1 2 3) (4 5 6)))
(defparameter *3d-array* (make-array '(2 3 4) :initial-element 0))

;; Bit vectors
(defparameter *bit-vector* #*10110)

;; Strings are vectors of characters
(defparameter *string-vector* (make-array 10 :element-type 'character :initial-element #\Space))

;;; ============================================================================
;;; Hash Tables
;;; ============================================================================

(defparameter *hash-table* (make-hash-table :test 'equal))

(defun hash-table-example ()
  (setf (gethash "name" *hash-table*) "Alice")
  (setf (gethash "age" *hash-table*) 30)
  (gethash "name" *hash-table*))

;;; ============================================================================
;;; Structures
;;; ============================================================================

(defstruct person
  (id 0 :type integer)
  (name "" :type string)
  (age 0 :type (integer 0 150))
  (email nil :type (or string null)))

(defstruct (employee (:include person))
  (department "" :type string)
  (salary 0.0 :type float))

;; Constructor with custom name
(defstruct (point (:constructor make-pt (x y)))
  (x 0.0 :type float)
  (y 0.0 :type float))

;;; ============================================================================
;;; CLOS (Common Lisp Object System)
;;; ============================================================================

;; Class definition
(defclass animal ()
  ((name :initarg :name
         :accessor animal-name
         :type string
         :documentation "The name of the animal")
   (age :initarg :age
        :accessor animal-age
        :initform 0
        :type integer))
  (:documentation "Base class for animals"))

;; Inheritance
(defclass dog (animal)
  ((breed :initarg :breed
          :accessor dog-breed
          :type string))
  (:documentation "A dog is an animal"))

(defclass cat (animal)
  ((indoor :initarg :indoor
           :accessor cat-indoor-p
           :initform t
           :type boolean)))

;; Multiple inheritance
(defclass pet-dog (dog)
  ((owner :initarg :owner
          :accessor pet-owner
          :type string)))

;; Generic function
(defgeneric speak (animal)
  (:documentation "Make the animal speak"))

;; Method definitions
(defmethod speak ((animal animal))
  (format nil "~A makes a sound" (animal-name animal)))

(defmethod speak ((dog dog))
  (format nil "~A says: Woof!" (animal-name dog)))

(defmethod speak ((cat cat))
  (format nil "~A says: Meow!" (animal-name cat)))

;; Before, after, and around methods
(defmethod speak :before ((animal animal))
  (format t "~A is about to speak...~%" (animal-name animal)))

(defmethod speak :after ((animal animal))
  (format t "~A has finished speaking.~%" (animal-name animal)))

(defmethod speak :around ((dog dog))
  (format t "[Dog speaking] ")
  (call-next-method))

;; Initialize instance
(defmethod initialize-instance :after ((dog dog) &key)
  (format t "A new dog named ~A has been created~%" (animal-name dog)))

;;; ============================================================================
;;; Functions
;;; ============================================================================

;; Simple function
(defun add (a b)
  "Add two numbers together."
  (+ a b))

;; Recursive function
(defun factorial (n)
  "Calculate factorial of N."
  (declare (type (integer 0) n))
  (if (<= n 1)
      1
      (* n (factorial (1- n)))))

;; Tail-recursive with labels
(defun factorial-tail (n)
  "Tail-recursive factorial."
  (labels ((fact-iter (n acc)
             (if (<= n 1)
                 acc
                 (fact-iter (1- n) (* n acc)))))
    (fact-iter n 1)))

;; Multiple return values
(defun divide-with-remainder (dividend divisor)
  "Return quotient and remainder."
  (values (floor dividend divisor)
          (mod dividend divisor)))

;; Optional parameters
(defun greet (name &optional (greeting "Hello"))
  "Greet someone with optional greeting."
  (format nil "~A, ~A!" greeting name))

;; Keyword parameters
(defun make-rectangle (&key (width 1) (height 1) (color :black))
  "Create a rectangle with keyword arguments."
  (list :width width :height height :color color))

;; Rest parameters
(defun sum-all (&rest numbers)
  "Sum all given numbers."
  (reduce #'+ numbers :initial-value 0))

;; Combining parameter types
(defun complex-args (required &optional opt1 (opt2 "default") &rest rest &key key1 key2)
  "Function with complex argument list."
  (list required opt1 opt2 rest key1 key2))

;; Lambda expressions
(defparameter *double* (lambda (x) (* x 2)))
(defparameter *adder* #'(lambda (x y) (+ x y)))

;; Higher-order functions
(defun apply-twice (f x)
  "Apply function F to X twice."
  (funcall f (funcall f x)))

(defun compose (f g)
  "Return composition of F and G."
  (lambda (x) (funcall f (funcall g x))))

;;; ============================================================================
;;; Local Functions
;;; ============================================================================

(defun flet-example (x)
  "Example of FLET for local functions."
  (flet ((square (n) (* n n))
         (cube (n) (* n n n)))
    (+ (square x) (cube x))))

(defun labels-example (n)
  "Example of LABELS for recursive local functions."
  (labels ((fib (n)
             (if (< n 2)
                 n
                 (+ (fib (- n 1)) (fib (- n 2))))))
    (fib n)))

;;; ============================================================================
;;; Macros
;;; ============================================================================

;; Simple macro
(defmacro when-not (test &body body)
  "Execute BODY when TEST is false."
  `(when (not ,test)
     ,@body))

;; Macro with gensym to avoid variable capture
(defmacro with-timing (&body body)
  "Execute BODY and print execution time."
  (let ((start (gensym "START"))
        (result (gensym "RESULT")))
    `(let ((,start (get-internal-real-time)))
       (let ((,result (progn ,@body)))
         (format t "Execution time: ~,3f seconds~%"
                 (/ (- (get-internal-real-time) ,start)
                    internal-time-units-per-second))
         ,result))))

;; Macro for defining simple getters
(defmacro define-getter (name slot)
  "Define a getter function for SLOT."
  `(defun ,name (obj)
     (slot-value obj ',slot)))

;; Anaphoric macro
(defmacro aif (test then &optional else)
  "Anaphoric IF - binds IT to test result."
  `(let ((it ,test))
     (if it ,then ,else)))

;; Reader macro example (commented as it modifies read table)
;; (set-dispatch-macro-character #\# #\{
;;   (lambda (stream char1 char2)
;;     (declare (ignore char1 char2))
;;     (let ((contents (read-delimited-list #\} stream t)))
;;       `(list ,@contents))))

;;; ============================================================================
;;; Control Flow
;;; ============================================================================

(defun control-flow-examples (x)
  "Demonstrate various control flow constructs."

  ;; IF
  (let ((result1 (if (> x 0) 'positive 'non-positive)))

    ;; COND
    (let ((result2 (cond
                     ((< x 0) 'negative)
                     ((= x 0) 'zero)
                     ((< x 10) 'small)
                     ((< x 100) 'medium)
                     (t 'large))))

      ;; CASE
      (let ((result3 (case x
                       (0 'zero)
                       ((1 2 3) 'small)
                       ((4 5 6) 'medium)
                       (otherwise 'other))))

        ;; TYPECASE
        (let ((result4 (typecase x
                         (integer 'integer)
                         (float 'float)
                         (string 'string)
                         (t 'unknown))))

          (list result1 result2 result3 result4))))))

;; Loops
(defun loop-examples ()
  "Demonstrate various loop constructs."

  ;; LOOP with keywords
  (let ((sum1 (loop for i from 1 to 10 sum i)))

    ;; LOOP collect
    (let ((squares (loop for i from 1 to 5 collect (* i i))))

      ;; LOOP with multiple clauses
      (let ((result (loop for i from 0
                          for j in '(a b c d e)
                          while (< i 3)
                          collect (cons i j))))

        ;; DO loop
        (let ((sum2 (do ((i 0 (1+ i))
                         (sum 0 (+ sum i)))
                        ((> i 10) sum))))

          ;; DOLIST
          (let ((processed nil))
            (dolist (item '(1 2 3 4 5))
              (push (* item 2) processed))

            ;; DOTIMES
            (let ((factorial 1))
              (dotimes (i 5)
                (setf factorial (* factorial (1+ i))))

              (list sum1 squares result sum2 (nreverse processed) factorial))))))))

;;; ============================================================================
;;; Conditions and Restarts
;;; ============================================================================

;; Define condition
(define-condition divide-by-zero-error (error)
  ((dividend :initarg :dividend :reader dividend))
  (:report (lambda (condition stream)
             (format stream "Cannot divide ~A by zero"
                     (dividend condition)))))

(define-condition invalid-input-warning (warning)
  ((input :initarg :input :reader invalid-input))
  (:report (lambda (condition stream)
             (format stream "Invalid input: ~A"
                     (invalid-input condition)))))

;; Function with restarts
(defun safe-divide (x y)
  "Divide X by Y with restart options."
  (restart-case
      (if (zerop y)
          (error 'divide-by-zero-error :dividend x)
          (/ x y))
    (return-zero ()
      :report "Return zero"
      0)
    (use-value (value)
      :report "Use a specific value"
      :interactive (lambda () (list (read)))
      value)
    (retry-with-new-divisor (new-y)
      :report "Retry with a new divisor"
      :interactive (lambda () (list (read)))
      (safe-divide x new-y))))

;; Handler example
(defun divide-handler (x y)
  "Handle division with automatic recovery."
  (handler-case
      (safe-divide x y)
    (divide-by-zero-error (c)
      (format t "Caught error: ~A~%" c)
      :infinity)))

;; Handler-bind for more control
(defun divide-with-handler-bind (x y)
  (handler-bind
      ((divide-by-zero-error
         (lambda (c)
           (declare (ignore c))
           (invoke-restart 'return-zero))))
    (safe-divide x y)))

;;; ============================================================================
;;; Special Variables and Declarations
;;; ============================================================================

(defvar *global-counter* 0
  "A global counter variable.")

(defparameter *configuration* '(:debug t :verbose nil)
  "Configuration settings.")

(defconstant +pi+ 3.14159265358979d0
  "The value of pi.")

(defconstant +days-of-week+
  '(:monday :tuesday :wednesday :thursday :friday :saturday :sunday)
  "Days of the week.")

;; Declarations
(defun optimized-function (x y)
  "Function with various declarations."
  (declare (optimize (speed 3) (safety 0) (debug 0)))
  (declare (type fixnum x y))
  (the fixnum (+ x y)))

(defun documented-types (list)
  "Function with type declarations."
  (declare (type list list))
  (let ((result 0))
    (declare (type integer result))
    (dolist (item list result)
      (declare (type number item))
      (incf result item))))

;;; ============================================================================
;;; Multiple Values
;;; ============================================================================

(defun multiple-values-example ()
  "Demonstrate multiple value handling."

  ;; Return multiple values
  (defun min-max (list)
    (values (reduce #'min list)
            (reduce #'max list)))

  ;; Receive multiple values
  (multiple-value-bind (min-val max-val)
      (min-max '(3 1 4 1 5 9 2 6))
    (format t "Min: ~A, Max: ~A~%" min-val max-val))

  ;; Multiple-value-list
  (let ((values-as-list (multiple-value-list (floor 17 5))))
    values-as-list)

  ;; Multiple-value-call
  (multiple-value-call #'list (floor 17 5) (floor 23 7)))

;;; ============================================================================
;;; Sequences and Iteration
;;; ============================================================================

(defun sequence-examples ()
  "Common sequence operations."
  (let ((list '(1 2 3 4 5 6 7 8 9 10)))

    ;; Map functions
    (let ((mapped (mapcar #'1+ list))
          (mapped2 (map 'vector #'1+ list)))

      ;; Reduce
      (let ((sum (reduce #'+ list))
            (product (reduce #'* list :initial-value 1)))

        ;; Filter equivalents
        (let ((evens (remove-if-not #'evenp list))
              (odds (remove-if #'evenp list)))

          ;; Find functions
          (let ((found (find 5 list))
                (position (position 5 list))
                (found-if (find-if #'evenp list)))

            ;; Sort and merge
            (let ((sorted (sort (copy-list list) #'>))
                  (reversed (reverse list)))

              (list mapped sum evens found sorted))))))))

;;; ============================================================================
;;; Format Directives
;;; ============================================================================

(defun format-examples ()
  "Demonstrate FORMAT directives."

  ;; Basic directives
  (format t "String: ~A~%" "hello")           ; Aesthetic
  (format t "S-expr: ~S~%" "hello")           ; S-expression
  (format t "Decimal: ~D~%" 42)               ; Decimal
  (format t "Hex: ~X~%" 255)                  ; Hexadecimal
  (format t "Binary: ~B~%" 42)                ; Binary
  (format t "Float: ~F~%" 3.14159)            ; Floating point
  (format t "Exponential: ~E~%" 6.022e23)     ; Exponential
  (format t "Dollars: ~$~%" 1234.56)          ; Monetary

  ;; Padding and alignment
  (format t "|~10A|~%" "hello")               ; Right pad
  (format t "|~10@A|~%" "hello")              ; Left pad
  (format t "|~10:A|~%" "hello")              ; Center (with mincol)

  ;; Iteration
  (format t "List: ~{~A~^, ~}~%" '(1 2 3 4))

  ;; Conditional
  (format t "~:[false~;true~]~%" t)
  (format t "~[zero~;one~;two~:;many~]~%" 5)

  ;; Pluralization
  (format t "~D item~:P~%" 1)
  (format t "~D item~:P~%" 5))

;;; ============================================================================
;;; File I/O
;;; ============================================================================

(defun file-io-examples ()
  "Demonstrate file I/O operations."

  ;; Write to file
  (with-open-file (out "/tmp/test.txt"
                       :direction :output
                       :if-exists :supersede)
    (format out "Hello, World!~%")
    (print '(1 2 3) out)
    (write-line "Another line" out))

  ;; Read from file
  (with-open-file (in "/tmp/test.txt"
                      :direction :input)
    (let ((line1 (read-line in nil))
          (sexp (read in nil))
          (line2 (read-line in nil)))
      (list line1 sexp line2)))

  ;; Read entire file
  (defun read-file-contents (filename)
    (with-open-file (in filename)
      (let ((contents (make-string (file-length in))))
        (read-sequence contents in)
        contents))))

;;; ============================================================================
;;; Fibonacci Implementations
;;; ============================================================================

(defun fibonacci (n)
  "Calculate nth Fibonacci number (naive recursive)."
  (declare (type (integer 0) n))
  (if (< n 2)
      n
      (+ (fibonacci (- n 1))
         (fibonacci (- n 2)))))

(defun fibonacci-iterative (n)
  "Calculate nth Fibonacci number (iterative)."
  (loop repeat n
        for a = 0 then b
        and b = 1 then (+ a b)
        finally (return a)))

(defun fibonacci-memo (n)
  "Calculate nth Fibonacci number (memoized)."
  (let ((cache (make-hash-table)))
    (labels ((fib (n)
               (or (gethash n cache)
                   (setf (gethash n cache)
                         (if (< n 2)
                             n
                             (+ (fib (- n 1))
                                (fib (- n 2))))))))
      (fib n))))

;;; ============================================================================
;;; Quicksort Implementation
;;; ============================================================================

(defun quicksort (list)
  "Sort LIST using quicksort algorithm."
  (if (null list)
      nil
      (let* ((pivot (first list))
             (rest (rest list))
             (smaller (remove-if-not (lambda (x) (<= x pivot)) rest))
             (larger (remove-if-not (lambda (x) (> x pivot)) rest)))
        (append (quicksort smaller)
                (list pivot)
                (quicksort larger)))))

;;; ============================================================================
;;; Main Entry Point
;;; ============================================================================

(defun main ()
  "Main entry point demonstrating various features."
  (format t "=== Common Lisp Sample Program ===~%")

  ;; Basic functions
  (format t "Factorial 10: ~A~%" (factorial 10))
  (format t "Fibonacci 20: ~A~%" (fibonacci-iterative 20))
  (format t "Quicksort: ~A~%" (quicksort '(3 1 4 1 5 9 2 6)))

  ;; Structures
  (let ((alice (make-person :id 1 :name "Alice" :age 30)))
    (format t "Person: ~A, age ~A~%" (person-name alice) (person-age alice)))

  ;; CLOS
  (let ((dog (make-instance 'dog :name "Rex" :age 5 :breed "German Shepherd")))
    (format t "~A~%" (speak dog)))

  ;; Macros
  (with-timing
    (sleep 0.1))

  ;; Multiple values
  (multiple-value-bind (q r) (divide-with-remainder 17 5)
    (format t "17 / 5 = ~A remainder ~A~%" q r))

  (format t "Done!~%"))

;; Run main when loading
;; (main)
