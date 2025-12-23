;; Comprehensive Clojure language sample demonstrating all syntax features
;; Clojure is a dynamic, functional Lisp dialect for the JVM

(ns com.example.sample
  "Sample namespace demonstrating Clojure features"
  (:require [clojure.string :as str]
            [clojure.set :as set]
            [clojure.walk :as walk]
            [clojure.pprint :refer [pprint]]
            [clojure.java.io :as io]
            [clojure.core.async :as async :refer [go go-loop <! >! chan close!]])
  (:import [java.time LocalDate LocalDateTime]
           [java.util UUID Date]
           [java.io File BufferedReader]
           [java.util.concurrent Executors]))

;; ============================================================================
;; Constants and Definitions
;; ============================================================================

(def ^:const max-buffer-size 1024)
(def ^:const pi 3.14159265358979323846)
(def greeting "Hello, Clojure!")

;; Dynamic variables (can be rebound with binding)
(def ^:dynamic *debug* false)
(def ^:dynamic *output* *out*)

;; Private definition
(def ^:private internal-state (atom {}))

;; ============================================================================
;; Basic Data Types
;; ============================================================================

;; Numbers
(def integer-val 42)
(def long-val 9223372036854775807)
(def big-int 123456789012345678901234567890N)
(def ratio-val 22/7)
(def float-val 3.14)
(def double-val 3.14159265358979)
(def big-decimal 3.14159265358979323846M)
(def hex-val 0xDEADBEEF)
(def octal-val 0755)
(def binary-val 2r10101010)
(def radix-val 36rCLOJURE)
(def scientific 1.23e-4)

;; Strings
(def string-val "Hello, World!")
(def multiline-string "This is a
multi-line string
in Clojure")
(def escaped-string "Tab:\t Newline:\n Quote:\"")

;; Characters
(def char-a \a)
(def char-newline \newline)
(def char-tab \tab)
(def char-unicode \u0041)

;; Keywords
(def keyword-val :keyword)
(def namespaced-keyword ::namespaced)
(def qualified-keyword :user/qualified)

;; Symbols
(def symbol-val 'symbol)
(def namespaced-symbol 'user/symbol)

;; Boolean and Nil
(def true-val true)
(def false-val false)
(def nil-val nil)

;; Regular expressions
(def pattern #"\d+")
(def email-pattern #"[\w.]+@[\w.]+\.\w+")

;; ============================================================================
;; Collections
;; ============================================================================

;; Lists (linked lists)
(def list-val '(1 2 3 4 5))
(def list-constructed (list 1 2 3 4 5))

;; Vectors (indexed arrays)
(def vector-val [1 2 3 4 5])
(def empty-vector [])
(def nested-vector [[1 2] [3 4] [5 6]])

;; Maps (hash maps)
(def map-val {:name "Alice" :age 30 :email "alice@example.com"})
(def empty-map {})
(def sorted-map-val (sorted-map :a 1 :b 2 :c 3))
(def array-map-val (array-map :a 1 :b 2 :c 3))

;; Sets
(def set-val #{1 2 3 4 5})
(def empty-set #{})
(def sorted-set-val (sorted-set 3 1 4 1 5 9))

;; ============================================================================
;; Functions
;; ============================================================================

;; Basic function definition
(defn add
  "Adds two numbers together"
  [a b]
  (+ a b))

;; Multi-arity function
(defn greet
  "Greets a person or the world"
  ([] (greet "World"))
  ([name] (str "Hello, " name "!"))
  ([greeting name] (str greeting ", " name "!")))

;; Variadic function
(defn sum
  "Sums any number of arguments"
  [& numbers]
  (reduce + 0 numbers))

;; Function with destructuring
(defn process-point
  "Processes a point map"
  [{:keys [x y z] :or {z 0}}]
  (Math/sqrt (+ (* x x) (* y y) (* z z))))

;; Function with pre/post conditions
(defn safe-divide
  "Divides a by b with validation"
  [a b]
  {:pre [(number? a) (number? b) (not= b 0)]
   :post [(number? %)]}
  (/ a b))

;; Private function
(defn- internal-helper
  "Internal helper function"
  [x]
  (* x 2))

;; Anonymous functions
(def square (fn [x] (* x x)))
(def cube #(* % % %))
(def add-fn #(+ %1 %2))
(def rest-args #(apply + %&))

;; Higher-order functions
(defn apply-twice
  "Applies a function twice"
  [f x]
  (f (f x)))

(defn compose
  "Composes two functions"
  [f g]
  (fn [x] (f (g x))))

(defn make-adder
  "Returns an adder function"
  [n]
  (fn [x] (+ x n)))

;; Recursive function
(defn factorial
  "Calculates factorial recursively"
  [n]
  (if (<= n 1)
    1
    (* n (factorial (dec n)))))

;; Tail-recursive with recur
(defn factorial-tail
  "Tail-recursive factorial"
  ([n] (factorial-tail n 1))
  ([n acc]
   (if (<= n 1)
     acc
     (recur (dec n) (* acc n)))))

;; Loop/recur
(defn factorial-loop
  "Factorial using loop/recur"
  [n]
  (loop [i n
         acc 1]
    (if (<= i 1)
      acc
      (recur (dec i) (* acc i)))))

;; Memoized function
(def fib
  "Memoized Fibonacci"
  (memoize
   (fn [n]
     (if (< n 2)
       n
       (+ (fib (- n 1)) (fib (- n 2)))))))

;; ============================================================================
;; Macros
;; ============================================================================

(defmacro when-debug
  "Executes body only when *debug* is true"
  [& body]
  `(when *debug*
     ~@body))

(defmacro with-timing
  "Times the execution of body"
  [& body]
  `(let [start# (System/nanoTime)
         result# (do ~@body)
         elapsed# (/ (- (System/nanoTime) start#) 1e6)]
     (println (str "Elapsed: " elapsed# " ms"))
     result#))

(defmacro unless
  "Opposite of when"
  [condition & body]
  `(when (not ~condition)
     ~@body))

(defmacro defonce-private
  "Defines a private var that's only set once"
  [name & body]
  `(defonce ^:private ~name ~@body))

;; Threading macros demonstration
(defn threading-examples []
  ;; Thread-first (->)
  (-> 1
      (+ 2)
      (* 3)
      (- 1))

  ;; Thread-last (->>)
  (->> (range 10)
       (filter even?)
       (map #(* % 2))
       (reduce +))

  ;; Some-threading (some->)
  (some-> {:a {:b {:c 1}}}
          :a
          :b
          :c)

  ;; Some-threading last (some->>)
  (some->> [1 2 3]
           (filter even?)
           first)

  ;; As-threading (as->)
  (as-> 1 $
    (+ $ 2)
    (* 3 $)
    (- $ 1)))

;; ============================================================================
;; Data Structures and Records
;; ============================================================================

;; Record definition
(defrecord Point [x y z])

(defrecord Person [name age email])

;; Protocol definition
(defprotocol Shape
  "Protocol for geometric shapes"
  (area [shape] "Calculate area")
  (perimeter [shape] "Calculate perimeter"))

(defprotocol Drawable
  "Protocol for drawable objects"
  (draw [drawable] "Draw the object"))

;; Record implementing protocols
(defrecord Circle [radius]
  Shape
  (area [_] (* pi radius radius))
  (perimeter [_] (* 2 pi radius))

  Drawable
  (draw [_] (println (str "Drawing circle with radius " radius))))

(defrecord Rectangle [width height]
  Shape
  (area [_] (* width height))
  (perimeter [_] (* 2 (+ width height)))

  Drawable
  (draw [_] (println (str "Drawing rectangle " width "x" height))))

;; Extend protocol to existing type
(extend-protocol Shape
  java.lang.Number
  (area [n] (* n n))
  (perimeter [n] (* 4 n)))

;; Multi-methods
(defmulti process-shape
  "Process a shape based on its type"
  :type)

(defmethod process-shape :circle
  [{:keys [radius]}]
  (println (str "Processing circle with radius " radius)))

(defmethod process-shape :rectangle
  [{:keys [width height]}]
  (println (str "Processing rectangle " width "x" height)))

(defmethod process-shape :default
  [shape]
  (println "Unknown shape"))

;; ============================================================================
;; State Management
;; ============================================================================

;; Atoms (synchronous, uncoordinated)
(def counter (atom 0))
(def app-state (atom {:users [] :config {}}))

(defn increment-counter! []
  (swap! counter inc))

(defn update-state! [key value]
  (swap! app-state assoc key value))

;; Refs (synchronous, coordinated)
(def account-a (ref 1000))
(def account-b (ref 2000))

(defn transfer! [from to amount]
  (dosync
   (alter from - amount)
   (alter to + amount)))

;; Agents (asynchronous)
(def log-agent (agent []))

(defn log! [message]
  (send log-agent conj {:time (System/currentTimeMillis) :message message}))

;; Vars (dynamic binding)
(defn with-debug [f]
  (binding [*debug* true]
    (f)))

;; Validators
(def validated-atom
  (atom 0
        :validator #(>= % 0)))

;; Watches
(add-watch counter :logger
           (fn [key ref old-val new-val]
             (println (str "Counter changed from " old-val " to " new-val))))

;; ============================================================================
;; Sequences and Lazy Evaluation
;; ============================================================================

;; Lazy sequences
(def natural-numbers (iterate inc 1))
(def fibonacci-seq
  ((fn fib [a b]
     (lazy-seq (cons a (fib b (+ a b)))))
   0 1))

;; Sequence operations
(defn sequence-examples []
  ;; Map, filter, reduce
  (map inc [1 2 3 4 5])
  (filter even? (range 10))
  (reduce + (range 100))

  ;; Take, drop
  (take 10 natural-numbers)
  (drop 5 (range 20))
  (take-while #(< % 10) natural-numbers)
  (drop-while #(< % 5) (range 20))

  ;; Partition
  (partition 3 (range 9))
  (partition-all 3 (range 10))
  (partition-by #(< % 5) (range 10))

  ;; Group and sort
  (group-by even? (range 10))
  (sort-by :age [{:name "Alice" :age 30} {:name "Bob" :age 25}])

  ;; Flatten
  (flatten [[1 2] [3 [4 5]] 6])

  ;; Interleave and interpose
  (interleave [:a :b :c] [1 2 3])
  (interpose ", " ["a" "b" "c"])

  ;; Frequencies
  (frequencies "abracadabra")

  ;; Distinct and dedupe
  (distinct [1 2 1 3 2 4])
  (dedupe [1 1 2 2 2 3 3]))

;; Transducers
(def xform
  (comp
   (filter even?)
   (map #(* % 2))
   (take 5)))

(defn transducer-examples []
  (into [] xform (range 100))
  (transduce xform + (range 100))
  (sequence xform (range 100)))

;; ============================================================================
;; Destructuring
;; ============================================================================

(defn destructuring-examples []
  ;; Sequential destructuring
  (let [[a b c] [1 2 3]]
    (+ a b c))

  ;; With rest
  (let [[head & tail] [1 2 3 4 5]]
    {:head head :tail tail})

  ;; Nested
  (let [[[a b] [c d]] [[1 2] [3 4]]]
    (+ a b c d))

  ;; Associative destructuring
  (let [{:keys [name age]} {:name "Alice" :age 30}]
    (str name " is " age))

  ;; With defaults
  (let [{:keys [x y z] :or {z 0}} {:x 1 :y 2}]
    (+ x y z))

  ;; With :as
  (let [{:keys [name] :as person} {:name "Alice" :age 30}]
    [name person])

  ;; String keys
  (let [{:strs [name age]} {"name" "Alice" "age" 30}]
    (str name " is " age))

  ;; Symbol keys
  (let [{:syms [x y]} {'x 1 'y 2}]
    (+ x y)))

;; ============================================================================
;; Error Handling
;; ============================================================================

(defn error-handling-examples []
  ;; Try/catch/finally
  (try
    (/ 1 0)
    (catch ArithmeticException e
      (println "Arithmetic error:" (.getMessage e)))
    (catch Exception e
      (println "General error:" (.getMessage e)))
    (finally
      (println "Cleanup")))

  ;; Throw
  (try
    (throw (ex-info "Custom error" {:code 42}))
    (catch clojure.lang.ExceptionInfo e
      (println "Error:" (.getMessage e))
      (println "Data:" (ex-data e))))

  ;; Assert
  (assert (> 5 0) "Value must be positive"))

;; ============================================================================
;; Java Interop
;; ============================================================================

(defn java-interop-examples []
  ;; Instance method call
  (.toUpperCase "hello")

  ;; Static method call
  (Math/sqrt 16)

  ;; Constructor
  (Date.)
  (java.util.ArrayList. [1 2 3])

  ;; Field access
  (.-x (Point. 1 2 3))

  ;; Set field (Java classes only)
  ;; (set! (.-field obj) value)

  ;; Chained calls
  (.. "hello" (toUpperCase) (substring 0 3))

  ;; doto for side effects
  (doto (java.util.ArrayList.)
    (.add 1)
    (.add 2)
    (.add 3))

  ;; reify for implementing interfaces
  (reify
    java.lang.Runnable
    (run [_] (println "Running!")))

  ;; proxy for extending classes
  (proxy [java.io.OutputStream] []
    (write [b] (println b))))

;; ============================================================================
;; Concurrency
;; ============================================================================

(defn concurrency-examples []
  ;; Future
  (let [f (future
            (Thread/sleep 100)
            42)]
    @f)  ; deref to get value

  ;; Promise
  (let [p (promise)]
    (future
      (Thread/sleep 100)
      (deliver p 42))
    @p)

  ;; pmap for parallel map
  (pmap #(* % %) (range 10))

  ;; pcalls and pvalues
  (pcalls #(+ 1 2) #(* 3 4) #(- 5 6))
  (pvalues (+ 1 2) (* 3 4) (- 5 6)))

;; core.async examples
(defn async-examples []
  ;; Create channels
  (let [c (chan)]
    (go (>! c "Hello"))
    (go (println (<! c))))

  ;; Buffered channel
  (let [c (chan 10)]
    (dotimes [i 10]
      (go (>! c i)))
    (go-loop []
      (when-let [v (<! c)]
        (println v)
        (recur))))

  ;; alts for selecting
  (let [c1 (chan)
        c2 (chan)]
    (go (>! c1 "first"))
    (go (>! c2 "second"))
    (go
      (let [[v ch] (async/alts! [c1 c2])]
        (println "Got" v "from" ch)))))

;; ============================================================================
;; Spec (clojure.spec.alpha)
;; ============================================================================

;; Note: Would require requiring clojure.spec.alpha
;; Example spec definitions:
#_(do
    (require '[clojure.spec.alpha :as s])

    (s/def ::name string?)
    (s/def ::age (s/and int? #(>= % 0)))
    (s/def ::email (s/and string? #(re-matches email-pattern %)))

    (s/def ::person
      (s/keys :req-un [::name ::age]
              :opt-un [::email]))

    (s/fdef greet
      :args (s/cat :name string?)
      :ret string?))

;; ============================================================================
;; Main Entry Point
;; ============================================================================

(defn -main
  "Main entry point"
  [& args]
  (println greeting)

  ;; Basic operations
  (println "Add:" (add 1 2))
  (println "Greet:" (greet "Clojure"))
  (println "Sum:" (sum 1 2 3 4 5))

  ;; Collection operations
  (println "Map:" (map inc [1 2 3]))
  (println "Filter:" (filter even? (range 10)))
  (println "Reduce:" (reduce + (range 10)))

  ;; Data structures
  (let [point (->Point 1 2 3)]
    (println "Point:" point)
    (println "X:" (:x point)))

  (let [person (map->Person {:name "Alice" :age 30 :email "alice@example.com"})]
    (println "Person:" person))

  ;; Shapes
  (let [circle (->Circle 5)
        rect (->Rectangle 4 6)]
    (println "Circle area:" (area circle))
    (println "Rectangle area:" (area rect))
    (draw circle)
    (draw rect))

  ;; Atoms
  (increment-counter!)
  (increment-counter!)
  (println "Counter:" @counter)

  ;; Lazy sequences
  (println "First 10 naturals:" (take 10 natural-numbers))
  (println "First 10 fibs:" (take 10 fibonacci-seq))

  ;; Threading
  (println "Threading:"
           (-> 1
               (+ 2)
               (* 3)))

  ;; Destructuring
  (let [{:keys [name age]} {:name "Bob" :age 25}]
    (println "Name:" name "Age:" age))

  ;; Pattern matching with core.match (if available)
  ;; (match [x]
  ;;   [1] "one"
  ;;   [2] "two"
  ;;   :else "other")

  ;; Java interop
  (println "Date:" (Date.))
  (println "UUID:" (UUID/randomUUID))

  ;; Control flow
  (when true
    (println "When true"))

  (if-let [x (some identity [nil nil 42])]
    (println "Found:" x)
    (println "Not found"))

  (cond
    (< 1 0) "impossible"
    (> 1 0) "greater"
    :else "equal")

  (case 1
    1 "one"
    2 "two"
    "other")

  (condp = 1
    1 "one"
    2 "two"
    "other")

  ;; Loop
  (println "Loop result:"
           (loop [i 0
                  acc []]
             (if (>= i 5)
               acc
               (recur (inc i) (conj acc i)))))

  ;; For comprehension
  (println "For:"
           (for [x (range 3)
                 y (range 3)
                 :when (not= x y)]
             [x y]))

  ;; Doseq (side effects)
  (doseq [x (range 5)]
    (println "Item:" x))

  ;; Dotimes
  (dotimes [i 3]
    (println "Iteration:" i))

  ;; Timing
  (println "Timed:"
           (time (reduce + (range 10000))))

  ;; Parallel
  (println "pmap:" (pmap #(* % %) (range 10)))

  (println "Program completed successfully!"))

;; Run main when loaded
(when (= *file* (System/getProperty "babashka.file"))
  (apply -main *command-line-args*))

;; REPL utilities
(comment
  ;; Evaluate these in the REPL

  ;; Start main
  (-main)

  ;; Test functions
  (add 1 2)
  (greet)
  (greet "Clojure")
  (sum 1 2 3 4 5)

  ;; Test data structures
  (->Point 1 2 3)
  (->Circle 5)
  (area (->Circle 5))

  ;; State
  @counter
  (increment-counter!)

  ;; Debug mode
  (binding [*debug* true]
    (when-debug (println "Debug output")))

  ;; End comment block
  nil)
