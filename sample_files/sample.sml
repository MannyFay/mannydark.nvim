(* ===========================================================================
   Comprehensive Standard ML Sample - Syntax Highlighting Demonstration
   =========================================================================== *)

(*
 * This file demonstrates all major Standard ML language features
 * for syntax highlighting purposes.
 *)

(* ===========================================================================
   Numeric and Character Literals
   =========================================================================== *)

(* Integer literals *)
val integerLiteral = 42
val negativeInt = ~17        (* Negative with tilde *)
val hexLiteral = 0xFF
val wordLiteral = 0w255      (* Word (unsigned) *)
val hexWord = 0wxFF

(* Real (floating point) literals *)
val realLiteral = 3.14159
val scientific = 6.022E23
val negativeReal = ~2.71828
val scientificNeg = 1.0E~10

(* Character literals *)
val charLiteral = #"A"
val escapedChar = #"\n"
val unicodeChar = #"\u03BB"  (* λ *)

(* String literals *)
val stringLiteral = "Hello, Standard ML!"
val escapedString = "Line 1\nLine 2\tTabbed"
val unicodeString = "Hello, \u4E16\u754C!"
val multilineString = "This is a \
                      \continuation of the same string"

(* ===========================================================================
   Type Definitions
   =========================================================================== *)

(* Type alias *)
type userId = int
type email = string
type point = real * real

(* Parameterized type alias *)
type 'a pair = 'a * 'a
type ('a, 'b) assoc = ('a * 'b) list

(* Datatype (algebraic data type) *)
datatype status = Active | Inactive | Pending of string | Error of int * string

datatype color = Red | Green | Blue | RGB of int * int * int

(* Parameterized datatype *)
datatype 'a option = NONE | SOME of 'a
datatype 'a tree = Leaf | Node of 'a * 'a tree * 'a tree

(* Mutually recursive datatypes *)
datatype expr =
    Num of int
  | Var of string
  | BinOp of oper * expr * expr
  | Let of string * expr * expr
  | Lambda of string * expr
  | App of expr * expr
and oper = Add | Sub | Mul | Div

(* Record type *)
type person = {
  id: int,
  name: string,
  age: int,
  email: string option
}

(* Exception definition *)
exception NotFound of string
exception InvalidArgument
exception DivisionByZero
exception ParseError of {line: int, col: int, msg: string}

(* ===========================================================================
   Pattern Matching
   =========================================================================== *)

(* Function with pattern matching *)
fun factorial 0 = 1
  | factorial n = n * factorial (n - 1)

(* Pattern matching on datatypes *)
fun describeStatus Active = "Currently active"
  | describeStatus Inactive = "Not active"
  | describeStatus (Pending reason) = "Pending: " ^ reason
  | describeStatus (Error (code, msg)) =
      "Error " ^ Int.toString code ^ ": " ^ msg

(* Pattern matching on lists *)
fun length [] = 0
  | length (_::xs) = 1 + length xs

fun sum [] = 0
  | sum (x::xs) = x + sum xs

fun last [x] = x
  | last (_::xs) = last xs
  | last [] = raise NotFound "empty list"

(* Pattern matching with guards (using if) *)
fun classify n =
  if n < 0 then "negative"
  else if n = 0 then "zero"
  else "positive"

(* As-patterns *)
fun firstTwo (all as x::y::_) = SOME (x, y, all)
  | firstTwo _ = NONE

(* Nested patterns *)
fun deepMatch (Node (x, Leaf, Leaf)) = "single node"
  | deepMatch (Node (_, Node _, Leaf)) = "left heavy"
  | deepMatch (Node (_, Leaf, Node _)) = "right heavy"
  | deepMatch (Node (_, Node _, Node _)) = "balanced"
  | deepMatch Leaf = "empty"

(* Pattern matching on tuples *)
fun swap (a, b) = (b, a)
fun fst (a, _) = a
fun snd (_, b) = b

(* Pattern matching on records *)
fun getPersonName ({name, ...}: person) = name
fun isAdult ({age, ...}: person) = age >= 18

(* ===========================================================================
   Functions
   =========================================================================== *)

(* Simple function *)
fun add x y = x + y

(* Function with type annotation *)
fun multiply (x: int) (y: int): int = x * y

(* Recursive function *)
fun fib 0 = 0
  | fib 1 = 1
  | fib n = fib (n - 1) + fib (n - 2)

(* Tail-recursive function *)
fun factorialTail n =
  let
    fun loop 0 acc = acc
      | loop n acc = loop (n - 1) (n * acc)
  in
    loop n 1
  end

(* Mutually recursive functions *)
fun isEven 0 = true
  | isEven n = isOdd (n - 1)
and isOdd 0 = false
  | isOdd n = isEven (n - 1)

(* Anonymous function (lambda) *)
val double = fn x => x * 2
val addPair = fn (x, y) => x + y

(* Function composition *)
val compose = fn f => fn g => fn x => f (g x)
infix 3 o
fun (f o g) x = f (g x)

(* Curried vs uncurried *)
fun curriedAdd x y = x + y
fun uncurriedAdd (x, y) = x + y

(* Partial application *)
val addFive = add 5
val doubled = List.map double

(* ===========================================================================
   Higher-Order Functions
   =========================================================================== *)

(* Map *)
fun map f [] = []
  | map f (x::xs) = f x :: map f xs

(* Filter *)
fun filter pred [] = []
  | filter pred (x::xs) =
      if pred x then x :: filter pred xs
      else filter pred xs

(* Fold left *)
fun foldl f acc [] = acc
  | foldl f acc (x::xs) = foldl f (f (x, acc)) xs

(* Fold right *)
fun foldr f acc [] = acc
  | foldr f acc (x::xs) = f (x, foldr f acc xs)

(* Using higher-order functions *)
val sumList = foldl op+ 0
val productList = foldl op* 1
val flatten = foldr op@ []

(* Function returning function *)
fun makeAdder n = fn x => x + n

(* ===========================================================================
   Let Expressions and Local Definitions
   =========================================================================== *)

fun complexComputation x =
  let
    val squared = x * x
    val cubed = x * x * x
    fun helper a b = squared + a * b
    val intermediate = helper 2 3
  in
    intermediate + cubed
  end

(* Nested let *)
fun nestedLet x =
  let
    val a = x + 1
    val b =
      let
        val inner = a * 2
      in
        inner + a
      end
  in
    a + b
  end

(* Local declarations in structure *)
local
  fun privateHelper x = x * 2
  val privateConstant = 42
in
  fun publicFunction x = privateHelper x + privateConstant
end

(* ===========================================================================
   Modules (Structures and Signatures)
   =========================================================================== *)

(* Signature (interface) *)
signature STACK = sig
  type 'a stack
  val empty: 'a stack
  val isEmpty: 'a stack -> bool
  val push: 'a * 'a stack -> 'a stack
  val pop: 'a stack -> ('a * 'a stack) option
  val peek: 'a stack -> 'a option
end

(* Structure (implementation) *)
structure ListStack :> STACK = struct
  type 'a stack = 'a list

  val empty = []

  fun isEmpty [] = true
    | isEmpty _ = false

  fun push (x, s) = x :: s

  fun pop [] = NONE
    | pop (x::xs) = SOME (x, xs)

  fun peek [] = NONE
    | peek (x::_) = SOME x
end

(* Parameterized signature *)
signature ORDERED = sig
  type t
  val compare: t * t -> order
end

signature SET = sig
  type elem
  type set
  val empty: set
  val insert: elem -> set -> set
  val member: elem -> set -> bool
  val toList: set -> elem list
end

(* Functor *)
functor MakeSet(Ord: ORDERED): SET where type elem = Ord.t = struct
  type elem = Ord.t
  type set = elem list  (* Simple implementation *)

  val empty = []

  fun insert x [] = [x]
    | insert x (y::ys) =
        case Ord.compare (x, y) of
          LESS => x :: y :: ys
        | EQUAL => y :: ys
        | GREATER => y :: insert x ys

  fun member x [] = false
    | member x (y::ys) =
        case Ord.compare (x, y) of
          LESS => false
        | EQUAL => true
        | GREATER => member x ys

  fun toList s = s
end

(* Using the functor *)
structure IntOrd: ORDERED = struct
  type t = int
  val compare = Int.compare
end

structure IntSet = MakeSet(IntOrd)

(* Opening a structure *)
fun useIntSet () =
  let
    open IntSet
    val s = insert 3 (insert 1 (insert 2 empty))
  in
    toList s
  end

(* ===========================================================================
   References and Imperative Features
   =========================================================================== *)

(* Reference cells *)
val counter = ref 0

fun increment () = counter := !counter + 1
fun decrement () = counter := !counter - 1
fun reset () = counter := 0
fun getCount () = !counter

(* While loop *)
fun countDown n =
  let
    val i = ref n
  in
    while !i > 0 do (
      print (Int.toString (!i) ^ "\n");
      i := !i - 1
    )
  end

(* Sequencing with semicolon *)
fun doMultiple () = (
  print "First\n";
  print "Second\n";
  print "Third\n";
  42  (* Return value *)
)

(* Arrays *)
fun arrayExample () =
  let
    val arr = Array.array (10, 0)
    val _ = Array.update (arr, 0, 42)
    val elem = Array.sub (arr, 0)
  in
    elem
  end

(* ===========================================================================
   Exception Handling
   =========================================================================== *)

(* Raising exceptions *)
fun divide x y =
  if y = 0 then raise DivisionByZero
  else x div y

fun findElement pred [] = raise NotFound "element not found"
  | findElement pred (x::xs) =
      if pred x then x
      else findElement pred xs

(* Handling exceptions *)
fun safeDivide x y =
  divide x y
  handle DivisionByZero => 0

fun findWithDefault pred lst default =
  findElement pred lst
  handle NotFound _ => default

(* Multiple exception handlers *)
fun robustOperation input =
  (someOperation input)
  handle NotFound msg => (print ("Not found: " ^ msg ^ "\n"); NONE)
       | InvalidArgument => (print "Invalid argument\n"; NONE)
       | _ => (print "Unknown error\n"; NONE)
and someOperation _ = raise NotFound "test"

(* ===========================================================================
   Imperative Constructs
   =========================================================================== *)

(* Before/after style *)
fun withResource acquire release use =
  let
    val resource = acquire ()
    val result = use resource
      handle e => (release resource; raise e)
  in
    release resource;
    result
  end

(* For loop simulation *)
fun for (start, stop, f) =
  let
    fun loop i =
      if i > stop then ()
      else (f i; loop (i + 1))
  in
    loop start
  end

(* ===========================================================================
   Infix Operators
   =========================================================================== *)

(* Declare infix *)
infix 5 +++
infix 6 ***
infixr 4 @@

fun x +++ y = x + y + 1
fun x *** y = x * y * 2
fun f @@ x = f x

(* Using infix operators *)
val infixResult = 2 +++ 3 *** 4  (* Uses precedence *)

(* Nonfix *)
nonfix +++
val nonfixResult = +++ (2, 3)

(* ===========================================================================
   Advanced Type Features
   =========================================================================== *)

(* Equality types *)
fun findIndex (x: ''a) (lst: ''a list) =
  let
    fun loop _ [] = NONE
      | loop i (y::ys) = if x = y then SOME i else loop (i + 1) ys
  in
    loop 0 lst
  end

(* Type annotations *)
fun annotated (x: int, y: real): string =
  Int.toString x ^ " and " ^ Real.toString y

(* Polymorphic functions *)
fun identity (x: 'a): 'a = x
fun constant (x: 'a) (_: 'b): 'a = x
fun apply (f: 'a -> 'b) (x: 'a): 'b = f x

(* Constrained type variables *)
fun maxList (lst: 'a list, lt: 'a * 'a -> bool): 'a option =
  case lst of
    [] => NONE
  | [x] => SOME x
  | x::xs =>
      case maxList (xs, lt) of
        NONE => SOME x
      | SOME m => SOME (if lt (x, m) then m else x)

(* ===========================================================================
   Record Operations
   =========================================================================== *)

(* Creating records *)
val alice: person = {
  id = 1,
  name = "Alice",
  age = 30,
  email = SOME "alice@example.com"
}

val bob = {id = 2, name = "Bob", age = 25, email = NONE}

(* Record field access *)
val aliceName = #name alice
val aliceAge = #age alice

(* Record update (functional) *)
fun birthday (p: person): person = {
  id = #id p,
  name = #name p,
  age = #age p + 1,
  email = #email p
}

(* Flexible records *)
fun getName {name: string, ...} = name
fun getAge {age: int, ...} = age

(* ===========================================================================
   Lazy Evaluation (Manual)
   =========================================================================== *)

(* Thunks for lazy evaluation *)
type 'a thunk = unit -> 'a

fun delay f = f
fun force f = f ()

(* Lazy stream *)
datatype 'a stream = Nil | Cons of 'a * (unit -> 'a stream)

fun naturalsFrom n = Cons (n, fn () => naturalsFrom (n + 1))
val naturals = naturalsFrom 0

fun take 0 _ = []
  | take _ Nil = []
  | take n (Cons (x, xs)) = x :: take (n - 1) (xs ())

fun mapStream f Nil = Nil
  | mapStream f (Cons (x, xs)) = Cons (f x, fn () => mapStream f (xs ()))

fun filterStream pred Nil = Nil
  | filterStream pred (Cons (x, xs)) =
      if pred x then Cons (x, fn () => filterStream pred (xs ()))
      else filterStream pred (xs ())

(* ===========================================================================
   Quicksort Implementation
   =========================================================================== *)

fun quicksort [] = []
  | quicksort (pivot::rest) =
      let
        val smaller = List.filter (fn x => x <= pivot) rest
        val larger = List.filter (fn x => x > pivot) rest
      in
        quicksort smaller @ [pivot] @ quicksort larger
      end

(* ===========================================================================
   Expression Evaluator
   =========================================================================== *)

(* Environment for variables *)
type env = (string * int) list

fun lookup (x: string) (env: env): int option =
  case List.find (fn (name, _) => name = x) env of
    NONE => NONE
  | SOME (_, v) => SOME v

fun eval (env: env) (Num n) = n
  | eval env (Var x) =
      (case lookup x env of
         SOME v => v
       | NONE => raise NotFound ("Variable " ^ x))
  | eval env (BinOp (Add, e1, e2)) = eval env e1 + eval env e2
  | eval env (BinOp (Sub, e1, e2)) = eval env e1 - eval env e2
  | eval env (BinOp (Mul, e1, e2)) = eval env e1 * eval env e2
  | eval env (BinOp (Div, e1, e2)) =
      let val v2 = eval env e2 in
        if v2 = 0 then raise DivisionByZero
        else eval env e1 div v2
      end
  | eval env (Let (x, e1, e2)) =
      let val v = eval env e1 in
        eval ((x, v) :: env) e2
      end
  | eval _ (Lambda _) = raise InvalidArgument
  | eval _ (App _) = raise InvalidArgument

(* ===========================================================================
   Standard Basis Library Usage
   =========================================================================== *)

fun basisExamples () =
  let
    (* String operations *)
    val s = "Hello, World!"
    val upper = String.map Char.toUpper s
    val tokens = String.tokens (fn c => c = #",") s
    val concat = String.concat ["a", "b", "c"]

    (* List operations *)
    val lst = [1, 2, 3, 4, 5]
    val doubled = List.map (fn x => x * 2) lst
    val evens = List.filter (fn x => x mod 2 = 0) lst
    val sum = List.foldl op+ 0 lst

    (* Option operations *)
    val opt = SOME 42
    val value = Option.valOf opt
    val mapped = Option.map (fn x => x * 2) opt

    (* Int operations *)
    val abs = Int.abs (~42)
    val max = Int.max (10, 20)
    val str = Int.toString 42

    (* Real operations *)
    val sqrt = Math.sqrt 2.0
    val sin = Math.sin Math.pi
    val floor = Real.floor 3.7
  in
    (upper, tokens, doubled, evens, sum)
  end

(* ===========================================================================
   Main Entry Point
   =========================================================================== *)

fun main () =
  let
    val _ = print "=== Standard ML Sample Program ===\n"
    val _ = print ("Factorial 10: " ^ Int.toString (factorial 10) ^ "\n")
    val _ = print ("Fibonacci 20: " ^ Int.toString (fib 20) ^ "\n")
    val _ = print ("Quicksort: " ^
                   String.concatWith ", " (List.map Int.toString (quicksort [3, 1, 4, 1, 5, 9, 2, 6])) ^
                   "\n")

    (* Pattern matching demo *)
    val _ = print ("Status: " ^ describeStatus Active ^ "\n")

    (* Records *)
    val _ = print ("Person: " ^ #name alice ^ ", age " ^ Int.toString (#age alice) ^ "\n")

    (* Module usage *)
    val s = IntSet.insert 3 (IntSet.insert 1 (IntSet.insert 2 IntSet.empty))
    val _ = print ("Set contains 2: " ^ Bool.toString (IntSet.member 2 s) ^ "\n")

    (* Stream demo *)
    val first10 = take 10 naturals
    val _ = print ("First 10 naturals: " ^
                   String.concatWith ", " (List.map Int.toString first10) ^
                   "\n")

    val _ = print "Done!\n"
  in
    ()
  end

(* Run main *)
val _ = main ()
