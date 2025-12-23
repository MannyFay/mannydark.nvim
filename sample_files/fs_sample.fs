/// Comprehensive F# language sample demonstrating all syntax features
/// F# is a functional-first programming language for .NET

module Sample

open System
open System.IO
open System.Text
open System.Text.RegularExpressions
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks

// ============================================================================
// Modules and Namespaces
// ============================================================================

/// Constants module
[<AutoOpen>]
module Constants =
    [<Literal>]
    let MaxBufferSize = 1024

    [<Literal>]
    let Pi = 3.14159265358979323846

    [<Literal>]
    let Greeting = "Hello, F#!"

// ============================================================================
// Type Definitions
// ============================================================================

/// Discriminated union for colors
type Color =
    | Red
    | Green
    | Blue
    | Rgb of r: int * g: int * b: int
    | Hex of int

/// Status enumeration
type Status =
    | Ok = 0
    | Error = 1
    | Pending = 2

/// Record type
type Point =
    { X: float
      Y: float
      Z: float }

    /// Instance member
    member this.Distance(other: Point) =
        let dx = this.X - other.X
        let dy = this.Y - other.Y
        let dz = this.Z - other.Z
        sqrt (dx * dx + dy * dy + dz * dz)

    /// Static member
    static member Origin = { X = 0.0; Y = 0.0; Z = 0.0 }

    /// Operator overloading
    static member (+) (a: Point, b: Point) =
        { X = a.X + b.X; Y = a.Y + b.Y; Z = a.Z + b.Z }

    static member (-) (a: Point, b: Point) =
        { X = a.X - b.X; Y = a.Y - b.Y; Z = a.Z - b.Z }

    static member (*) (p: Point, scalar: float) =
        { X = p.X * scalar; Y = p.Y * scalar; Z = p.Z * scalar }

/// Person record with mutable field
type Person =
    { Name: string
      mutable Age: int
      Email: string option }

/// Struct record
[<Struct>]
type PointStruct =
    { X: float
      Y: float }

/// Anonymous record usage (inline)
let anonymousRecord = {| Name = "Alice"; Age = 30 |}

// ============================================================================
// Discriminated Unions (Algebraic Data Types)
// ============================================================================

/// Result type for error handling
type Result<'T, 'TError> =
    | Ok of 'T
    | Error of 'TError

/// Option-like type
type Maybe<'T> =
    | Just of 'T
    | Nothing

/// Binary tree
type Tree<'T> =
    | Empty
    | Leaf of 'T
    | Node of Tree<'T> * 'T * Tree<'T>

/// Expression AST
type Expr =
    | Const of int
    | Var of string
    | Add of Expr * Expr
    | Mul of Expr * Expr
    | Let of string * Expr * Expr

// ============================================================================
// Classes and Interfaces
// ============================================================================

/// Interface definition
type IShape =
    abstract member Area: float
    abstract member Perimeter: float

/// Abstract class
[<AbstractClass>]
type Shape(name: string) =
    member val Name = name with get
    member val Color = Color.Red with get, set

    abstract member Area: float
    abstract member Perimeter: float

    default this.ToString() =
        sprintf "%s: Area=%.2f" this.Name this.Area

/// Concrete class
type Circle(radius: float) =
    inherit Shape("Circle")

    member val Radius = radius with get, set

    override this.Area = Math.PI * this.Radius * this.Radius
    override this.Perimeter = 2.0 * Math.PI * this.Radius

    interface IShape with
        member this.Area = this.Area
        member this.Perimeter = this.Perimeter

/// Another concrete class
type Rectangle(width: float, height: float) =
    inherit Shape("Rectangle")

    member val Width = width with get, set
    member val Height = height with get, set

    override this.Area = this.Width * this.Height
    override this.Perimeter = 2.0 * (this.Width + this.Height)

    interface IShape with
        member this.Area = this.Area
        member this.Perimeter = this.Perimeter

// ============================================================================
// Object Expressions
// ============================================================================

/// Create interface implementation inline
let createShape area perimeter =
    { new IShape with
        member _.Area = area
        member _.Perimeter = perimeter }

// ============================================================================
// Functions
// ============================================================================

/// Basic function
let add a b = a + b

/// Function with type annotations
let multiply (a: int) (b: int) : int = a * b

/// Curried function
let addCurried = fun a -> fun b -> a + b

/// Partial application
let add5 = add 5

/// Recursive function
let rec factorial n =
    if n <= 1 then 1
    else n * factorial (n - 1)

/// Tail-recursive function
let factorialTail n =
    let rec loop acc n =
        if n <= 1 then acc
        else loop (acc * n) (n - 1)
    loop 1 n

/// Mutually recursive functions
let rec isEven n =
    if n = 0 then true
    else isOdd (n - 1)

and isOdd n =
    if n = 0 then false
    else isEven (n - 1)

/// Higher-order function
let applyTwice f x = f (f x)

/// Function composition
let compose f g = fun x -> f (g x)

/// Pipe-forward and composition operators
let processNumbers numbers =
    numbers
    |> List.filter (fun x -> x % 2 = 0)
    |> List.map (fun x -> x * 2)
    |> List.sum

let incrementThenDouble = ((+) 1) >> ((*) 2)

/// Generic function
let inline genericAdd a b = a + b

/// Function with constraints
let inline printValue (x: 'T when 'T :> IFormattable) =
    printfn "%s" (x.ToString("N2", null))

/// Active patterns
let (|Even|Odd|) n =
    if n % 2 = 0 then Even else Odd

let (|DivisibleBy|_|) divisor n =
    if n % divisor = 0 then Some (n / divisor) else None

/// Parameterized active pattern
let (|ParseInt|_|) (str: string) =
    match Int32.TryParse(str) with
    | true, value -> Some value
    | false, _ -> None

/// Partial active pattern
let (|StartsWith|_|) (prefix: string) (str: string) =
    if str.StartsWith(prefix) then
        Some (str.Substring(prefix.Length))
    else None

// ============================================================================
// Pattern Matching
// ============================================================================

/// Match expression
let describe x =
    match x with
    | 0 -> "Zero"
    | 1 -> "One"
    | n when n > 0 -> sprintf "Positive: %d" n
    | n -> sprintf "Negative: %d" n

/// Pattern matching with discriminated unions
let colorToHex color =
    match color with
    | Red -> 0xFF0000
    | Green -> 0x00FF00
    | Blue -> 0x0000FF
    | Rgb (r, g, b) -> (r <<< 16) ||| (g <<< 8) ||| b
    | Hex h -> h

/// Pattern matching with records
let formatPoint point =
    match point with
    | { X = 0.0; Y = 0.0; Z = 0.0 } -> "Origin"
    | { X = x; Y = y; Z = 0.0 } -> sprintf "2D: (%.1f, %.1f)" x y
    | { X = x; Y = y; Z = z } -> sprintf "3D: (%.1f, %.1f, %.1f)" x y z

/// Pattern matching with lists
let rec sumList list =
    match list with
    | [] -> 0
    | head :: tail -> head + sumList tail

let describeList list =
    match list with
    | [] -> "Empty"
    | [x] -> sprintf "Single: %d" x
    | [x; y] -> sprintf "Pair: %d, %d" x y
    | head :: _ -> sprintf "Many, starts with: %d" head

/// Pattern matching with active patterns
let classifyNumber n =
    match n with
    | Even -> "Even"
    | Odd -> "Odd"

let parseInput input =
    match input with
    | ParseInt n -> sprintf "Integer: %d" n
    | StartsWith "hello" rest -> sprintf "Greeting: %s" rest
    | _ -> "Unknown"

/// Guards in pattern matching
let sign x =
    match x with
    | _ when x < 0 -> -1
    | _ when x > 0 -> 1
    | _ -> 0

// ============================================================================
// Collections
// ============================================================================

/// List operations
let listOperations () =
    // List literal
    let list = [1; 2; 3; 4; 5]

    // List comprehension
    let squares = [ for x in 1..10 -> x * x ]

    // Cons operator
    let extended = 0 :: list

    // Concatenation
    let combined = list @ [6; 7; 8]

    // List functions
    let mapped = List.map (fun x -> x * 2) list
    let filtered = List.filter (fun x -> x > 2) list
    let folded = List.fold (+) 0 list
    let reduced = List.reduce (+) list
    let zipped = List.zip list squares
    let grouped = List.groupBy (fun x -> x % 2) list

    // List slicing
    let slice = list.[1..3]

    list

/// Sequence operations (lazy)
let sequenceOperations () =
    // Sequence expression
    let seq1 = seq { 1..10 }

    // Infinite sequence
    let naturals = Seq.initInfinite id

    // Sequence comprehension
    let evens = seq {
        for x in 1..100 do
            if x % 2 = 0 then
                yield x
    }

    // Yield!
    let combined = seq {
        yield! [1; 2; 3]
        yield! [4; 5; 6]
    }

    // Lazy evaluation
    let first10 = naturals |> Seq.take 10 |> Seq.toList

    first10

/// Array operations
let arrayOperations () =
    // Array literal
    let arr = [| 1; 2; 3; 4; 5 |]

    // Array comprehension
    let squares = [| for x in 1..10 -> x * x |]

    // Array indexing
    let first = arr.[0]
    let last = arr.[arr.Length - 1]

    // Array slicing
    let slice = arr.[1..3]

    // Array mutation
    arr.[0] <- 10

    // Array2D
    let matrix = Array2D.init 3 3 (fun i j -> i * 3 + j)

    arr

/// Map and Set
let mapAndSetOperations () =
    // Map
    let map = Map.ofList [ ("one", 1); ("two", 2); ("three", 3) ]
    let value = Map.find "one" map
    let tryValue = Map.tryFind "four" map
    let updated = Map.add "four" 4 map

    // Set
    let set = Set.ofList [ 1; 2; 3; 3; 4 ]
    let hasValue = Set.contains 2 set
    let added = Set.add 5 set
    let union = Set.union set (Set.ofList [4; 5; 6])

    (map, set)

// ============================================================================
// Computation Expressions
// ============================================================================

/// Option computation expression
type OptionBuilder() =
    member _.Bind(x, f) = Option.bind f x
    member _.Return(x) = Some x
    member _.ReturnFrom(x) = x
    member _.Zero() = None

let option = OptionBuilder()

let divideOptions a b =
    option {
        let! x = a
        let! y = b
        if y = 0 then
            return! None
        else
            return x / y
    }

/// Result computation expression
type ResultBuilder() =
    member _.Bind(x, f) =
        match x with
        | Ok v -> f v
        | Error e -> Error e
    member _.Return(x) = Ok x
    member _.ReturnFrom(x) = x

let result = ResultBuilder()

let processResult () =
    result {
        let! x = Ok 10
        let! y = Ok 20
        return x + y
    }

/// Async workflows
let asyncOperation () = async {
    do! Async.Sleep 100
    let! result = async { return 42 }
    return result * 2
}

let parallelAsync () = async {
    let! results =
        [1..10]
        |> List.map (fun x -> async { return x * x })
        |> Async.Parallel
    return Array.sum results
}

// ============================================================================
// Units of Measure
// ============================================================================

[<Measure>] type m
[<Measure>] type s
[<Measure>] type kg
[<Measure>] type N = kg * m / s^2

let distance = 100.0<m>
let time = 10.0<s>
let speed = distance / time // float<m/s>

let mass = 10.0<kg>
let acceleration = 9.81<m/s^2>
let force : float<N> = mass * acceleration

// ============================================================================
// Type Providers (conceptual - requires actual provider)
// ============================================================================

// Example syntax (would need actual type provider):
// type CsvData = CsvProvider<"data.csv">
// let data = CsvData.GetSample()

// ============================================================================
// Quotations
// ============================================================================

open Microsoft.FSharp.Quotations

let expr = <@ 1 + 2 @>
let exprWithHole = <@ fun x -> x + 1 @>

// ============================================================================
// Main Program
// ============================================================================

[<EntryPoint>]
let main args =
    // Variable bindings
    let immutable = 42
    let mutable mutableVar = 0
    mutableVar <- mutableVar + 1

    // Type inference
    let inferredInt = 42
    let inferredString = "Hello"
    let inferredList = [1; 2; 3]

    // Explicit type annotation
    let explicitInt : int = 42

    // Numeric literals
    let decimal = 42
    let hex = 0xDEADBEEF
    let binary = 0b10101010
    let octal = 0o755
    let long = 123456789L
    let float32 = 3.14f
    let float64 = 3.14
    let bigint = 123456789I
    let decimal' = 3.14M

    // Strings
    let string = "Hello, World!"
    let verbatim = @"C:\Users\Name"
    let tripleQuoted = """This contains "quotes" easily"""
    let interpolated = $"Value: {immutable}"

    // Characters
    let char = 'A'
    let escaped = '\n'
    let unicode = '\u0041'

    // Unit type
    let unit = ()

    // Tuples
    let tuple = (1, "hello", 3.14)
    let (a, b, c) = tuple
    let first = fst (1, 2)
    let second = snd (1, 2)

    // Records
    let point = { X = 1.0; Y = 2.0; Z = 3.0 }
    let point2 = { point with X = 10.0 }
    let { X = px; Y = py; Z = pz } = point

    printfn "Point: %A" point
    printfn "Distance from origin: %.2f" (point.Distance Point.Origin)

    // Discriminated unions
    let color = Rgb (255, 128, 0)
    printfn "Color hex: %X" (colorToHex color)

    // Classes
    let circle = Circle(5.0)
    printfn "Circle area: %.2f" circle.Area

    let rect = Rectangle(4.0, 6.0)
    printfn "Rectangle area: %.2f" rect.Area

    // Pattern matching
    printfn "Describe 42: %s" (describe 42)
    printfn "Describe -5: %s" (describe -5)

    // Lists
    let numbers = [1..10]
    let evens = numbers |> List.filter (fun x -> x % 2 = 0)
    let doubled = numbers |> List.map (fun x -> x * 2)
    let sum = numbers |> List.sum

    printfn "Numbers: %A" numbers
    printfn "Evens: %A" evens
    printfn "Sum: %d" sum

    // Sequences (lazy)
    let lazySeq = seq { for i in 1..1000000 -> i * i }
    let first10 = lazySeq |> Seq.take 10 |> Seq.toList
    printfn "First 10 squares: %A" first10

    // Arrays
    let arr = [| 1; 2; 3; 4; 5 |]
    arr.[0] <- 10
    printfn "Array: %A" arr

    // Map and Set
    let map = Map.ofList [ ("a", 1); ("b", 2); ("c", 3) ]
    printfn "Map: %A" map

    // Option
    let someValue = Some 42
    let noValue = None

    match someValue with
    | Some x -> printfn "Got: %d" x
    | None -> printfn "Nothing"

    // Computation expressions
    let optResult = divideOptions (Some 10) (Some 2)
    printfn "Option result: %A" optResult

    // Higher-order functions
    let square x = x * x
    let result = applyTwice square 3
    printfn "Apply twice: %d" result

    // Lambda expressions
    let lambda = fun x y -> x + y
    let lambdaShort = (+)

    // Piping
    let processed =
        [1..100]
        |> List.filter (fun x -> x % 2 = 0)
        |> List.map (fun x -> x * 2)
        |> List.take 10
        |> List.sum

    printfn "Processed: %d" processed

    // Composition
    let incrementAndDouble = ((+) 1) >> ((*) 2)
    printfn "Increment and double 5: %d" (incrementAndDouble 5)

    // Async
    let asyncResult = asyncOperation () |> Async.RunSynchronously
    printfn "Async result: %d" asyncResult

    // Exception handling
    try
        failwith "Test error"
    with
    | :? ArgumentException as ex -> printfn "Argument error: %s" ex.Message
    | Failure msg -> printfn "Failure: %s" msg
    | ex -> printfn "Error: %s" ex.Message

    // Lazy evaluation
    let lazyValue = lazy (printfn "Computing..."; 42)
    let forcedValue = lazyValue.Force()
    printfn "Lazy value: %d" forcedValue

    // Memoization
    let memoizedFib =
        let cache = Dictionary<int, int>()
        let rec fib n =
            match cache.TryGetValue(n) with
            | true, v -> v
            | false, _ ->
                let v = if n < 2 then n else fib (n-1) + fib (n-2)
                cache.[n] <- v
                v
        fib

    printfn "Fib 10: %d" (memoizedFib 10)

    // Units of measure
    printfn "Speed: %A" speed
    printfn "Force: %A" force

    // Active patterns
    printfn "Classify 4: %s" (classifyNumber 4)
    printfn "Classify 5: %s" (classifyNumber 5)

    // Object expressions
    let customShape = createShape 100.0 40.0
    printfn "Custom shape area: %.2f" customShape.Area

    // Quotations
    printfn "Quotation: %A" expr

    // Printf formatting
    printfn "Formatted: %d, %.2f, %s" 42 3.14 "hello"
    sprintf "Sprintf: %d" 42 |> printfn "%s"

    printfn "%s" Greeting
    printfn "Program completed successfully!"

    0 // Return exit code
