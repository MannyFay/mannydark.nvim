# Comprehensive Nim language sample demonstrating all syntax features
# Nim is a statically typed, systems programming language

## Module documentation
## This module demonstrates Nim's syntax and features

# Import statements
import std/[strutils, strformat, sequtils, tables, sets, options]
import std/[algorithm, sugar, math, times, os, streams]
import std/[json, parseutils, re, unicode, hashes]
import std/[asyncdispatch, httpclient, threadpool]
import std/[macros, typetraits, enumerate]

# Export symbols
export strutils.toLower, strutils.toUpper

# ============================================================================
# Constants and Compile-time Values
# ============================================================================

const
  MaxBufferSize = 1024
  Pi = 3.14159265358979323846
  Greeting = "Hello, Nim!"
  Version = (major: 1, minor: 6, patch: 0)

# Compile-time computation
const ComputedValue = block:
  var result = 0
  for i in 0 ..< 10:
    result += i
  result

# Static assertions
static:
  assert sizeof(int) >= 4, "int must be at least 4 bytes"
  assert ComputedValue == 45

# ============================================================================
# Type Declarations
# ============================================================================

# Enumeration
type
  Color = enum
    Red = "red"
    Green = "green"
    Blue = "blue"

  Status = enum
    Ok = 0
    Error = 1
    Pending = 2

  # Flags set
  Permission = enum
    Read, Write, Execute

  Permissions = set[Permission]

# Distinct types
type
  Meters = distinct float
  Seconds = distinct float
  UserId = distinct int

# Object types
type
  Point = object
    x, y, z: float

  # Ref object (reference type)
  Person = ref object
    name: string
    age: int
    email: string

  # Inheritance
  Shape = ref object of RootObj
    name: string
    color: Color

  Circle = ref object of Shape
    radius: float

  Rectangle = ref object of Shape
    width, height: float

# Variant types (case objects)
type
  NodeKind = enum
    nkInt, nkFloat, nkString, nkList

  Node = ref object
    case kind: NodeKind
    of nkInt:
      intVal: int
    of nkFloat:
      floatVal: float
    of nkString:
      strVal: string
    of nkList:
      children: seq[Node]

# Generic types
type
  Container[T] = ref object
    items: seq[T]

  Pair[K, V] = object
    key: K
    value: V

  Result[T, E] = object
    case isOk: bool
    of true:
      value: T
    of false:
      error: E

# Type aliases
type
  StringList = seq[string]
  Callback = proc(x: int): int
  AsyncCallback = proc(x: int): Future[int]

# Tuple types
type
  Coordinate = tuple[x, y: float]
  NamedTuple = tuple[name: string, age: int]

# ============================================================================
# Procedures (Functions)
# ============================================================================

# Basic procedure
proc add(a, b: int): int =
  result = a + b

# With explicit return
proc multiply(a, b: int): int =
  return a * b

# Procedure with var parameter (mutable reference)
proc increment(x: var int) =
  x += 1

# Procedure with default parameters
proc greet(name: string, greeting = "Hello"): string =
  fmt"{greeting}, {name}!"

# Procedure with result variable
proc factorial(n: int): int =
  result = 1
  for i in 2 .. n:
    result *= i

# Generic procedure
proc max[T](a, b: T): T =
  if a > b: a else: b

proc min[T: SomeNumber](a, b: T): T =
  if a < b: a else: b

# Procedure with type constraints
proc sum[T: SomeNumber](items: openArray[T]): T =
  result = T(0)
  for item in items:
    result += item

# Variadic procedure
proc print(args: varargs[string, `$`]) =
  for arg in args:
    stdout.write(arg, " ")
  stdout.writeLine("")

# Iterator
iterator countUp(a, b: int): int =
  var i = a
  while i <= b:
    yield i
    inc i

# Closure iterator
iterator items[T](c: Container[T]): T =
  for item in c.items:
    yield item

# Inline iterator
iterator pairs[T](c: Container[T]): (int, T) {.inline.} =
  for i, item in c.items:
    yield (i, item)

# Converter
converter toFloat(x: int): float = float(x)

# Template
template withLock(lock: Lock, body: untyped) =
  acquire(lock)
  try:
    body
  finally:
    release(lock)

template benchmark(name: string, body: untyped) =
  let start = cpuTime()
  body
  let elapsed = cpuTime() - start
  echo fmt"{name} took {elapsed:.3f} seconds"

# Macro
macro debug(args: varargs[untyped]): untyped =
  result = newStmtList()
  for arg in args:
    result.add quote do:
      echo astToStr(`arg`), " = ", `arg`

macro generateGetters(T: typedesc): untyped =
  result = newStmtList()
  let impl = getImpl(T)
  for field in impl[2][2]:
    let fieldName = field[0]
    let getter = ident("get" & fieldName.strVal.capitalizeAscii)
    result.add quote do:
      proc `getter`(self: `T`): auto = self.`fieldName`

# ============================================================================
# Object Methods
# ============================================================================

# Constructor
proc newPoint(x, y, z = 0.0): Point =
  Point(x: x, y: y, z: z)

proc newPerson(name: string, age: int): Person =
  Person(name: name, age: age)

proc newCircle(radius: float, color = Red): Circle =
  Circle(name: "Circle", radius: radius, color: color)

# Methods using UFCS (Uniform Function Call Syntax)
proc distance(p1, p2: Point): float =
  let dx = p1.x - p2.x
  let dy = p1.y - p2.y
  let dz = p1.z - p2.z
  sqrt(dx*dx + dy*dy + dz*dz)

proc scale(p: var Point, factor: float) =
  p.x *= factor
  p.y *= factor
  p.z *= factor

proc `+`(a, b: Point): Point =
  Point(x: a.x + b.x, y: a.y + b.y, z: a.z + b.z)

proc `-`(a, b: Point): Point =
  Point(x: a.x - b.x, y: a.y - b.y, z: a.z - b.z)

proc `*`(p: Point, scalar: float): Point =
  Point(x: p.x * scalar, y: p.y * scalar, z: p.z * scalar)

proc `$`(p: Point): string =
  fmt"Point({p.x}, {p.y}, {p.z})"

# Virtual methods using method
method area(s: Shape): float {.base.} =
  raise newException(CatchableError, "Abstract method")

method area(c: Circle): float =
  Pi * c.radius * c.radius

method area(r: Rectangle): float =
  r.width * r.height

method perimeter(s: Shape): float {.base.} =
  raise newException(CatchableError, "Abstract method")

method perimeter(c: Circle): float =
  2 * Pi * c.radius

method perimeter(r: Rectangle): float =
  2 * (r.width + r.height)

method draw(s: Shape) {.base.} =
  echo fmt"Drawing {s.name}"

method draw(c: Circle) =
  procCall draw(Shape(c))
  echo fmt"  radius: {c.radius}"

# ============================================================================
# Generic Container Implementation
# ============================================================================

proc newContainer[T](): Container[T] =
  Container[T](items: @[])

proc add[T](c: Container[T], item: T) =
  c.items.add(item)

proc get[T](c: Container[T], index: int): T =
  c.items[index]

proc len[T](c: Container[T]): int =
  c.items.len

proc `[]`[T](c: Container[T], index: int): T =
  c.items[index]

proc `[]=`[T](c: Container[T], index: int, value: T) =
  c.items[index] = value

# ============================================================================
# Error Handling
# ============================================================================

type
  AppError = object of CatchableError
  FileNotFoundError = object of AppError
  ValidationError = object of AppError

proc validateAge(age: int): int {.raises: [ValidationError].} =
  if age < 0 or age > 150:
    raise newException(ValidationError, "Invalid age")
  age

proc readConfigFile(path: string): string {.raises: [IOError, OSError].} =
  if not fileExists(path):
    raise newException(IOError, "File not found: " & path)
  readFile(path)

proc safeDivide(a, b: int): Option[int] =
  if b == 0:
    none(int)
  else:
    some(a div b)

# Result type pattern
proc parseNumber(s: string): Result[int, string] =
  try:
    Result[int, string](isOk: true, value: parseInt(s))
  except ValueError:
    Result[int, string](isOk: false, error: "Invalid number: " & s)

# ============================================================================
# Async/Await
# ============================================================================

proc fetchData(url: string): Future[string] {.async.} =
  let client = newAsyncHttpClient()
  defer: client.close()
  let response = await client.get(url)
  return await response.body

proc parallelFetch(urls: seq[string]): Future[seq[string]] {.async.} =
  var futures: seq[Future[string]] = @[]
  for url in urls:
    futures.add(fetchData(url))
  result = await all(futures)

proc asyncExample() {.async.} =
  echo "Starting async operation..."
  await sleepAsync(100)
  echo "Async operation complete!"

# ============================================================================
# Threading and Parallelism
# ============================================================================

proc threadedWork(data: int) {.thread.} =
  echo fmt"Processing {data} in thread"

proc parallelMap[T, U](items: seq[T], f: proc(x: T): U): seq[U] =
  result = newSeq[U](items.len)
  parallel:
    for i in 0 ..< items.len:
      spawn f(items[i])
      result[i] = ^spawn f(items[i])

# ============================================================================
# Metaprogramming
# ============================================================================

# Compile-time type introspection
proc printFields(T: typedesc) =
  for name, value in T().fieldPairs:
    echo name, ": ", typeof(value)

# Generate code at compile time
macro createEnum(name: untyped, values: varargs[untyped]): untyped =
  var enumDef = nnkEnumTy.newTree(newEmptyNode())
  for value in values:
    enumDef.add(value)
  result = nnkTypeDef.newTree(
    name,
    newEmptyNode(),
    enumDef
  )

# Quote and unquote
macro logCall(call: untyped): untyped =
  let callStr = call.repr
  result = quote do:
    echo "Calling: ", `callStr`
    `call`

# ============================================================================
# Effects System
# ============================================================================

proc pureFunction(x: int): int {.noSideEffect.} =
  x * 2

func pureFunctionAlt(x: int): int =
  x * 2

proc mayRaise(): int {.raises: [IOError].} =
  if true:
    raise newException(IOError, "Error")
  42

proc noRaise(): int {.raises: [].} =
  42

# ============================================================================
# Pragmas and Attributes
# ============================================================================

{.push inline.}
proc fastAdd(a, b: int): int = a + b
proc fastMul(a, b: int): int = a * b
{.pop.}

proc deprecatedProc() {.deprecated: "Use newProc instead".} =
  discard

proc exportedProc() {.exportc: "nim_exported_proc".} =
  echo "Exported to C"

proc importedProc(x: cint): cint {.importc: "abs", header: "<stdlib.h>".}

{.emit: """
// Inline C code
int custom_c_function(int x) {
    return x * 2;
}
""".}

# ============================================================================
# Main Program
# ============================================================================

when isMainModule:
  # Variable declarations
  var mutableInt = 42
  let immutableInt = 100
  const compileTimeInt = 200

  # Type inference
  var inferredInt = 42
  var inferredString = "hello"
  var inferredSeq = @[1, 2, 3]

  # Numeric literals
  let decimal = 42
  let hex = 0xDEAD_BEEF
  let octal = 0o755
  let binary = 0b1010_1010
  let floatVal = 3.14
  let scientific = 1.23e-4
  let underscored = 1_000_000

  # String literals
  let singleLine = "Hello, World!"
  let multiLine = """
    This is a
    multi-line string
  """
  let rawString = r"Raw \n string"
  let fmtString = fmt"Value: {mutableInt}"
  let interpolated = &"Interpolated: {mutableInt}"

  # Character literals
  let charVal = 'A'
  let escapeChar = '\n'
  let unicodeChar = '\u0041'

  # Sequences (dynamic arrays)
  var numbers = @[1, 2, 3, 4, 5]
  numbers.add(6)
  let first = numbers[0]
  let slice = numbers[1..3]
  let length = numbers.len

  # Arrays (fixed size)
  var fixedArray: array[5, int] = [1, 2, 3, 4, 5]
  fixedArray[0] = 10

  # Tables (hash maps)
  var table = initTable[string, int]()
  table["one"] = 1
  table["two"] = 2
  if "one" in table:
    echo table["one"]

  # Sets
  var numberSet = initHashSet[int]()
  numberSet.incl(1)
  numberSet.incl(2)
  if 1 in numberSet:
    echo "1 is in set"

  # Tuples
  let coord: Coordinate = (x: 1.0, y: 2.0)
  let (x, y) = coord
  echo fmt"x={x}, y={y}"

  # Objects
  var point = newPoint(1, 2, 3)
  echo point
  point.scale(2)
  echo point

  let person = newPerson("Alice", 30)
  echo fmt"{person.name} is {person.age} years old"

  # Polymorphism
  let shapes: seq[Shape] = @[
    Circle(name: "Circle", radius: 5, color: Red),
    Rectangle(name: "Rectangle", width: 4, height: 6, color: Blue)
  ]

  for shape in shapes:
    shape.draw()
    echo fmt"  Area: {shape.area()}"

  # Control flow
  if mutableInt > 0:
    echo "Positive"
  elif mutableInt < 0:
    echo "Negative"
  else:
    echo "Zero"

  # Case statement
  case mutableInt
  of 0:
    echo "Zero"
  of 1..10:
    echo "1-10"
  of 42:
    echo "The answer"
  else:
    echo "Other"

  # When (compile-time if)
  when defined(windows):
    echo "Windows"
  elif defined(linux):
    echo "Linux"
  elif defined(macosx):
    echo "macOS"

  # Loops
  for i in 0 ..< 10:
    echo i

  for i in countdown(10, 0):
    echo i

  for item in numbers:
    echo item

  for i, item in numbers:
    echo fmt"[{i}] = {item}"

  var counter = 0
  while counter < 5:
    echo counter
    inc counter

  # Block expressions
  let blockResult = block:
    var temp = 0
    for i in 1..10:
      temp += i
    temp

  # Exception handling
  try:
    let age = validateAge(25)
    echo fmt"Valid age: {age}"
  except ValidationError as e:
    echo fmt"Validation error: {e.msg}"
  except CatchableError:
    echo "Unknown error"
  finally:
    echo "Cleanup"

  # Option handling
  let maybeInt = safeDivide(10, 2)
  if maybeInt.isSome:
    echo fmt"Result: {maybeInt.get}"

  # Defer
  block:
    defer: echo "Deferred cleanup"
    echo "Main code"

  # Closures
  let multiplier = 5
  let multiply = proc(x: int): int = x * multiplier

  echo multiply(10)

  # Higher-order functions
  let doubled = numbers.map(x => x * 2)
  let evens = numbers.filter(x => x mod 2 == 0)
  let sumVal = numbers.foldl(a + b)

  echo doubled
  echo evens
  echo sumVal

  # Method chaining
  let processed = @[1, 2, 3, 4, 5]
    .filter(x => x mod 2 == 0)
    .map(x => x * 2)
    .foldl(a + b)

  echo processed

  # Iterators
  for i in countUp(1, 5):
    echo i

  # Container usage
  let container = newContainer[int]()
  container.add(1)
  container.add(2)
  container.add(3)

  for item in container:
    echo item

  # Generic functions
  echo max(3, 5)
  echo min(3.14, 2.71)

  # Templates
  benchmark "loop":
    var total = 0
    for i in 0 ..< 1_000_000:
      total += i

  # Macros
  debug mutableInt, inferredString

  # JSON
  let jsonData = %* {
    "name": "John",
    "age": 30,
    "hobbies": ["reading", "gaming"]
  }
  echo jsonData.pretty()

  # Regex
  let pattern = re"\d+"
  if "hello123world".contains(pattern):
    echo "Found digits"

  # Date/Time
  let now = now()
  echo fmt"Current time: {now}"

  # File I/O
  writeFile("test.txt", "Hello, File!")
  let content = readFile("test.txt")
  echo content
  removeFile("test.txt")

  # Command line args
  for arg in commandLineParams():
    echo fmt"Arg: {arg}"

  # Environment
  let home = getEnv("HOME", "/tmp")
  echo fmt"Home: {home}"

  # Distinct types
  let distance: Meters = Meters(100.0)
  let duration: Seconds = Seconds(10.0)

  # Type conversions
  let intVal = 42
  let floatVal2 = float(intVal)
  let strVal = $intVal

  echo fmt"int: {intVal}, float: {floatVal2}, str: {strVal}"

  echo "Program completed successfully!"
