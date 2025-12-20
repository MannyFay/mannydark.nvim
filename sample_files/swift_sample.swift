// ==============================================================================
// Comprehensive Swift Sample - Syntax Highlighting Demonstration
// ==============================================================================

// This file demonstrates all major Swift language features
// for syntax highlighting purposes.

import Foundation
import Combine
import SwiftUI

// ==============================================================================
// Comments
// ==============================================================================

// Single line comment

/* Multi-line
   block comment */

/// Documentation comment for functions/types
/// - Parameter x: The input value
/// - Returns: The processed result

// MARK: - Section Marker
// TODO: Something to do
// FIXME: Something to fix

// ==============================================================================
// Variables and Constants
// ==============================================================================

// Constants
let constantInt: Int = 42
let constantDouble = 3.14159
let constantString = "Hello, Swift!"

// Variables
var mutableInt = 100
var mutableString: String = "Mutable"

// Type inference
let inferredInt = 42
let inferredDouble = 3.14
let inferredArray = [1, 2, 3, 4, 5]

// Numeric literals
let decimal = 17
let binary = 0b10001
let octal = 0o21
let hexadecimal = 0x11
let exponent = 1.25e2
let hexFloat = 0xC.3p0

// Underscores for readability
let million = 1_000_000
let binaryReadable = 0b1010_1010_1010

// Unicode
let emoji = "🚀"
let unicode = "\u{1F600}"

// ==============================================================================
// Basic Types
// ==============================================================================

// Integer types
let int8: Int8 = 127
let int16: Int16 = 32767
let int32: Int32 = 2147483647
let int64: Int64 = 9223372036854775807
let uInt: UInt = 100

// Floating point
let float: Float = 3.14
let double: Double = 3.14159265358979

// Boolean
let isTrue: Bool = true
let isFalse = false

// Character
let char: Character = "A"
let unicodeChar: Character = "🎉"

// String
let string = "Hello, World!"
let multilineString = """
    This is a multi-line
    string literal in Swift.
    It preserves formatting.
    """

// String interpolation
let name = "Swift"
let greeting = "Hello, \(name)!"
let expression = "2 + 2 = \(2 + 2)"

// ==============================================================================
// Optionals
// ==============================================================================

var optionalInt: Int? = 42
var optionalString: String? = nil

// Optional unwrapping
if let unwrapped = optionalInt {
    print("Value: \(unwrapped)")
}

// Guard statement
func processValue(_ value: Int?) {
    guard let unwrapped = value else {
        print("No value")
        return
    }
    print("Value: \(unwrapped)")
}

// Force unwrapping (use with caution!)
let forced = optionalInt!

// Optional chaining
let count = optionalString?.count

// Nil coalescing
let defaultValue = optionalString ?? "Default"

// Implicitly unwrapped optional
var implicitOptional: String! = "Implicit"

// ==============================================================================
// Collections
// ==============================================================================

// Arrays
var arrayInt: [Int] = [1, 2, 3, 4, 5]
var arrayString = ["a", "b", "c"]
var emptyArray: [Double] = []
var repeatedArray = Array(repeating: 0, count: 5)

// Array operations
arrayInt.append(6)
arrayInt.insert(0, at: 0)
arrayInt.remove(at: 0)
let firstElement = arrayInt.first
let lastElement = arrayInt.last
let sliced = arrayInt[1...3]

// Dictionaries
var dictionary: [String: Int] = ["one": 1, "two": 2, "three": 3]
var emptyDict: [Int: String] = [:]

dictionary["four"] = 4
let value = dictionary["one"]
dictionary.removeValue(forKey: "one")

// Sets
var setInt: Set<Int> = [1, 2, 3, 4, 5]
var setString: Set = ["a", "b", "c"]

setInt.insert(6)
setInt.remove(1)
let contains = setInt.contains(2)

// Set operations
let setA: Set = [1, 2, 3]
let setB: Set = [3, 4, 5]
let union = setA.union(setB)
let intersection = setA.intersection(setB)
let difference = setA.subtracting(setB)

// ==============================================================================
// Tuples
// ==============================================================================

let simpleTuple = (1, "hello")
let namedTuple = (x: 10, y: 20)

// Accessing tuple elements
let first = simpleTuple.0
let second = simpleTuple.1
let xValue = namedTuple.x

// Decomposition
let (a, b) = simpleTuple
let (x, _) = namedTuple  // Ignore second element

// ==============================================================================
// Control Flow
// ==============================================================================

// If statement
let score = 85
if score >= 90 {
    print("A")
} else if score >= 80 {
    print("B")
} else if score >= 70 {
    print("C")
} else {
    print("F")
}

// Ternary operator
let result = score >= 60 ? "Pass" : "Fail"

// Switch statement
let letter = "a"
switch letter {
case "a", "e", "i", "o", "u":
    print("Vowel")
case "b"..."z":
    print("Consonant")
default:
    print("Other")
}

// Switch with value binding
let point = (2, 0)
switch point {
case (0, 0):
    print("Origin")
case (let x, 0):
    print("On x-axis at \(x)")
case (0, let y):
    print("On y-axis at \(y)")
case let (x, y) where x == y:
    print("On diagonal")
case let (x, y):
    print("At (\(x), \(y))")
}

// ==============================================================================
// Loops
// ==============================================================================

// For-in loop
for i in 1...5 {
    print(i)
}

for i in 0..<5 {
    print(i)
}

for item in arrayInt {
    print(item)
}

for (key, value) in dictionary {
    print("\(key): \(value)")
}

for (index, value) in arrayInt.enumerated() {
    print("\(index): \(value)")
}

// While loop
var counter = 0
while counter < 5 {
    print(counter)
    counter += 1
}

// Repeat-while (do-while)
repeat {
    print(counter)
    counter -= 1
} while counter > 0

// Control transfer
for i in 1...10 {
    if i == 3 {
        continue
    }
    if i == 8 {
        break
    }
    print(i)
}

// Labeled statements
outerLoop: for i in 1...3 {
    for j in 1...3 {
        if i * j > 6 {
            break outerLoop
        }
        print("\(i) * \(j) = \(i * j)")
    }
}

// ==============================================================================
// Functions
// ==============================================================================

// Basic function
func sayHello() {
    print("Hello!")
}

// Function with parameters
func greet(person: String) {
    print("Hello, \(person)!")
}

// Function with return value
func add(_ a: Int, _ b: Int) -> Int {
    return a + b
}

// Multiple return values (tuple)
func minMax(_ array: [Int]) -> (min: Int, max: Int)? {
    guard !array.isEmpty else { return nil }
    return (array.min()!, array.max()!)
}

// Argument labels
func greet(person: String, from hometown: String) {
    print("Hello \(person) from \(hometown)!")
}

// Default parameters
func greet(_ name: String, greeting: String = "Hello") {
    print("\(greeting), \(name)!")
}

// Variadic parameters
func sum(_ numbers: Int...) -> Int {
    return numbers.reduce(0, +)
}

// Inout parameters
func swapValues(_ a: inout Int, _ b: inout Int) {
    let temp = a
    a = b
    b = temp
}

// Function types
typealias MathOperation = (Int, Int) -> Int

let operation: MathOperation = add

func applyOperation(_ a: Int, _ b: Int, operation: MathOperation) -> Int {
    return operation(a, b)
}

// Nested functions
func chooseOperation(isAddition: Bool) -> MathOperation {
    func add(_ a: Int, _ b: Int) -> Int { a + b }
    func subtract(_ a: Int, _ b: Int) -> Int { a - b }
    return isAddition ? add : subtract
}

// ==============================================================================
// Closures
// ==============================================================================

// Closure expression
let multiply: (Int, Int) -> Int = { (a: Int, b: Int) -> Int in
    return a * b
}

// Shortened forms
let multiply2 = { (a: Int, b: Int) in a * b }
let multiply3: (Int, Int) -> Int = { $0 * $1 }

// Trailing closure
let sorted = arrayInt.sorted { $0 > $1 }

// Multiple trailing closures
func performOperation(
    input: Int,
    transform: (Int) -> Int,
    completion: (Int) -> Void
) {
    let result = transform(input)
    completion(result)
}

performOperation(input: 5) { value in
    value * 2
} completion: { result in
    print("Result: \(result)")
}

// Capturing values
func makeIncrementer(increment: Int) -> () -> Int {
    var total = 0
    return {
        total += increment
        return total
    }
}

// Escaping closures
func asyncOperation(completion: @escaping (String) -> Void) {
    DispatchQueue.main.async {
        completion("Done")
    }
}

// Autoclosure
func logIfTrue(_ condition: @autoclosure () -> Bool) {
    if condition() {
        print("True")
    }
}

// ==============================================================================
// Enumerations
// ==============================================================================

enum Direction {
    case north
    case south
    case east
    case west
}

enum Planet: Int {
    case mercury = 1
    case venus
    case earth
    case mars
}

// Associated values
enum Barcode {
    case upc(Int, Int, Int, Int)
    case qrCode(String)
}

let productCode = Barcode.upc(8, 85909, 51226, 3)
let qrCode = Barcode.qrCode("ABCDEFG")

switch productCode {
case .upc(let numberSystem, let manufacturer, let product, let check):
    print("UPC: \(numberSystem)-\(manufacturer)-\(product)-\(check)")
case .qrCode(let code):
    print("QR Code: \(code)")
}

// Enum with methods
enum CompassPoint: CaseIterable {
    case north, south, east, west

    var description: String {
        switch self {
        case .north: return "North"
        case .south: return "South"
        case .east: return "East"
        case .west: return "West"
        }
    }

    mutating func turnRight() {
        switch self {
        case .north: self = .east
        case .east: self = .south
        case .south: self = .west
        case .west: self = .north
        }
    }
}

// Recursive enumerations
indirect enum ArithmeticExpression {
    case number(Int)
    case addition(ArithmeticExpression, ArithmeticExpression)
    case multiplication(ArithmeticExpression, ArithmeticExpression)
}

// ==============================================================================
// Structures
// ==============================================================================

struct Point {
    var x: Double
    var y: Double

    // Computed property
    var magnitude: Double {
        return sqrt(x * x + y * y)
    }

    // Property observer
    var label: String = "" {
        willSet {
            print("About to change from \(label) to \(newValue)")
        }
        didSet {
            print("Changed from \(oldValue) to \(label)")
        }
    }

    // Static property
    static let origin = Point(x: 0, y: 0)

    // Methods
    func distance(to other: Point) -> Double {
        let dx = x - other.x
        let dy = y - other.y
        return sqrt(dx * dx + dy * dy)
    }

    // Mutating method
    mutating func moveBy(dx: Double, dy: Double) {
        x += dx
        y += dy
    }

    // Static method
    static func midpoint(_ p1: Point, _ p2: Point) -> Point {
        return Point(x: (p1.x + p2.x) / 2, y: (p1.y + p2.y) / 2)
    }
}

// ==============================================================================
// Classes
// ==============================================================================

class Animal {
    var name: String
    var age: Int

    init(name: String, age: Int) {
        self.name = name
        self.age = age
    }

    deinit {
        print("\(name) is being deinitialized")
    }

    func speak() {
        print("\(name) makes a sound")
    }
}

class Dog: Animal {
    var breed: String

    init(name: String, age: Int, breed: String) {
        self.breed = breed
        super.init(name: name, age: age)
    }

    override func speak() {
        print("\(name) barks")
    }

    func fetch() {
        print("\(name) is fetching")
    }
}

final class Cat: Animal {
    var isIndoor: Bool

    init(name: String, age: Int, isIndoor: Bool = true) {
        self.isIndoor = isIndoor
        super.init(name: name, age: age)
    }

    override func speak() {
        print("\(name) meows")
    }
}

// ==============================================================================
// Protocols
// ==============================================================================

protocol Drawable {
    func draw()
}

protocol Printable {
    var description: String { get }
    func printInfo()
}

protocol Identifiable {
    associatedtype ID: Hashable
    var id: ID { get }
}

// Protocol with default implementation
extension Printable {
    func printInfo() {
        print(description)
    }
}

// Class conforming to protocols
class Shape: Drawable, Printable {
    var name: String

    init(name: String) {
        self.name = name
    }

    func draw() {
        print("Drawing \(name)")
    }

    var description: String {
        return "Shape: \(name)"
    }
}

// Protocol composition
func displayAndDraw(_ item: Drawable & Printable) {
    item.draw()
    item.printInfo()
}

// ==============================================================================
// Extensions
// ==============================================================================

extension Int {
    var squared: Int {
        return self * self
    }

    func times(_ action: () -> Void) {
        for _ in 0..<self {
            action()
        }
    }

    mutating func increment() {
        self += 1
    }
}

extension Array where Element: Numeric {
    var total: Element {
        return reduce(0, +)
    }
}

// ==============================================================================
// Generics
// ==============================================================================

func swapGeneric<T>(_ a: inout T, _ b: inout T) {
    let temp = a
    a = b
    b = temp
}

struct Stack<Element> {
    private var items: [Element] = []

    mutating func push(_ item: Element) {
        items.append(item)
    }

    mutating func pop() -> Element? {
        return items.isEmpty ? nil : items.removeLast()
    }

    var isEmpty: Bool {
        return items.isEmpty
    }

    var count: Int {
        return items.count
    }
}

// Generic constraints
func findIndex<T: Equatable>(of value: T, in array: [T]) -> Int? {
    for (index, item) in array.enumerated() {
        if item == value {
            return index
        }
    }
    return nil
}

// Where clause
func allEqual<C: Collection>(_ collection: C) -> Bool where C.Element: Equatable {
    guard let first = collection.first else { return true }
    return collection.allSatisfy { $0 == first }
}

// ==============================================================================
// Error Handling
// ==============================================================================

enum NetworkError: Error {
    case invalidURL
    case noConnection
    case timeout
    case serverError(code: Int)
}

func fetchData(from url: String) throws -> Data {
    guard url.hasPrefix("https://") else {
        throw NetworkError.invalidURL
    }
    // ... fetch data
    return Data()
}

// Handling errors
do {
    let data = try fetchData(from: "https://example.com")
    print("Got \(data.count) bytes")
} catch NetworkError.invalidURL {
    print("Invalid URL")
} catch NetworkError.serverError(let code) {
    print("Server error: \(code)")
} catch {
    print("Unknown error: \(error)")
}

// Optional try
let data1 = try? fetchData(from: "invalid")
let data2 = try! fetchData(from: "https://example.com")

// Defer
func processFile() {
    print("Opening file")
    defer {
        print("Closing file")
    }
    print("Processing file")
}

// ==============================================================================
// Concurrency (async/await)
// ==============================================================================

func fetchUser(id: Int) async throws -> String {
    try await Task.sleep(nanoseconds: 1_000_000_000)
    return "User \(id)"
}

func fetchMultipleUsers() async throws -> [String] {
    async let user1 = fetchUser(id: 1)
    async let user2 = fetchUser(id: 2)
    async let user3 = fetchUser(id: 3)

    return try await [user1, user2, user3]
}

// Actor
actor Counter {
    private var value = 0

    func increment() {
        value += 1
    }

    func getValue() -> Int {
        return value
    }
}

// Task
func performTask() {
    Task {
        do {
            let users = try await fetchMultipleUsers()
            print(users)
        } catch {
            print("Error: \(error)")
        }
    }
}

// ==============================================================================
// Property Wrappers
// ==============================================================================

@propertyWrapper
struct Clamped<Value: Comparable> {
    private var value: Value
    private let range: ClosedRange<Value>

    var wrappedValue: Value {
        get { value }
        set { value = min(max(newValue, range.lowerBound), range.upperBound) }
    }

    init(wrappedValue: Value, _ range: ClosedRange<Value>) {
        self.range = range
        self.value = min(max(wrappedValue, range.lowerBound), range.upperBound)
    }
}

struct Settings {
    @Clamped(0...100) var volume: Int = 50
    @Clamped(0.0...1.0) var brightness: Double = 0.5
}

// ==============================================================================
// Result Builders
// ==============================================================================

@resultBuilder
struct StringBuilder {
    static func buildBlock(_ components: String...) -> String {
        components.joined(separator: "\n")
    }

    static func buildOptional(_ component: String?) -> String {
        component ?? ""
    }

    static func buildEither(first component: String) -> String {
        component
    }

    static func buildEither(second component: String) -> String {
        component
    }
}

@StringBuilder
func buildDocument(includeFooter: Bool) -> String {
    "Header"
    "Content"
    if includeFooter {
        "Footer"
    }
}

// ==============================================================================
// SwiftUI Preview
// ==============================================================================

struct ContentView: View {
    @State private var count = 0
    @Binding var text: String

    var body: some View {
        VStack {
            Text("Count: \(count)")
                .font(.largeTitle)
                .foregroundColor(.blue)

            Button("Increment") {
                count += 1
            }
            .padding()
            .background(Color.blue)
            .foregroundColor(.white)
            .cornerRadius(10)
        }
    }
}

// ==============================================================================
// Memory Management
// ==============================================================================

class Person {
    let name: String
    var apartment: Apartment?

    init(name: String) {
        self.name = name
    }

    deinit {
        print("\(name) is deinitialized")
    }
}

class Apartment {
    let unit: String
    weak var tenant: Person?  // Weak reference

    init(unit: String) {
        self.unit = unit
    }
}

// Unowned reference
class Customer {
    let name: String
    var card: CreditCard?

    init(name: String) {
        self.name = name
    }
}

class CreditCard {
    let number: UInt64
    unowned let customer: Customer  // Unowned reference

    init(number: UInt64, customer: Customer) {
        self.number = number
        self.customer = customer
    }
}

// Capture lists
class ViewController {
    var handler: (() -> Void)?

    func setup() {
        handler = { [weak self] in
            guard let self = self else { return }
            self.doSomething()
        }
    }

    func doSomething() {}
}

// ==============================================================================
// Operators
// ==============================================================================

// Custom operator
prefix operator +++
postfix operator +++

prefix func +++(value: inout Int) -> Int {
    value += 2
    return value
}

postfix func +++(value: inout Int) -> Int {
    let original = value
    value += 2
    return original
}

// Operator overloading
struct Vector2D {
    var x: Double
    var y: Double

    static func + (left: Vector2D, right: Vector2D) -> Vector2D {
        return Vector2D(x: left.x + right.x, y: left.y + right.y)
    }

    static func += (left: inout Vector2D, right: Vector2D) {
        left = left + right
    }

    static prefix func - (vector: Vector2D) -> Vector2D {
        return Vector2D(x: -vector.x, y: -vector.y)
    }
}

// ==============================================================================
// Access Control
// ==============================================================================

public class PublicClass {
    public var publicProperty: Int = 0
    internal var internalProperty: Int = 0
    fileprivate var fileprivateProperty: Int = 0
    private var privateProperty: Int = 0

    public init() {}

    public func publicMethod() {}
    internal func internalMethod() {}
    fileprivate func fileprivateMethod() {}
    private func privateMethod() {}
}

// ==============================================================================
// Macros (Swift 5.9+)
// ==============================================================================

// Example macro usage
// @Observable
// class Model {
//     var value: Int = 0
// }

// #warning("This is a warning")
// #error("This is an error")

let debugInfo = #file + ":\(#line)"

// ==============================================================================
// Main Entry Point
// ==============================================================================

@main
struct SampleApp {
    static func main() {
        print("Sample Swift Application")

        // Using various features
        let dog = Dog(name: "Buddy", age: 3, breed: "Labrador")
        dog.speak()

        var point = Point(x: 3, y: 4)
        print("Magnitude: \(point.magnitude)")

        var stack = Stack<Int>()
        stack.push(1)
        stack.push(2)
        print("Popped: \(stack.pop() ?? 0)")

        3.times {
            print("Hello!")
        }
    }
}
