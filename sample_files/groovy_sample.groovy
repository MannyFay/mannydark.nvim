#!/usr/bin/env groovy

/**
 * Comprehensive Groovy language sample demonstrating all syntax features
 * Groovy is a dynamic language for the JVM with Java interoperability
 *
 * @author Sample Author
 * @version 1.0.0
 */

package com.example.sample

import groovy.json.*
import groovy.sql.Sql
import groovy.transform.*
import groovy.xml.*
import java.time.*
import java.util.concurrent.*
import java.util.regex.*

// ============================================================================
// Constants and Static Imports
// ============================================================================

import static java.lang.Math.PI
import static java.lang.Math.sqrt

// Package-level constants
class Constants {
    static final int MAX_BUFFER_SIZE = 1024
    static final String GREETING = "Hello, Groovy!"
}

// ============================================================================
// Enums
// ============================================================================

enum Color {
    RED(0xFF0000, 'Red'),
    GREEN(0x00FF00, 'Green'),
    BLUE(0x0000FF, 'Blue'),
    WHITE(0xFFFFFF, 'White'),
    BLACK(0x000000, 'Black')

    final int hex
    final String displayName

    Color(int hex, String displayName) {
        this.hex = hex
        this.displayName = displayName
    }

    String toHexString() {
        String.format('#%06X', hex)
    }

    static Color fromHex(int hex) {
        values().find { it.hex == hex }
    }
}

enum Status {
    OK, ERROR, PENDING, TIMEOUT
}

// ============================================================================
// Traits
// ============================================================================

trait Drawable {
    abstract void draw()

    void update(double deltaTime) {
        println "Updating with delta: $deltaTime"
    }
}

trait Serializable {
    byte[] serialize() {
        // Default implementation
        return [] as byte[]
    }

    void deserialize(byte[] data) {
        // Default implementation
    }
}

trait Logging {
    void log(String message) {
        println "[${this.class.simpleName}] $message"
    }

    void logError(String message) {
        System.err.println "[ERROR] $message"
    }
}

// ============================================================================
// Classes with AST Transformations
// ============================================================================

@Immutable
class Point {
    double x
    double y
    double z = 0.0

    double distance(Point other) {
        def dx = x - other.x
        def dy = y - other.y
        def dz = z - other.z
        sqrt(dx * dx + dy * dy + dz * dz)
    }

    Point plus(Point other) {
        new Point(x: x + other.x, y: y + other.y, z: z + other.z)
    }

    Point minus(Point other) {
        new Point(x: x - other.x, y: y - other.y, z: z - other.z)
    }

    Point multiply(double scalar) {
        new Point(x: x * scalar, y: y * scalar, z: z * scalar)
    }

    static Point getORIGIN() {
        new Point(x: 0, y: 0, z: 0)
    }
}

@Canonical
class Person implements Comparable<Person> {
    String name
    int age
    String email

    @Override
    int compareTo(Person other) {
        name <=> other.name
    }
}

@ToString(includeNames = true, includeFields = true)
@EqualsAndHashCode
class Employee extends Person {
    String department
    BigDecimal salary
    Employee manager

    Employee(Map props) {
        super(props)
        this.department = props.department
        this.salary = props.salary
        this.manager = props.manager
    }
}

// ============================================================================
// Abstract Classes and Inheritance
// ============================================================================

abstract class Shape implements Drawable, Logging {
    String name
    Color color = Color.BLACK

    abstract double area()
    abstract double perimeter()

    String describe() {
        "Shape: area=${area()}, perimeter=${perimeter()}"
    }

    @Override
    void draw() {
        log "Drawing $name in ${color.displayName}"
    }
}

class Circle extends Shape {
    double radius

    Circle(double radius, Color color = Color.BLACK) {
        this.name = 'Circle'
        this.radius = radius
        this.color = color
    }

    @Override
    double area() {
        PI * radius * radius
    }

    @Override
    double perimeter() {
        2 * PI * radius
    }

    @Override
    String describe() {
        "Circle(radius=$radius)"
    }
}

class Rectangle extends Shape {
    double width
    double height

    Rectangle(double width, double height, Color color = Color.BLACK) {
        this.name = 'Rectangle'
        this.width = width
        this.height = height
        this.color = color
    }

    @Override
    double area() {
        width * height
    }

    @Override
    double perimeter() {
        2 * (width + height)
    }

    @Override
    String describe() {
        "Rectangle(width=$width, height=$height)"
    }
}

// ============================================================================
// Generic Classes
// ============================================================================

class Container<T> implements Iterable<T> {
    private List<T> items = []

    void add(T item) {
        items << item
    }

    T get(int index) {
        items[index]
    }

    int size() {
        items.size()
    }

    Container<T> leftShift(T item) {
        add(item)
        this
    }

    @Override
    Iterator<T> iterator() {
        items.iterator()
    }
}

class Pair<A, B> {
    final A first
    final B second

    Pair(A first, B second) {
        this.first = first
        this.second = second
    }

    Pair<B, A> swap() {
        new Pair<>(second, first)
    }

    @Override
    String toString() {
        "Pair($first, $second)"
    }
}

// ============================================================================
// Closures and Higher-Order Functions
// ============================================================================

class FunctionUtils {
    // Closure as return value
    static Closure<Integer> multiplier(int factor) {
        { int x -> x * factor }
    }

    // Currying
    static Closure add = { a, b -> a + b }
    static Closure add5 = add.curry(5)

    // Right currying
    static Closure divide = { a, b -> a / b }
    static Closure divideBy2 = divide.rcurry(2)

    // Composition
    static Closure compose(Closure f, Closure g) {
        { x -> f(g(x)) }
    }

    // Memoization
    static Closure<BigInteger> factorial
    static {
        factorial = { n ->
            if (n <= 1) 1G
            else n * factorial(n - 1)
        }.memoize()
    }

    // Trampoline for tail recursion
    static def trampolineFactorial(n, acc = 1G) {
        if (n <= 1) acc
        else trampolineFactorial.trampoline(n - 1, n * acc)
    }
}

// ============================================================================
// Categories and Metaprogramming
// ============================================================================

@Category(String)
class StringExtensions {
    List<String> words() {
        this.split(/\s+/) as List
    }

    boolean isPalindrome() {
        def normalized = this.toLowerCase().replaceAll(/[^a-z0-9]/, '')
        normalized == normalized.reverse()
    }

    int getWordCount() {
        words().size()
    }
}

@Category(Integer)
class IntegerExtensions {
    BigInteger factorial() {
        if (this < 0) throw new IllegalArgumentException("Negative numbers not supported")
        (1..this).inject(1G) { acc, n -> acc * n }
    }

    boolean isEven() { this % 2 == 0 }
    boolean isOdd() { !isEven() }
}

// ============================================================================
// Builders
// ============================================================================

class HtmlBuilder {
    private StringBuilder content = new StringBuilder()

    def html(@DelegatesTo(HtmlBuilder) Closure block) {
        content << '<html>'
        block.delegate = this
        block.resolveStrategy = Closure.DELEGATE_FIRST
        block()
        content << '</html>'
        content.toString()
    }

    def head(@DelegatesTo(HtmlBuilder) Closure block) {
        content << '<head>'
        block.delegate = this
        block()
        content << '</head>'
    }

    def body(@DelegatesTo(HtmlBuilder) Closure block) {
        content << '<body>'
        block.delegate = this
        block()
        content << '</body>'
    }

    def title(String text) {
        content << "<title>$text</title>"
    }

    def h1(String text) {
        content << "<h1>$text</h1>"
    }

    def p(String text) {
        content << "<p>$text</p>"
    }

    def div(@DelegatesTo(HtmlBuilder) Closure block) {
        content << '<div>'
        block.delegate = this
        block()
        content << '</div>'
    }
}

// ============================================================================
// Annotations and AST Transformations
// ============================================================================

@Singleton
class AppConfig {
    Map<String, Object> settings = [:]

    def getAt(String key) {
        settings[key]
    }

    void putAt(String key, Object value) {
        settings[key] = value
    }
}

@Sortable(includes = ['name', 'age'])
class SortablePerson {
    String name
    int age
}

@TupleConstructor
@AutoClone
class CloneablePerson {
    String name
    int age
}

@Memoized
BigInteger fib(int n) {
    if (n < 2) return n
    fib(n - 1) + fib(n - 2)
}

@Synchronized
class ThreadSafe {
    private int counter = 0

    void increment() {
        counter++
    }

    int getCounter() {
        counter
    }
}

// ============================================================================
// ExpandoMetaClass and Runtime Metaprogramming
// ============================================================================

class MetaProgramming {
    static void addDynamicMethods() {
        // Add method to String at runtime
        String.metaClass.shout = { -> delegate.toUpperCase() + '!' }

        // Add property
        String.metaClass.getReversed = { -> delegate.reverse() }

        // Override existing method
        Integer.metaClass.plus = { Integer other ->
            println "Adding $delegate and $other"
            delegate + other
        }
    }

    static void expandoExample() {
        def person = new Expando()
        person.name = 'John'
        person.age = 30
        person.greet = { "Hello, I'm $name" }

        println person.greet()
    }
}

// ============================================================================
// Main Script
// ============================================================================

// Variable declarations
def dynamicVar = 42
int typedInt = 100
String typedString = "Hello"
final CONSTANT = "Immutable"

// Type inference
var inferredInt = 42
var inferredString = "Hello"
var inferredList = [1, 2, 3]

// Numeric literals
def decimal = 42
def hex = 0xDEADBEEF
def octal = 0755
def binary = 0b10101010
def longVal = 123_456_789L
def bigInt = 123456789012345678901234567890G
def bigDecimal = 3.14159265358979323846G
def doubleVal = 3.14d
def floatVal = 3.14f
def scientific = 1.23e-4

// Strings
def singleQuote = 'Hello'
def doubleQuote = "Hello, World!"
def multiline = '''
    This is a
    multi-line string
'''
def gstring = "Value: $dynamicVar"
def expressionGstring = "Sum: ${1 + 2}"
def slashy = /This is a slashy string with no escapes needed/
def dollarSlashy = $/
    Dollar slashy string
    Can contain $ and / without escaping
/$

// Characters
char charVal = 'A' as char
def charExplicit = 'A'.charAt(0)

// Lists
def list = [1, 2, 3, 4, 5]
def emptyList = []
def typedList = [1, 2, 3] as List<Integer>
def linkedList = [1, 2, 3] as LinkedList
def range = 1..10
def rangeExclusive = 1..<10

// List operations
list << 6                    // Append
list += [7, 8]              // Add all
def first = list[0]          // Get by index
def last = list[-1]          // Negative index
def sublist = list[2..4]     // Slice
def spread = [*list, 9, 10]  // Spread operator

// Maps
def map = [one: 1, two: 2, three: 3]
def emptyMap = [:]
def stringKeyMap = ['key-with-dash': 'value']
def dynamicKeyMap = [(dynamicVar): 'value']

// Map operations
map.four = 4                 // Add entry
map['five'] = 5              // Add with subscript
def value = map.one          // Get value
def valueSubscript = map['two']
def defaultValue = map.get('missing', 0)

// Sets
def set = [1, 2, 3, 3] as Set
def hashSet = [1, 2, 3] as HashSet
def treeSet = [3, 1, 2] as TreeSet

// Arrays
def array = [1, 2, 3] as int[]
String[] stringArray = ['a', 'b', 'c']
def multiDimArray = [[1, 2], [3, 4]] as int[][]

// Control flow
if (dynamicVar > 0) {
    println 'Positive'
} else if (dynamicVar < 0) {
    println 'Negative'
} else {
    println 'Zero'
}

// Ternary operator
def result = dynamicVar > 0 ? 'Positive' : 'Non-positive'

// Elvis operator
def nullableVar = null
def defaulted = nullableVar ?: 'Default'

// Safe navigation
def person = new Person(name: 'Alice', age: 30, email: 'alice@example.com')
def emailLength = person?.email?.length()

// Switch statement
switch (dynamicVar) {
    case 0:
        println 'Zero'
        break
    case 1..10:
        println '1-10'
        break
    case Integer:
        println 'Integer'
        break
    case ~/\d+/:
        println 'Digits'
        break
    case { it > 100 }:
        println 'Greater than 100'
        break
    default:
        println 'Other'
}

// For loops
for (i in 0..9) {
    println i
}

for (item in list) {
    println item
}

for (def i = 0; i < 10; i++) {
    println i
}

list.each { println it }
list.eachWithIndex { item, index -> println "[$index] = $item" }

// While loop
def counter = 0
while (counter < 5) {
    counter++
}

// Collection operations
def doubled = list.collect { it * 2 }
def evens = list.findAll { it % 2 == 0 }
def sum = list.sum()
def product = list.inject(1) { acc, n -> acc * n }
def grouped = list.groupBy { it % 2 == 0 ? 'even' : 'odd' }

// Spread operator
def names = [new Person(name: 'Alice', age: 25, email: ''),
             new Person(name: 'Bob', age: 30, email: '')]
def allNames = names*.name

// Method chaining
def processed = list
    .findAll { it > 2 }
    .collect { it * 2 }
    .sort()
    .take(3)

// Closures
def closure = { x -> x * 2 }
def closureImplicit = { it * 2 }
def closureMultiParam = { a, b -> a + b }
def closureTyped = { int a, int b -> a + b }
def closureWithDefault = { a, b = 10 -> a + b }

// Closure delegation
def builder = new HtmlBuilder()
def html = builder.html {
    head {
        title 'Sample'
    }
    body {
        h1 'Welcome'
        p 'Hello, World!'
        div {
            p 'Nested content'
        }
    }
}
println html

// Regular expressions
def pattern = ~/\d+/
def matcher = 'hello123world456' =~ /\d+/
def matches = matcher.collect { it }
def matchResult = 'hello123' ==~ /\w+\d+/

// Replace with regex
def replaced = 'hello 123 world'.replaceAll(/\d+/, 'NUM')

// Object creation
def point = new Point(x: 1, y: 2, z: 3)
println point
println "Distance: ${point.distance(Point.ORIGIN)}"

// Named parameters
def employee = new Employee(
    name: 'Bob',
    age: 35,
    email: 'bob@example.com',
    department: 'Engineering',
    salary: 75000.00
)
println employee

// Shapes
def circle = new Circle(5.0, Color.BLUE)
circle.draw()
println "Area: ${circle.area()}"

def rectangle = new Rectangle(4.0, 6.0)
println rectangle.describe()

// Container
def container = new Container<String>()
container << 'Hello' << 'World'
container.each { println it }

// Using categories
use(StringExtensions, IntegerExtensions) {
    println 'hello world'.words()
    println 'racecar'.isPalindrome()
    println 5.factorial()
}

// JSON handling
def jsonBuilder = new JsonBuilder()
jsonBuilder {
    name 'John'
    age 30
    hobbies 'reading', 'gaming'
    address {
        city 'New York'
        zip '10001'
    }
}
println jsonBuilder.toPrettyString()

def jsonSlurper = new JsonSlurper()
def parsed = jsonSlurper.parseText('{"name": "John", "age": 30}')
println "Name: ${parsed.name}, Age: ${parsed.age}"

// XML building
def xmlBuilder = new MarkupBuilder()
def writer = new StringWriter()
def xml = new MarkupBuilder(writer)
xml.root {
    item(id: 1) {
        name 'Item 1'
        price 9.99
    }
    item(id: 2) {
        name 'Item 2'
        price 19.99
    }
}
println writer.toString()

// XML parsing
def xmlSlurper = new XmlSlurper()
def xmlDoc = xmlSlurper.parseText('<root><item>Hello</item></root>')
println xmlDoc.item.text()

// Exception handling
try {
    throw new RuntimeException('Error')
} catch (RuntimeException e) {
    println "Caught: ${e.message}"
} catch (Exception e) {
    println "General exception: ${e.message}"
} finally {
    println 'Cleanup'
}

// Multi-catch (Groovy 3+)
try {
    // risky code
} catch (IOException | SQLException e) {
    println "IO or SQL error: ${e.message}"
}

// Try with resources
new File('test.txt').withWriter { writer ->
    writer.writeLine 'Hello, File!'
}

// File operations
def file = new File('test.txt')
file.text = 'Hello, World!'
println file.text
file.eachLine { line -> println line }
file.delete()

// Date/Time
def now = LocalDateTime.now()
def date = LocalDate.of(2024, 1, 15)
def formatted = now.format(java.time.format.DateTimeFormatter.ISO_LOCAL_DATE_TIME)
println "Current time: $now"

// Concurrency
def executor = Executors.newFixedThreadPool(4)
def future = executor.submit({
    Thread.sleep(100)
    42
} as Callable<Integer>)
println "Result: ${future.get()}"
executor.shutdown()

// GPars parallel (if available)
// GParsPool.withPool {
//     def results = [1, 2, 3, 4, 5].collectParallel { it * 2 }
// }

// AST transformations in action
def config = AppConfig.instance
config['host'] = 'localhost'
config['port'] = 8080
println "Host: ${config['host']}"

// Operators
def a = 10
def b = 3
println "a + b = ${a + b}"
println "a - b = ${a - b}"
println "a * b = ${a * b}"
println "a / b = ${a / b}"
println "a % b = ${a % b}"
println "a ** b = ${a ** b}"  // Power
println "a <=> b = ${a <=> b}"  // Spaceship

// Bit operations
println "a & b = ${a & b}"
println "a | b = ${a | b}"
println "a ^ b = ${a ^ b}"
println "~a = ${~a}"
println "a << 2 = ${a << 2}"
println "a >> 2 = ${a >> 2}"

// Null-safe operations
def nullValue = null
def safeCall = nullValue?.toString()
def elvisOp = nullValue ?: 'default'

// Type coercion
def intFromString = '42' as int
def listFromArray = [1, 2, 3] as int[]
def setFromList = [1, 2, 2, 3] as Set

// Assertions
assert dynamicVar > 0 : 'Value must be positive'
assert list.size() == 8

// Method reference
def numbers = [1, 2, 3, 4, 5]
def printer = this.&println
numbers.each(printer)

// Operator overloading
def p1 = new Point(x: 1, y: 2, z: 3)
def p2 = new Point(x: 4, y: 5, z: 6)
def p3 = p1 + p2
def p4 = p1 * 2.0
println "p1 + p2 = $p3"
println "p1 * 2 = $p4"

// Script binding
binding.customVar = 'Custom value'
println binding.customVar

// Command expressions (method calls without parentheses)
println 'Hello, Groovy!'
assert true
// move 10, 'forward' would call move(10, 'forward')

println 'Program completed successfully!'
