/**
 * Comprehensive Kotlin language sample demonstrating all syntax features
 *
 * @author Sample Author
 * @version 1.0.0
 */

package com.example.sample

import kotlinx.coroutines.*
import kotlinx.coroutines.flow.*
import java.io.*
import java.time.*
import java.util.concurrent.atomic.AtomicInteger
import kotlin.properties.Delegates
import kotlin.reflect.full.*

// ============================================================================
// Constants and Top-level Declarations
// ============================================================================

const val MAX_BUFFER_SIZE = 1024
const val PI = 3.14159265358979323846
const val GREETING = "Hello, Kotlin!"

// Type aliases
typealias StringList = List<String>
typealias Callback = (Int) -> Int
typealias AsyncCallback = suspend (Int) -> Int
typealias Predicate<T> = (T) -> Boolean

// ============================================================================
// Enums
// ============================================================================

enum class Color(val hex: Int, val displayName: String) {
    RED(0xFF0000, "Red"),
    GREEN(0x00FF00, "Green"),
    BLUE(0x0000FF, "Blue"),
    WHITE(0xFFFFFF, "White"),
    BLACK(0x000000, "Black");

    fun toHexString(): String = "#${hex.toString(16).padStart(6, '0').uppercase()}"

    companion object {
        fun fromHex(hex: Int): Color? = entries.find { it.hex == hex }
    }
}

enum class Status {
    OK, ERROR, PENDING, TIMEOUT
}

// ============================================================================
// Sealed Classes and Interfaces
// ============================================================================

sealed class Result<out T, out E> {
    data class Success<T>(val value: T) : Result<T, Nothing>()
    data class Failure<E>(val error: E) : Result<Nothing, E>()

    inline fun <R> map(transform: (T) -> R): Result<R, E> = when (this) {
        is Success -> Success(transform(value))
        is Failure -> this
    }

    inline fun <R> flatMap(transform: (T) -> Result<R, @UnsafeVariance E>): Result<R, E> = when (this) {
        is Success -> transform(value)
        is Failure -> this
    }
}

sealed interface Shape {
    fun area(): Double
    fun perimeter(): Double

    fun describe(): String = "Shape: area=${area()}, perimeter=${perimeter()}"
}

// ============================================================================
// Data Classes
// ============================================================================

data class Point(
    val x: Double,
    val y: Double,
    val z: Double = 0.0
) {
    constructor(x: Double, y: Double) : this(x, y, 0.0)

    fun distance(other: Point): Double {
        val dx = x - other.x
        val dy = y - other.y
        val dz = z - other.z
        return kotlin.math.sqrt(dx * dx + dy * dy + dz * dz)
    }

    operator fun plus(other: Point) = Point(x + other.x, y + other.y, z + other.z)
    operator fun minus(other: Point) = Point(x - other.x, y - other.y, z - other.z)
    operator fun times(scalar: Double) = Point(x * scalar, y * scalar, z * scalar)

    companion object {
        val ORIGIN = Point(0.0, 0.0, 0.0)

        fun fromPolar(r: Double, theta: Double): Point {
            return Point(r * kotlin.math.cos(theta), r * kotlin.math.sin(theta))
        }
    }
}

data class Person(
    val name: String,
    val age: Int,
    val email: String? = null
) : Comparable<Person> {
    override fun compareTo(other: Person): Int = name.compareTo(other.name)
}

// ============================================================================
// Classes and Inheritance
// ============================================================================

abstract class AbstractShape(
    protected val name: String,
    var color: Color = Color.BLACK
) : Shape {
    abstract fun draw()

    open fun render() {
        println("Rendering $name in ${color.displayName}")
    }
}

class Circle(
    val radius: Double,
    color: Color = Color.BLACK
) : AbstractShape("Circle", color) {

    override fun area(): Double = PI * radius * radius

    override fun perimeter(): Double = 2 * PI * radius

    override fun draw() {
        println("Drawing circle with radius $radius")
    }

    override fun describe(): String = "Circle(radius=$radius)"
}

class Rectangle(
    val width: Double,
    val height: Double,
    color: Color = Color.BLACK
) : AbstractShape("Rectangle", color) {

    override fun area(): Double = width * height

    override fun perimeter(): Double = 2 * (width + height)

    override fun draw() {
        println("Drawing rectangle ${width}x$height")
    }

    override fun describe(): String = "Rectangle(width=$width, height=$height)"
}

// ============================================================================
// Object and Companion Object
// ============================================================================

object Singleton {
    private val counter = AtomicInteger(0)

    fun nextId(): Int = counter.incrementAndGet()

    fun reset() {
        counter.set(0)
    }
}

class Factory private constructor() {
    companion object {
        private val instances = mutableMapOf<String, Factory>()

        fun getInstance(name: String): Factory {
            return instances.getOrPut(name) { Factory() }
        }

        @JvmStatic
        fun create(): Factory = Factory()
    }
}

// ============================================================================
// Interfaces and Delegation
// ============================================================================

interface Drawable {
    fun draw()
    fun update(deltaTime: Double) {}
}

interface Serializable {
    fun serialize(): ByteArray
    fun deserialize(data: ByteArray)
}

class Sprite(
    private val position: Point,
    private val texturePath: String
) : Drawable {
    override fun draw() {
        println("Drawing sprite at $position")
    }

    override fun update(deltaTime: Double) {
        println("Updating sprite, delta: $deltaTime")
    }
}

// Delegation
class LoggingDrawable(private val delegate: Drawable) : Drawable by delegate {
    override fun draw() {
        println("Before draw")
        delegate.draw()
        println("After draw")
    }
}

// ============================================================================
// Generics
// ============================================================================

class Container<T>(private val items: MutableList<T> = mutableListOf()) : Iterable<T> {
    fun add(item: T) {
        items.add(item)
    }

    fun get(index: Int): T = items[index]

    fun size(): Int = items.size

    operator fun plusAssign(item: T) {
        add(item)
    }

    override fun iterator(): Iterator<T> = items.iterator()
}

class Pair<out A, out B>(val first: A, val second: B) {
    operator fun component1(): A = first
    operator fun component2(): B = second

    fun <C> mapFirst(transform: (A) -> C): Pair<C, B> = Pair(transform(first), second)
    fun <C> mapSecond(transform: (B) -> C): Pair<A, C> = Pair(first, transform(second))
}

// Generic function with constraints
fun <T : Comparable<T>> max(a: T, b: T): T = if (a > b) a else b

fun <T> List<T>.secondOrNull(): T? = if (size >= 2) this[1] else null

inline fun <reified T> typeOf(): String = T::class.simpleName ?: "Unknown"

// ============================================================================
// Extension Functions and Properties
// ============================================================================

fun String.words(): List<String> = split(Regex("\\s+"))

fun String.isPalindrome(): Boolean {
    val normalized = lowercase().filter { it.isLetterOrDigit() }
    return normalized == normalized.reversed()
}

fun Int.factorial(): Long {
    require(this >= 0) { "Factorial not defined for negative numbers" }
    return if (this <= 1) 1L else this.toLong() * (this - 1).factorial()
}

val String.wordCount: Int
    get() = words().size

var StringBuilder.lastChar: Char
    get() = this[length - 1]
    set(value) {
        setCharAt(length - 1, value)
    }

// Extension function with receiver
fun <T> T.applyIf(condition: Boolean, block: T.() -> Unit): T {
    if (condition) block()
    return this
}

// ============================================================================
// Higher-Order Functions and Lambdas
// ============================================================================

inline fun <T> measure(block: () -> T): kotlin.Pair<T, Long> {
    val start = System.nanoTime()
    val result = block()
    val elapsed = System.nanoTime() - start
    return result to elapsed
}

inline fun <T> retry(times: Int, block: () -> T): T {
    var lastException: Exception? = null
    repeat(times) {
        try {
            return block()
        } catch (e: Exception) {
            lastException = e
        }
    }
    throw lastException!!
}

fun <T, R> Collection<T>.foldRight(initial: R, operation: (T, R) -> R): R {
    var accumulator = initial
    for (element in this.reversed()) {
        accumulator = operation(element, accumulator)
    }
    return accumulator
}

// Function returning function
fun multiplier(factor: Int): (Int) -> Int = { it * factor }

// ============================================================================
// Delegation Properties
// ============================================================================

class Example {
    // Lazy property
    val lazyValue: String by lazy {
        println("Computing lazy value...")
        "Hello"
    }

    // Observable property
    var observedValue: Int by Delegates.observable(0) { prop, old, new ->
        println("${prop.name}: $old -> $new")
    }

    // Vetoable property
    var vetoedValue: Int by Delegates.vetoable(0) { _, _, new ->
        new >= 0
    }

    // Custom delegate
    var customValue: String by LoggingDelegate("initial")

    // Map-backed properties
    private val map = mutableMapOf<String, Any?>()
    var mapProperty: String by map
}

class LoggingDelegate<T>(private var value: T) {
    operator fun getValue(thisRef: Any?, property: kotlin.reflect.KProperty<*>): T {
        println("Getting ${property.name}: $value")
        return value
    }

    operator fun setValue(thisRef: Any?, property: kotlin.reflect.KProperty<*>, newValue: T) {
        println("Setting ${property.name}: $value -> $newValue")
        value = newValue
    }
}

// ============================================================================
// Coroutines
// ============================================================================

suspend fun fetchData(url: String): String {
    delay(100) // Simulate network delay
    return "Data from $url"
}

suspend fun parallelFetch(urls: List<String>): List<String> = coroutineScope {
    urls.map { url ->
        async { fetchData(url) }
    }.awaitAll()
}

fun flowExample(): Flow<Int> = flow {
    for (i in 1..10) {
        delay(100)
        emit(i)
    }
}

suspend fun flowCollect() {
    flowExample()
        .filter { it % 2 == 0 }
        .map { it * 2 }
        .collect { println(it) }
}

// ============================================================================
// DSL
// ============================================================================

class HtmlBuilder {
    private val children = mutableListOf<String>()

    fun head(block: HeadBuilder.() -> Unit) {
        val builder = HeadBuilder()
        builder.block()
        children.add("<head>${builder.build()}</head>")
    }

    fun body(block: BodyBuilder.() -> Unit) {
        val builder = BodyBuilder()
        builder.block()
        children.add("<body>${builder.build()}</body>")
    }

    fun build(): String = "<html>${children.joinToString("")}</html>"
}

class HeadBuilder {
    private var title = ""

    fun title(text: String) {
        title = text
    }

    fun build(): String = "<title>$title</title>"
}

class BodyBuilder {
    private val elements = mutableListOf<String>()

    fun h1(text: String) {
        elements.add("<h1>$text</h1>")
    }

    fun p(text: String) {
        elements.add("<p>$text</p>")
    }

    fun div(block: BodyBuilder.() -> Unit) {
        val builder = BodyBuilder()
        builder.block()
        elements.add("<div>${builder.build()}</div>")
    }

    fun build(): String = elements.joinToString("")
}

fun html(block: HtmlBuilder.() -> Unit): String {
    val builder = HtmlBuilder()
    builder.block()
    return builder.build()
}

// ============================================================================
// Annotations
// ============================================================================

@Target(AnnotationTarget.CLASS, AnnotationTarget.FUNCTION)
@Retention(AnnotationRetention.RUNTIME)
annotation class CustomAnnotation(
    val value: String = "",
    val priority: Int = 0
)

@Target(AnnotationTarget.PROPERTY)
@Retention(AnnotationRetention.RUNTIME)
annotation class JsonField(val name: String)

// ============================================================================
// Inline Classes (Value Classes)
// ============================================================================

@JvmInline
value class UserId(val value: Long) {
    init {
        require(value > 0) { "User ID must be positive" }
    }

    override fun toString(): String = "UserId($value)"
}

@JvmInline
value class Email(val value: String) {
    init {
        require(value.contains("@")) { "Invalid email format" }
    }
}

// ============================================================================
// Context Receivers (Kotlin 1.6.20+)
// ============================================================================

interface Logger {
    fun log(message: String)
}

interface Database {
    fun query(sql: String): List<Map<String, Any>>
}

context(Logger, Database)
fun processData(id: Int) {
    log("Processing data for id: $id")
    val results = query("SELECT * FROM data WHERE id = $id")
    log("Found ${results.size} results")
}

// ============================================================================
// Main Function
// ============================================================================

@CustomAnnotation("main", priority = 1)
fun main(args: Array<String>) {
    // Variable declarations
    val immutable: Int = 42
    var mutable: Int = 0
    mutable++

    // Type inference
    val inferredInt = 42
    val inferredString = "Hello"
    val inferredList = listOf(1, 2, 3)

    // Nullable types
    var nullable: String? = "Hello"
    nullable = null

    // Safe calls and Elvis operator
    val length: Int = nullable?.length ?: 0
    val upper: String? = nullable?.uppercase()

    // Not-null assertion
    // val forced: Int = nullable!!.length // Would throw NPE

    // Numeric literals
    val decimal = 42
    val hex = 0xDEADBEEF
    val binary = 0b10101010
    val long = 123_456_789L
    val double = 3.14
    val float = 3.14f
    val scientific = 1.23e-4

    // Strings
    val string = "Hello, World!"
    val multiline = """
        This is a
        multi-line string
        in Kotlin
    """.trimIndent()

    val template = "Value: $mutable"
    val expression = "Sum: ${1 + 2}"
    val rawString = """Raw \n string"""

    // Characters
    val char = 'A'
    val escaped = '\n'
    val unicode = '\u0041'

    // Arrays
    val intArray = intArrayOf(1, 2, 3, 4, 5)
    val stringArray = arrayOf("a", "b", "c")
    val nullableArray = arrayOfNulls<String>(10)
    val initArray = Array(5) { it * 2 }

    // Collections
    val list = listOf(1, 2, 3)
    val mutableList = mutableListOf(1, 2, 3)
    mutableList.add(4)

    val set = setOf("a", "b", "c")
    val mutableSet = mutableSetOf("a", "b")
    mutableSet.add("c")

    val map = mapOf("one" to 1, "two" to 2)
    val mutableMap = mutableMapOf("one" to 1)
    mutableMap["two"] = 2

    // Sequences (lazy)
    val sequence = sequenceOf(1, 2, 3, 4, 5)
    val generated = generateSequence(1) { it + 1 }.take(10)

    // Ranges
    val intRange = 1..10
    val charRange = 'a'..'z'
    val downTo = 10 downTo 1
    val step = 1..10 step 2
    val until = 1 until 10

    // Control flow
    if (mutable > 0) {
        println("Positive")
    } else if (mutable < 0) {
        println("Negative")
    } else {
        println("Zero")
    }

    // If as expression
    val max = if (mutable > immutable) mutable else immutable

    // When expression
    val description = when (mutable) {
        0 -> "Zero"
        in 1..10 -> "1-10"
        in 11..100 -> "11-100"
        else -> "Other"
    }

    // When with subject
    when (val result = mutable * 2) {
        0 -> println("Zero")
        else -> println("Result: $result")
    }

    // When without subject
    when {
        mutable < 0 -> println("Negative")
        mutable > 0 -> println("Positive")
        else -> println("Zero")
    }

    // For loops
    for (i in 0..9) {
        println(i)
    }

    for (item in list) {
        println(item)
    }

    for ((index, value) in list.withIndex()) {
        println("[$index] = $value")
    }

    for ((key, value) in map) {
        println("$key -> $value")
    }

    // While and do-while
    var counter = 0
    while (counter < 5) {
        counter++
    }

    do {
        counter--
    } while (counter > 0)

    // Labels and break/continue
    outer@ for (i in 0..9) {
        for (j in 0..9) {
            if (i * j > 50) break@outer
        }
    }

    // Return from lambda
    listOf(1, 2, 3, 4, 5).forEach {
        if (it == 3) return@forEach
        println(it)
    }

    // Try-catch
    try {
        throw RuntimeException("Error")
    } catch (e: RuntimeException) {
        println("Caught: ${e.message}")
    } catch (e: Exception) {
        println("General exception")
    } finally {
        println("Cleanup")
    }

    // Try as expression
    val parsed: Int? = try {
        "42".toInt()
    } catch (e: NumberFormatException) {
        null
    }

    // Use (try-with-resources)
    File("test.txt").bufferedReader().use { reader ->
        reader.readLine()
    }

    // Object creation
    val point = Point(1.0, 2.0, 3.0)
    println(point)
    println("Distance: ${point.distance(Point.ORIGIN)}")

    val person = Person("Alice", 30, "alice@example.com")
    println(person)

    // Copy data class
    val olderPerson = person.copy(age = 31)

    // Destructuring
    val (name, age, email) = person
    val (x, y, z) = point

    // Class instances
    val circle = Circle(5.0, Color.BLUE)
    circle.draw()
    println("Area: ${circle.area()}")

    val rectangle = Rectangle(4.0, 6.0)
    println(rectangle.describe())

    // Shape as sealed class
    fun handleShape(shape: Shape): String = when (shape) {
        is Circle -> "Circle with radius ${shape.radius}"
        is Rectangle -> "Rectangle ${shape.width}x${shape.height}"
    }

    // Result sealed class
    val success: Result<Int, String> = Result.Success(42)
    val failure: Result<Int, String> = Result.Failure("Error")

    when (success) {
        is Result.Success -> println("Value: ${success.value}")
        is Result.Failure -> println("Error: ${success.error}")
    }

    // Container
    val container = Container<String>()
    container += "Hello"
    container += "World"
    for (item in container) {
        println(item)
    }

    // Extension functions
    println("hello world".words())
    println("racecar".isPalindrome())
    println(5.factorial())

    // Higher-order functions
    val (result, time) = measure {
        Thread.sleep(100)
        42
    }
    println("Result: $result, Time: ${time / 1_000_000}ms")

    // Lambda expressions
    val square: (Int) -> Int = { it * it }
    val sum: (Int, Int) -> Int = { a, b -> a + b }

    // Collection operations
    val numbers = listOf(1, 2, 3, 4, 5, 6, 7, 8, 9, 10)

    val evens = numbers.filter { it % 2 == 0 }
    val doubled = numbers.map { it * 2 }
    val total = numbers.reduce { acc, n -> acc + n }
    val grouped = numbers.groupBy { if (it % 2 == 0) "even" else "odd" }
    val partitioned = numbers.partition { it % 2 == 0 }

    // Chaining
    val processed = numbers
        .filter { it > 3 }
        .map { it * 2 }
        .take(3)
        .sorted()

    // Scope functions
    val stringBuilder = StringBuilder().apply {
        append("Hello")
        append(", ")
        append("World!")
    }

    val upperString = "hello".let { it.uppercase() }

    val config = mutableMapOf<String, Any>().also {
        it["host"] = "localhost"
        it["port"] = 8080
    }

    person.run {
        println("$name is $age years old")
    }

    with(person) {
        println("$name - $email")
    }

    // Singleton
    println("ID: ${Singleton.nextId()}")
    println("ID: ${Singleton.nextId()}")

    // Enum
    val color = Color.RED
    println("${color.displayName}: ${color.toHexString()}")

    Color.entries.forEach {
        println("${it.name}: ${it.hex}")
    }

    // Value classes
    val userId = UserId(42)
    println(userId)

    // DSL
    val htmlContent = html {
        head {
            title("Hello")
        }
        body {
            h1("Welcome")
            p("This is a paragraph")
            div {
                p("Nested content")
            }
        }
    }
    println(htmlContent)

    // Coroutines
    runBlocking {
        val data = fetchData("https://example.com")
        println(data)

        val urls = listOf("url1", "url2", "url3")
        val results = parallelFetch(urls)
        results.forEach { println(it) }

        // Flow
        flowExample()
            .take(5)
            .collect { println("Flow: $it") }

        // Launch and async
        val job = launch {
            delay(100)
            println("Job completed")
        }

        val deferred = async {
            delay(100)
            42
        }
        println("Async result: ${deferred.await()}")

        job.join()
    }

    // Reflection
    val klass = Person::class
    println("Class: ${klass.simpleName}")

    for (property in klass.memberProperties) {
        println("Property: ${property.name}")
    }

    // Type checks and casts
    val any: Any = "Hello"

    if (any is String) {
        println("Length: ${any.length}") // Smart cast
    }

    val str = any as? String ?: "Default"

    // Require and check
    require(mutable >= 0) { "Value must be non-negative" }
    check(mutable < 100) { "Value out of range" }

    // Error handling
    val nonNull = nullable ?: error("Value cannot be null")

    println("Program completed successfully!")
}
