/**
 * Comprehensive Scala language sample demonstrating all syntax features
 * Scala combines object-oriented and functional programming
 */

package com.example.sample

import scala.annotation._
import scala.collection.mutable
import scala.concurrent._
import scala.concurrent.duration._
import scala.concurrent.ExecutionContext.Implicits.global
import scala.io.Source
import scala.language.{implicitConversions, postfixOps}
import scala.math._
import scala.reflect.ClassTag
import scala.util._
import scala.util.matching.Regex

// ============================================================================
// Object and Package Object
// ============================================================================

/** Package-level constants would go in a package object */
object Constants {
  val MaxBufferSize: Int = 1024
  val Pi: Double = 3.14159265358979323846
  val Greeting: String = "Hello, Scala!"
}

// ============================================================================
// Enumerations
// ============================================================================

/** Scala 3 style enum (also works in Scala 2 with sealed traits) */
sealed trait Color {
  def hex: Int
  def displayName: String
  def toHexString: String = f"#$hex%06X"
}

object Color {
  case object Red extends Color {
    val hex = 0xFF0000
    val displayName = "Red"
  }
  case object Green extends Color {
    val hex = 0x00FF00
    val displayName = "Green"
  }
  case object Blue extends Color {
    val hex = 0x0000FF
    val displayName = "Blue"
  }
  case object White extends Color {
    val hex = 0xFFFFFF
    val displayName = "White"
  }
  case object Black extends Color {
    val hex = 0x000000
    val displayName = "Black"
  }

  val values: List[Color] = List(Red, Green, Blue, White, Black)

  def fromHex(hex: Int): Option[Color] = values.find(_.hex == hex)
}

/** Scala 2 Enumeration style */
object Status extends Enumeration {
  type Status = Value
  val Ok, Error, Pending, Timeout = Value
}

// ============================================================================
// Type Aliases and Type Members
// ============================================================================

object Types {
  type StringList = List[String]
  type Callback = Int => Int
  type AsyncCallback = Int => Future[Int]
  type Predicate[T] = T => Boolean
  type Result[+T] = Either[Throwable, T]
}

// ============================================================================
// Case Classes
// ============================================================================

case class Point(x: Double, y: Double, z: Double = 0.0) {
  def distance(other: Point): Double = {
    val dx = x - other.x
    val dy = y - other.y
    val dz = z - other.z
    sqrt(dx * dx + dy * dy + dz * dz)
  }

  def +(other: Point): Point = Point(x + other.x, y + other.y, z + other.z)
  def -(other: Point): Point = Point(x - other.x, y - other.y, z - other.z)
  def *(scalar: Double): Point = Point(x * scalar, y * scalar, z * scalar)
  def unary_- : Point = Point(-x, -y, -z)
}

object Point {
  val Origin: Point = Point(0, 0, 0)

  def fromPolar(r: Double, theta: Double): Point =
    Point(r * cos(theta), r * sin(theta))

  /** Extractor for pattern matching */
  def unapply(p: Point): Option[(Double, Double, Double)] =
    Some((p.x, p.y, p.z))
}

case class Person(
    name: String,
    age: Int,
    email: Option[String] = None
) extends Ordered[Person] {
  def compare(that: Person): Int = this.name.compare(that.name)

  def withEmail(email: String): Person = copy(email = Some(email))
}

// ============================================================================
// Traits and Abstract Classes
// ============================================================================

trait Shape {
  def area: Double
  def perimeter: Double
  def describe: String = s"Shape: area=$area, perimeter=$perimeter"
}

trait Drawable {
  def draw(): Unit
  def update(deltaTime: Double): Unit = {}
}

trait Serializable[T] {
  def serialize: Array[Byte]
  def deserialize(data: Array[Byte]): T
}

/** Trait with self-type annotation */
trait Logging { self: Shape =>
  def log(message: String): Unit = {
    println(s"[${this.getClass.getSimpleName}] $message (area: $area)")
  }
}

abstract class AbstractShape(val name: String) extends Shape with Drawable {
  var color: Color = Color.Black

  def render(): Unit = {
    println(s"Rendering $name in ${color.displayName}")
  }
}

// ============================================================================
// Classes with Inheritance
// ============================================================================

class Circle(val radius: Double, color: Color = Color.Black)
    extends AbstractShape("Circle")
    with Logging {

  this.color = color

  override def area: Double = Constants.Pi * radius * radius
  override def perimeter: Double = 2 * Constants.Pi * radius

  override def draw(): Unit = {
    log(s"Drawing circle with radius $radius")
  }

  override def describe: String = s"Circle(radius=$radius)"
}

class Rectangle(val width: Double, val height: Double, color: Color = Color.Black)
    extends AbstractShape("Rectangle")
    with Logging {

  this.color = color

  override def area: Double = width * height
  override def perimeter: Double = 2 * (width + height)

  override def draw(): Unit = {
    log(s"Drawing rectangle ${width}x$height")
  }

  override def describe: String = s"Rectangle(width=$width, height=$height)"
}

// ============================================================================
// Sealed Traits and ADTs
// ============================================================================

sealed trait Result[+A, +E]
case class Success[A](value: A) extends Result[A, Nothing]
case class Failure[E](error: E) extends Result[Nothing, E]

object Result {
  def success[A](value: A): Result[A, Nothing] = Success(value)
  def failure[E](error: E): Result[Nothing, E] = Failure(error)

  implicit class ResultOps[A, E](val result: Result[A, E]) extends AnyVal {
    def map[B](f: A => B): Result[B, E] = result match {
      case Success(a) => Success(f(a))
      case Failure(e) => Failure(e)
    }

    def flatMap[B, E2 >: E](f: A => Result[B, E2]): Result[B, E2] = result match {
      case Success(a) => f(a)
      case Failure(e) => Failure(e)
    }

    def fold[B](onFailure: E => B, onSuccess: A => B): B = result match {
      case Success(a) => onSuccess(a)
      case Failure(e) => onFailure(e)
    }
  }
}

sealed trait Tree[+A]
case object Empty extends Tree[Nothing]
case class Leaf[A](value: A) extends Tree[A]
case class Node[A](left: Tree[A], value: A, right: Tree[A]) extends Tree[A]

object Tree {
  def apply[A](value: A): Tree[A] = Leaf(value)
  def empty[A]: Tree[A] = Empty
}

// ============================================================================
// Generic Classes and Type Parameters
// ============================================================================

class Container[A](private var items: List[A] = Nil) extends Iterable[A] {
  def add(item: A): Unit = {
    items = items :+ item
  }

  def get(index: Int): Option[A] = items.lift(index)

  def +=(item: A): Container[A] = {
    add(item)
    this
  }

  override def iterator: Iterator[A] = items.iterator
  override def size: Int = items.size
}

class Pair[+A, +B](val first: A, val second: B) {
  def swap: Pair[B, A] = new Pair(second, first)

  def map[C, D](f: A => C, g: B => D): Pair[C, D] =
    new Pair(f(first), g(second))

  override def toString: String = s"Pair($first, $second)"
}

object Pair {
  def apply[A, B](a: A, b: B): Pair[A, B] = new Pair(a, b)

  def unapply[A, B](pair: Pair[A, B]): Option[(A, B)] =
    Some((pair.first, pair.second))
}

/** Type class pattern */
trait Ordering[T] {
  def compare(x: T, y: T): Int
  def lt(x: T, y: T): Boolean = compare(x, y) < 0
  def gt(x: T, y: T): Boolean = compare(x, y) > 0
  def eq(x: T, y: T): Boolean = compare(x, y) == 0
}

object Ordering {
  implicit val intOrdering: Ordering[Int] = (x: Int, y: Int) => x - y
  implicit val stringOrdering: Ordering[String] = (x: String, y: String) => x.compareTo(y)

  def apply[T](implicit ord: Ordering[T]): Ordering[T] = ord
}

// ============================================================================
// Implicits and Type Classes
// ============================================================================

/** Implicit class for extension methods */
implicit class StringOps(val s: String) extends AnyVal {
  def words: List[String] = s.split("\\s+").toList
  def isPalindrome: Boolean = {
    val normalized = s.toLowerCase.filter(_.isLetterOrDigit)
    normalized == normalized.reverse
  }
  def wordCount: Int = words.length
}

implicit class IntOps(val n: Int) extends AnyVal {
  def factorial: Long = {
    require(n >= 0, "Factorial not defined for negative numbers")
    (1L to n.toLong).product
  }
  def isEven: Boolean = n % 2 == 0
  def isOdd: Boolean = !isEven
}

/** Show type class */
trait Show[T] {
  def show(value: T): String
}

object Show {
  def apply[T](implicit s: Show[T]): Show[T] = s

  implicit val intShow: Show[Int] = (value: Int) => value.toString
  implicit val stringShow: Show[String] = (value: String) => s""""$value""""
  implicit val booleanShow: Show[Boolean] = (value: Boolean) => if (value) "yes" else "no"

  implicit def optionShow[T: Show]: Show[Option[T]] = {
    case Some(value) => s"Some(${Show[T].show(value)})"
    case None => "None"
  }

  implicit def listShow[T: Show]: Show[List[T]] = (list: List[T]) =>
    list.map(Show[T].show).mkString("[", ", ", "]")
}

/** Syntax extension via implicit class */
implicit class ShowOps[T: Show](val value: T) {
  def show: String = Show[T].show(value)
}

// ============================================================================
// Higher-Order Functions
// ============================================================================

object Functions {
  /** Function composition */
  def compose[A, B, C](f: B => C, g: A => B): A => C = a => f(g(a))

  def andThen[A, B, C](f: A => B, g: B => C): A => C = a => g(f(a))

  /** Currying */
  def add(a: Int)(b: Int): Int = a + b
  val add5: Int => Int = add(5)

  /** Partial application */
  def multiply(a: Int, b: Int): Int = a * b
  val double: Int => Int = multiply(2, _)

  /** By-name parameters */
  def byNameExample(x: => Int): Int = {
    println("Evaluating x...")
    x + x
  }

  /** Varargs */
  def sum(numbers: Int*): Int = numbers.sum

  /** Named and default parameters */
  def greet(name: String, greeting: String = "Hello"): String =
    s"$greeting, $name!"

  /** Multiple parameter lists */
  def foldLeft[A, B](list: List[A])(initial: B)(op: (B, A) => B): B =
    list.foldLeft(initial)(op)

  /** Context bounds */
  def max[T: Ordering](a: T, b: T): T = {
    val ord = implicitly[Ordering[T]]
    if (ord.gt(a, b)) a else b
  }

  /** View bounds (deprecated, use context bounds instead) */
  def sortedList[T](list: List[T])(implicit ord: scala.Ordering[T]): List[T] =
    list.sorted
}

// ============================================================================
// Pattern Matching
// ============================================================================

object PatternMatching {
  def matchExample(x: Any): String = x match {
    case 0 => "zero"
    case i: Int if i > 0 => s"positive int: $i"
    case i: Int => s"negative int: $i"
    case s: String => s"string: $s"
    case (a, b) => s"tuple: ($a, $b)"
    case Point(x, y, z) => s"point: ($x, $y, $z)"
    case list: List[_] => s"list with ${list.size} elements"
    case Some(value) => s"some: $value"
    case None => "none"
    case _ => "unknown"
  }

  /** Extractors with custom unapply */
  object Even {
    def unapply(n: Int): Option[Int] =
      if (n % 2 == 0) Some(n) else None
  }

  object Odd {
    def unapply(n: Int): Option[Int] =
      if (n % 2 != 0) Some(n) else None
  }

  def evenOdd(n: Int): String = n match {
    case Even(e) => s"$e is even"
    case Odd(o) => s"$o is odd"
  }

  /** Sequence patterns */
  def listPattern(list: List[Int]): String = list match {
    case Nil => "empty"
    case head :: Nil => s"single: $head"
    case head :: second :: _ => s"first: $head, second: $second"
    case _ => "other"
  }

  /** Type patterns with erasure workaround */
  def typePattern[T: ClassTag](list: List[T]): String = {
    val clazz = implicitly[ClassTag[T]].runtimeClass
    s"List of ${clazz.getSimpleName}"
  }
}

// ============================================================================
// For Comprehensions
// ============================================================================

object ForComprehensions {
  val combinations: List[(Int, Char)] = for {
    n <- List(1, 2, 3)
    c <- List('a', 'b', 'c')
  } yield (n, c)

  val filtered: List[Int] = for {
    n <- List(1, 2, 3, 4, 5)
    if n % 2 == 0
    doubled = n * 2
  } yield doubled

  val optionFor: Option[Int] = for {
    a <- Some(1)
    b <- Some(2)
    c <- Some(3)
  } yield a + b + c

  val eitherFor: Either[String, Int] = for {
    a <- Right(1): Either[String, Int]
    b <- Right(2): Either[String, Int]
  } yield a + b

  val futureFor: Future[Int] = for {
    a <- Future.successful(1)
    b <- Future.successful(2)
  } yield a + b
}

// ============================================================================
// Collections
// ============================================================================

object CollectionExamples {
  // Immutable collections
  val list: List[Int] = List(1, 2, 3, 4, 5)
  val vector: Vector[Int] = Vector(1, 2, 3)
  val set: Set[String] = Set("a", "b", "c")
  val map: Map[String, Int] = Map("one" -> 1, "two" -> 2)

  // Mutable collections
  val mutableList: mutable.ListBuffer[Int] = mutable.ListBuffer(1, 2, 3)
  val mutableSet: mutable.Set[String] = mutable.Set("a", "b")
  val mutableMap: mutable.Map[String, Int] = mutable.Map("one" -> 1)

  // Operations
  val mapped: List[Int] = list.map(_ * 2)
  val filtered: List[Int] = list.filter(_ > 2)
  val flatMapped: List[Int] = list.flatMap(n => List(n, n * 2))
  val reduced: Int = list.reduce(_ + _)
  val folded: Int = list.foldLeft(0)(_ + _)
  val grouped: Map[Boolean, List[Int]] = list.groupBy(_ % 2 == 0)
  val partitioned: (List[Int], List[Int]) = list.partition(_ % 2 == 0)

  // Chaining
  val processed: List[Int] = list
    .filter(_ > 2)
    .map(_ * 2)
    .take(2)
    .sorted

  // Lazy evaluation with views
  val viewed: Seq[Int] = list.view
    .filter(_ > 2)
    .map(_ * 2)
    .toSeq

  // Streams (lazy infinite sequences)
  def fibs: LazyList[BigInt] = {
    def loop(a: BigInt, b: BigInt): LazyList[BigInt] =
      a #:: loop(b, a + b)
    loop(0, 1)
  }
}

// ============================================================================
// Futures and Concurrency
// ============================================================================

object Concurrency {
  def fetchData(url: String): Future[String] = Future {
    Thread.sleep(100)
    s"Data from $url"
  }

  def parallelFetch(urls: List[String]): Future[List[String]] =
    Future.traverse(urls)(fetchData)

  def combineFutures(): Future[Int] = {
    val f1 = Future.successful(1)
    val f2 = Future.successful(2)
    val f3 = Future.successful(3)

    for {
      a <- f1
      b <- f2
      c <- f3
    } yield a + b + c
  }

  def recoverExample(): Future[Int] = {
    Future.failed[Int](new RuntimeException("Error"))
      .recover {
        case _: RuntimeException => 0
      }
      .recoverWith {
        case _: Exception => Future.successful(-1)
      }
  }

  def transformExample(): Future[String] = {
    Future.successful(42)
      .map(_.toString)
      .filter(_.nonEmpty)
      .transform(
        success => s"Success: $success",
        failure => new RuntimeException(s"Failed: ${failure.getMessage}")
      )
  }
}

// ============================================================================
// Exception Handling
// ============================================================================

object Exceptions {
  def tryCatch(): Unit = {
    try {
      throw new RuntimeException("Error")
    } catch {
      case e: IllegalArgumentException =>
        println(s"Illegal argument: ${e.getMessage}")
      case e: RuntimeException =>
        println(s"Runtime error: ${e.getMessage}")
      case e: Exception =>
        println(s"General exception: ${e.getMessage}")
    } finally {
      println("Cleanup")
    }
  }

  def tryExpression(): Int = {
    Try("42".toInt).getOrElse(0)
  }

  def tryPatternMatch(): String = {
    Try("abc".toInt) match {
      case scala.util.Success(value) => s"Parsed: $value"
      case scala.util.Failure(e) => s"Error: ${e.getMessage}"
    }
  }

  def eitherExample(s: String): Either[String, Int] = {
    Try(s.toInt).toEither.left.map(_.getMessage)
  }
}

// ============================================================================
// Lazy Evaluation
// ============================================================================

object LazyEval {
  lazy val expensiveComputation: Int = {
    println("Computing...")
    (1 to 1000000).sum
  }

  def byName(x: => Int): Int = {
    println("Before x")
    val result = x
    println("After x")
    result
  }

  def memoize[A, B](f: A => B): A => B = {
    val cache = mutable.Map.empty[A, B]
    a => cache.getOrElseUpdate(a, f(a))
  }
}

// ============================================================================
// Annotations
// ============================================================================

@deprecated("Use newMethod instead", "1.0")
def oldMethod(): Unit = {}

@tailrec
def factorial(n: Int, acc: Long = 1): Long = {
  if (n <= 1) acc
  else factorial(n - 1, n * acc)
}

@inline
def fastMethod(x: Int): Int = x * 2

// ============================================================================
// Main Object
// ============================================================================

object Sample extends App {
  import Constants._
  import Types._

  // Variable declarations
  val immutable: Int = 42
  var mutable: Int = 0
  mutable += 1

  // Type inference
  val inferredInt = 42
  val inferredString = "Hello"
  val inferredList = List(1, 2, 3)

  // Lazy val
  lazy val lazyVal = {
    println("Lazy evaluation")
    "Hello"
  }

  // Numeric literals
  val decimal = 42
  val hex = 0xDEADBEEF
  val long = 123456789L
  val double = 3.14
  val float = 3.14f
  val bigInt = BigInt("123456789012345678901234567890")
  val bigDecimal = BigDecimal("3.14159265358979323846")

  // Strings
  val string = "Hello, World!"
  val multiline =
    """This is a
      |multi-line string
      |in Scala""".stripMargin

  val interpolated = s"Value: $mutable"
  val formatted = f"Pi: $Pi%.4f"
  val raw = raw"No \n escape"

  // Characters
  val char = 'A'
  val escaped = '\n'
  val unicode = '\u0041'

  // Tuples
  val tuple2 = (1, "hello")
  val tuple3 = (1, "hello", 3.14)
  val (a, b) = tuple2
  val (x, y, z) = tuple3

  // Options
  val some: Option[Int] = Some(42)
  val none: Option[Int] = None
  val optionValue = some.getOrElse(0)
  val mapped = some.map(_ * 2)
  val flatMapped = some.flatMap(n => Some(n.toString))

  // Either
  val right: Either[String, Int] = Right(42)
  val left: Either[String, Int] = Left("Error")
  val eitherFold = right.fold(
    error => s"Error: $error",
    value => s"Value: $value"
  )

  // Collections
  val list = List(1, 2, 3, 4, 5)
  val vector = Vector("a", "b", "c")
  val set = Set(1, 2, 3)
  val map = Map("one" -> 1, "two" -> 2)

  // Ranges
  val range = 1 to 10
  val rangeExclusive = 1 until 10
  val rangeStep = 1 to 10 by 2

  // Control flow
  if (mutable > 0) {
    println("Positive")
  } else if (mutable < 0) {
    println("Negative")
  } else {
    println("Zero")
  }

  // If expression
  val max = if (mutable > immutable) mutable else immutable

  // Match expression
  val description = mutable match {
    case 0 => "Zero"
    case n if n > 0 => s"Positive: $n"
    case n => s"Negative: $n"
  }

  // For loops
  for (i <- 0 until 10) {
    println(i)
  }

  for (item <- list) {
    println(item)
  }

  for ((k, v) <- map) {
    println(s"$k -> $v")
  }

  // For comprehension
  val combinations = for {
    i <- 1 to 3
    j <- 1 to 3
    if i != j
  } yield (i, j)

  // While loop
  var counter = 0
  while (counter < 5) {
    counter += 1
  }

  // Pattern matching
  PatternMatching.matchExample(42)
  PatternMatching.matchExample("hello")
  PatternMatching.matchExample(Point(1, 2, 3))

  // Case class
  val point = Point(1.0, 2.0, 3.0)
  println(point)
  println(s"Distance: ${point.distance(Point.Origin)}")

  val point2 = point.copy(x = 10.0)
  val Point(px, py, pz) = point

  // Person
  val person = Person("Alice", 30, Some("alice@example.com"))
  println(person)

  val olderPerson = person.copy(age = 31)

  // Shapes
  val circle = new Circle(5.0, Color.Blue)
  circle.draw()
  println(s"Area: ${circle.area}")

  val rectangle = new Rectangle(4.0, 6.0)
  println(rectangle.describe)

  // Generic container
  val container = new Container[String]()
  container += "Hello"
  container += "World"
  container.foreach(println)

  // Extension methods
  println("hello world".words)
  println("racecar".isPalindrome)
  println(5.factorial)

  // Type classes
  println(42.show)
  println("hello".show)
  println(List(1, 2, 3).show)

  // Higher-order functions
  val squared = list.map(x => x * x)
  val evens = list.filter(_ % 2 == 0)
  val sum = list.reduce(_ + _)

  // Partial functions
  val divide: PartialFunction[Int, Int] = {
    case n if n != 0 => 10 / n
  }
  val safeDiv = divide.lift(0) // None

  // Currying
  val add = (a: Int) => (b: Int) => a + b
  val add5 = add(5)
  println(add5(3))

  // Function composition
  val f: Int => Int = _ + 1
  val g: Int => Int = _ * 2
  val composed = f andThen g
  val composed2 = f compose g

  // Closures
  var total = 0
  val adder = (n: Int) => { total += n; total }

  // Anonymous functions
  list.map { n =>
    val doubled = n * 2
    doubled + 1
  }

  // Method references
  list.map(_.toString)
  list.foreach(println)

  // Futures
  val future = Future {
    Thread.sleep(100)
    42
  }

  future.onComplete {
    case scala.util.Success(value) => println(s"Got: $value")
    case scala.util.Failure(e) => println(s"Error: ${e.getMessage}")
  }

  // Await (blocking)
  val result = Await.result(future, 5.seconds)

  // Result ADT
  val success: Result[Int, String] = Success(42)
  val failure: Result[Int, String] = Failure("Error")

  success match {
    case Success(v) => println(s"Value: $v")
    case Failure(e) => println(s"Error: $e")
  }

  // Lazy evaluation
  println(LazyEval.expensiveComputation)

  // Exception handling
  Exceptions.tryCatch()
  println(Exceptions.tryExpression())

  // Apply and unapply
  val pair = Pair(1, "hello")
  val Pair(first, second) = pair
  println(s"First: $first, Second: $second")

  // Singleton
  object Counter {
    private var count = 0
    def next(): Int = { count += 1; count }
  }
  println(Counter.next())
  println(Counter.next())

  // Structural types (duck typing)
  type Closeable = { def close(): Unit }

  // Implicit conversions
  implicit def intToString(n: Int): String = n.toString

  // Context parameters (using/given in Scala 3)
  def printWithOrdering[T](list: List[T])(implicit ord: scala.Ordering[T]): Unit = {
    list.sorted.foreach(println)
  }

  // Regex
  val pattern = """\d+""".r
  val matches = pattern.findAllIn("hello 123 world 456").toList

  // XML literals (if enabled)
  // val xml = <root><item>Hello</item></root>

  // Assertions
  assert(mutable >= 0, "Value must be non-negative")
  require(immutable > 0, "Must be positive")

  // Main output
  println(s"Hello from $Greeting")
  println(s"PI = $Pi")
  println("Program completed successfully!")
}
