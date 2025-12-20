// ==============================================================================
// Comprehensive Dart Sample - Syntax Highlighting Demonstration
// ==============================================================================

// This file demonstrates all major Dart language features
// for syntax highlighting purposes.

import 'dart:async';
import 'dart:collection';
import 'dart:convert';
import 'dart:io';
import 'dart:math';

// Library declaration
library sample;

// Part directive
// part 'sample_part.dart';

// Export
// export 'other_library.dart' show SomeClass;

// ==============================================================================
// Comments
// ==============================================================================

// Single line comment

/* Multi-line
   block comment */

/// Documentation comment
/// Supports [markdown] formatting
/// {@category Core}
/// {@template example}
/// Example template
/// {@endtemplate}

// ==============================================================================
// Variables and Constants
// ==============================================================================

// Variable declarations
var inferredVar = 'inferred type';
String explicitType = 'explicit type';
dynamic dynamicVar = 'can be anything';
Object objectVar = 'object type';

// Final and const
final String finalString = 'cannot be reassigned';
final inferredFinal = 'inferred final';
const double pi = 3.14159;
const List<int> constList = [1, 2, 3];

// Late initialization
late String lateVariable;
late final int lateFinal;

// Nullable types
String? nullableString;
int? nullableInt = null;

// ==============================================================================
// Basic Types
// ==============================================================================

// Numbers
int integer = 42;
double floating = 3.14159;
num number = 100;

// Numeric literals
int hex = 0xDEADBEEF;
int binary = 0b10101010;
double scientific = 1.5e10;
int underscore = 1_000_000;

// Strings
String singleQuote = 'single quotes';
String doubleQuote = "double quotes";
String multiline = '''
This is a
multiline string
''';
String rawString = r'Raw string with \n no escaping';

// String interpolation
String name = 'Dart';
String greeting = 'Hello, $name!';
String expression = 'Sum: ${2 + 2}';

// Booleans
bool isTrue = true;
bool isFalse = false;

// Lists
List<int> list = [1, 2, 3, 4, 5];
var growableList = <String>[];
List<int> filledList = List.filled(5, 0);
List<int> generatedList = List.generate(5, (i) => i * 2);

// Collection if and for
var nav = ['Home', 'Furniture', if (true) 'Plants', 'Outlet'];
var listOfStrings = ['#0', for (var i in [1, 2, 3]) '#$i'];

// Spread operator
var list2 = [0, ...list];
var nullableList = null;
var list3 = [0, ...?nullableList];

// Sets
Set<String> set = {'a', 'b', 'c'};
var emptySet = <int>{};

// Maps
Map<String, int> map = {'one': 1, 'two': 2, 'three': 3};
var emptyMap = <String, dynamic>{};

// Runes and Symbols
Runes runes = Runes('\u2665 \u{1f600}');
Symbol symbol = #symbolName;

// ==============================================================================
// Operators
// ==============================================================================

void operatorsDemo() {
  // Arithmetic
  int a = 10, b = 3;
  print(a + b);  // 13
  print(a - b);  // 7
  print(a * b);  // 30
  print(a / b);  // 3.333...
  print(a ~/ b); // 3 (integer division)
  print(a % b);  // 1 (modulo)

  // Increment/decrement
  int c = 0;
  c++;
  ++c;
  c--;
  --c;

  // Comparison
  print(a == b);
  print(a != b);
  print(a > b);
  print(a < b);
  print(a >= b);
  print(a <= b);

  // Type test
  if (a is int) print('is int');
  if (a is! String) print('is not String');

  // Logical
  bool x = true, y = false;
  print(x && y); // AND
  print(x || y); // OR
  print(!x);     // NOT

  // Bitwise
  print(a & b);  // AND
  print(a | b);  // OR
  print(a ^ b);  // XOR
  print(~a);     // NOT
  print(a << 2); // Left shift
  print(a >> 2); // Right shift
  print(a >>> 2); // Unsigned right shift

  // Null-aware operators
  String? nullableStr;
  print(nullableStr ?? 'default');     // If null
  nullableStr ??= 'assigned if null';  // Assign if null
  print(nullableStr?.length);          // Conditional access

  // Cascade notation
  var list = <int>[]
    ..add(1)
    ..add(2)
    ..add(3);

  // Null-shorting cascade
  nullableStr
    ?..trim()
    ..toLowerCase();

  // Conditional expression
  int result = a > b ? a : b;

  // Assignment operators
  a += 5;
  a -= 5;
  a *= 2;
  a ~/= 2;
}

// ==============================================================================
// Control Flow
// ==============================================================================

void controlFlow() {
  int score = 85;

  // If-else
  if (score >= 90) {
    print('A');
  } else if (score >= 80) {
    print('B');
  } else if (score >= 70) {
    print('C');
  } else {
    print('F');
  }

  // Switch statement
  String command = 'start';
  switch (command) {
    case 'start':
      print('Starting...');
      break;
    case 'stop':
      print('Stopping...');
      break;
    case 'pause':
    case 'resume':
      print('Pause or resume');
      break;
    default:
      print('Unknown command');
  }

  // Switch expression (Dart 3)
  String message = switch (command) {
    'start' => 'Starting the engine',
    'stop' => 'Stopping the engine',
    _ => 'Unknown command',
  };

  // For loop
  for (int i = 0; i < 5; i++) {
    print(i);
  }

  // For-in loop
  for (var item in [1, 2, 3]) {
    print(item);
  }

  // While loop
  int counter = 0;
  while (counter < 5) {
    print(counter);
    counter++;
  }

  // Do-while loop
  do {
    print(counter);
    counter--;
  } while (counter > 0);

  // Break and continue
  for (int i = 0; i < 10; i++) {
    if (i == 3) continue;
    if (i == 7) break;
    print(i);
  }

  // Labels
  outerLoop:
  for (int i = 0; i < 3; i++) {
    for (int j = 0; j < 3; j++) {
      if (i * j > 3) break outerLoop;
      print('$i, $j');
    }
  }

  // Assert
  assert(score > 0, 'Score must be positive');
}

// ==============================================================================
// Functions
// ==============================================================================

// Basic function
void sayHello() {
  print('Hello!');
}

// Function with return type
int add(int a, int b) {
  return a + b;
}

// Arrow function
int multiply(int a, int b) => a * b;

// Optional positional parameters
String greet(String name, [String? greeting]) {
  return '${greeting ?? 'Hello'}, $name!';
}

// Named parameters
void createUser({
  required String name,
  int age = 0,
  String? email,
}) {
  print('Creating user: $name, age: $age');
}

// Function as parameter
void processItems(List<int> items, int Function(int) processor) {
  for (var item in items) {
    print(processor(item));
  }
}

// Anonymous function (lambda)
var double = (int x) => x * 2;
var complexLambda = (int x, int y) {
  var result = x + y;
  return result * 2;
};

// Closures
Function makeAdder(int addend) {
  return (int i) => addend + i;
}

// Generator functions
Iterable<int> naturalsTo(int n) sync* {
  int k = 0;
  while (k < n) yield k++;
}

Stream<int> asynchronousNaturalsTo(int n) async* {
  int k = 0;
  while (k < n) yield k++;
}

// Typedef
typedef IntOperation = int Function(int a, int b);
typedef JsonMap = Map<String, dynamic>;
typedef Callback<T> = void Function(T value);

// ==============================================================================
// Classes
// ==============================================================================

class Animal {
  // Instance variables
  String name;
  int _age; // Private

  // Static variable
  static int count = 0;

  // Constant
  static const String species = 'Unknown';

  // Constructor
  Animal(this.name, this._age) {
    count++;
  }

  // Named constructor
  Animal.baby(this.name) : _age = 0;

  // Factory constructor
  factory Animal.fromJson(Map<String, dynamic> json) {
    return Animal(json['name'], json['age']);
  }

  // Getter
  int get age => _age;

  // Setter
  set age(int value) {
    if (value >= 0) _age = value;
  }

  // Instance method
  void speak() {
    print('$name makes a sound');
  }

  // Static method
  static void printCount() {
    print('Total animals: $count');
  }

  // Operator overloading
  Animal operator +(Animal other) {
    return Animal('${name} & ${other.name}', (_age + other._age) ~/ 2);
  }

  @override
  String toString() => 'Animal(name: $name, age: $_age)';

  @override
  bool operator ==(Object other) =>
      other is Animal && name == other.name && _age == other._age;

  @override
  int get hashCode => Object.hash(name, _age);
}

// Inheritance
class Dog extends Animal {
  String breed;

  Dog(String name, int age, this.breed) : super(name, age);

  Dog.puppy(String name, this.breed) : super.baby(name);

  @override
  void speak() {
    print('$name barks');
  }

  void fetch() {
    print('$name fetches the ball');
  }
}

// Abstract class
abstract class Shape {
  double get area;
  double get perimeter;
  void draw();
}

class Circle implements Shape {
  final double radius;

  Circle(this.radius);

  @override
  double get area => pi * radius * radius;

  @override
  double get perimeter => 2 * pi * radius;

  @override
  void draw() => print('Drawing circle with radius $radius');
}

// Mixin
mixin Flyable {
  void fly() {
    print('Flying!');
  }
}

mixin Swimmable {
  void swim() {
    print('Swimming!');
  }
}

class Duck extends Animal with Flyable, Swimmable {
  Duck(String name, int age) : super(name, age);

  @override
  void speak() {
    print('$name quacks');
  }
}

// Extension
extension StringExtension on String {
  String get reversed => split('').reversed.join('');
  bool get isEmail => contains('@') && contains('.');
}

extension on int {
  int get doubled => this * 2;
  bool get isEven => this % 2 == 0;
}

// ==============================================================================
// Interfaces and Abstract Classes
// ==============================================================================

abstract interface class Printable {
  void printInfo();
}

abstract interface class Serializable {
  Map<String, dynamic> toJson();
  factory Serializable.fromJson(Map<String, dynamic> json) {
    throw UnimplementedError();
  }
}

class Document implements Printable, Serializable {
  final String content;

  Document(this.content);

  @override
  void printInfo() => print(content);

  @override
  Map<String, dynamic> toJson() => {'content': content};
}

// ==============================================================================
// Generics
// ==============================================================================

class Box<T> {
  T value;

  Box(this.value);

  T getValue() => value;
  void setValue(T newValue) => value = newValue;
}

class Pair<K, V> {
  final K key;
  final V value;

  Pair(this.key, this.value);

  @override
  String toString() => 'Pair($key, $value)';
}

// Generic function
T first<T>(List<T> items) {
  return items.first;
}

// Bounded generics
class NumberBox<T extends num> {
  T value;

  NumberBox(this.value);

  T add(T other) => (value + other) as T;
}

// ==============================================================================
// Enums
// ==============================================================================

enum Status { pending, approved, rejected }

// Enhanced enums (Dart 2.17+)
enum Color {
  red('FF0000'),
  green('00FF00'),
  blue('0000FF');

  final String hexCode;

  const Color(this.hexCode);

  String get displayName => name.toUpperCase();
}

enum HttpStatus {
  ok(200, 'OK'),
  notFound(404, 'Not Found'),
  serverError(500, 'Internal Server Error');

  final int code;
  final String message;

  const HttpStatus(this.code, this.message);

  bool get isSuccess => code >= 200 && code < 300;
}

// ==============================================================================
// Exception Handling
// ==============================================================================

class CustomException implements Exception {
  final String message;
  CustomException(this.message);

  @override
  String toString() => 'CustomException: $message';
}

void exceptionDemo() {
  try {
    throw CustomException('Something went wrong');
  } on CustomException catch (e) {
    print('Caught: $e');
  } on FormatException catch (e, stackTrace) {
    print('Format error: $e');
    print('Stack trace: $stackTrace');
  } catch (e) {
    print('Unknown error: $e');
    rethrow;
  } finally {
    print('Cleanup');
  }
}

// ==============================================================================
// Async Programming
// ==============================================================================

Future<String> fetchData() async {
  await Future.delayed(Duration(seconds: 1));
  return 'Data fetched';
}

Future<void> asyncDemo() async {
  // Await
  String data = await fetchData();
  print(data);

  // Future methods
  fetchData()
      .then((value) => print(value))
      .catchError((error) => print(error))
      .whenComplete(() => print('Done'));

  // Multiple futures
  var results = await Future.wait([
    fetchData(),
    fetchData(),
    fetchData(),
  ]);

  // Future.any - first to complete
  var first = await Future.any([
    Future.delayed(Duration(seconds: 1), () => 'First'),
    Future.delayed(Duration(seconds: 2), () => 'Second'),
  ]);
}

// Streams
Stream<int> countStream(int max) async* {
  for (int i = 0; i < max; i++) {
    await Future.delayed(Duration(milliseconds: 100));
    yield i;
  }
}

Future<void> streamDemo() async {
  // Listen
  countStream(5).listen(
    (data) => print(data),
    onError: (error) => print(error),
    onDone: () => print('Done'),
  );

  // Await for
  await for (var value in countStream(5)) {
    print(value);
  }

  // Stream transformations
  var doubled = countStream(5).map((x) => x * 2);
  var evens = countStream(10).where((x) => x.isEven);
  var firstThree = countStream(10).take(3);
}

// Completer
Future<String> fetchWithCompleter() {
  var completer = Completer<String>();

  Future.delayed(Duration(seconds: 1), () {
    completer.complete('Completed');
  });

  return completer.future;
}

// ==============================================================================
// Records (Dart 3.0+)
// ==============================================================================

// Record type
(int, String) getRecord() {
  return (42, 'hello');
}

// Named fields
({int x, int y}) getPoint() {
  return (x: 10, y: 20);
}

void recordsDemo() {
  // Positional record
  var record = (1, 'hello', true);
  print(record.$1); // 1
  print(record.$2); // hello

  // Named record
  var point = (x: 10, y: 20);
  print(point.x);
  print(point.y);

  // Record destructuring
  var (a, b, c) = record;
  var (x: px, y: py) = point;
}

// ==============================================================================
// Patterns (Dart 3.0+)
// ==============================================================================

void patternsDemo() {
  // Pattern matching in switch
  Object obj = [1, 2, 3];

  switch (obj) {
    case [int a, int b, int c]:
      print('List of 3 ints: $a, $b, $c');
    case {'name': String name}:
      print('Map with name: $name');
    case (int x, int y):
      print('Record: $x, $y');
    default:
      print('Unknown');
  }

  // If-case
  if (obj case [int first, ...]) {
    print('List starting with $first');
  }

  // Guard clause
  switch (obj) {
    case [int a, int b] when a > b:
      print('First is greater');
    case [int a, int b]:
      print('Second is greater or equal');
  }
}

// ==============================================================================
// Sealed Classes (Dart 3.0+)
// ==============================================================================

sealed class Result<T> {}

class Success<T> extends Result<T> {
  final T value;
  Success(this.value);
}

class Failure<T> extends Result<T> {
  final Exception error;
  Failure(this.error);
}

String handleResult(Result<int> result) {
  return switch (result) {
    Success(value: var v) => 'Success: $v',
    Failure(error: var e) => 'Error: $e',
  };
}

// ==============================================================================
// Class Modifiers (Dart 3.0+)
// ==============================================================================

// Base class - can be extended but not implemented
base class Vehicle {
  void start() => print('Starting');
}

// Interface class - can be implemented but not extended
interface class Drivable {
  void drive() => print('Driving');
}

// Final class - cannot be extended or implemented
final class Config {
  final String apiUrl;
  Config(this.apiUrl);
}

// Mixin class
mixin class Logger {
  void log(String message) => print('[LOG] $message');
}

// ==============================================================================
// Annotations
// ==============================================================================

class Todo {
  final String task;
  final String author;

  const Todo(this.task, {this.author = 'Unknown'});
}

@Todo('Implement this method', author: 'Developer')
void todoExample() {
  // Implementation pending
}

@deprecated
void oldMethod() {}

@override
String toString() => 'Sample';

// ==============================================================================
// Isolates
// ==============================================================================

Future<void> isolateDemo() async {
  // Spawn an isolate
  final receivePort = ReceivePort();

  await Isolate.spawn(
    isolateEntry,
    receivePort.sendPort,
  );

  final result = await receivePort.first;
  print('Result from isolate: $result');
}

void isolateEntry(SendPort sendPort) {
  // Do heavy computation
  int result = 0;
  for (int i = 0; i < 1000000; i++) {
    result += i;
  }
  sendPort.send(result);
}

// ==============================================================================
// Main Entry Point
// ==============================================================================

void main(List<String> arguments) async {
  print('=== Dart Sample Program ===\n');

  // Using classes
  var dog = Dog('Buddy', 3, 'Labrador');
  dog.speak();
  dog.fetch();

  // Using generics
  var box = Box<int>(42);
  print('Box value: ${box.getValue()}');

  // Using extensions
  print('Reversed: ${'hello'.reversed}');
  print('Is even: ${4.isEven}');

  // Using enhanced enums
  print('Color: ${Color.red.displayName}');
  print('HTTP: ${HttpStatus.ok.message}');

  // Using records
  var (x, y) = (10, 20);
  print('Point: $x, $y');

  // Async operations
  var data = await fetchData();
  print(data);

  print('\n=== Program Complete ===');
}
