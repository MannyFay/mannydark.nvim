/**
 * Comprehensive D language sample demonstrating all syntax features
 * D is a systems programming language with modern features
 */
module sample;

// Import declarations
import std.stdio;
import std.string;
import std.array;
import std.algorithm;
import std.range;
import std.conv;
import std.traits;
import std.typecons;
import std.functional;
import std.exception;
import std.format;
import std.file;
import std.path;
import std.datetime;
import std.json;
import std.regex;
import std.parallelism;
import std.concurrency;
import core.thread;
import core.sync.mutex;
import core.atomic;
import core.memory;

// Version and debug conditions
version (Windows) {
    enum PLATFORM = "Windows";
}
else version (linux) {
    enum PLATFORM = "Linux";
}
else version (OSX) {
    enum PLATFORM = "macOS";
}
else {
    enum PLATFORM = "Unknown";
}

debug {
    enum DEBUG_MODE = true;
}
else {
    enum DEBUG_MODE = false;
}

// Static assertions
static assert(int.sizeof == 4, "int must be 4 bytes");
static assert(is(typeof(42) == int));

// Constants and enums
enum MAX_BUFFER_SIZE = 1024;
enum PI = 3.14159265358979323846;
enum GREETING = "Hello, D!";

// Enum type
enum Color : ubyte {
    red = 0,
    green = 1,
    blue = 2,
    alpha = 255
}

// Enum with string values
enum Status {
    ok = "OK",
    error = "ERROR",
    pending = "PENDING"
}

// Flags enum
enum Flags {
    none = 0,
    read = 1 << 0,
    write = 1 << 1,
    execute = 1 << 2,
    all = read | write | execute
}

// Type aliases
alias StringList = string[];
alias IntPair = Tuple!(int, int);
alias Callback = void delegate(int);
alias PureFunc = int function(int) pure nothrow @nogc;

// Struct definition
struct Point {
    double x = 0.0;
    double y = 0.0;
    double z = 0.0;

    // Constructor
    this(double x, double y, double z = 0.0) {
        this.x = x;
        this.y = y;
        this.z = z;
    }

    // Copy constructor
    this(ref return scope const Point other) {
        this.x = other.x;
        this.y = other.y;
        this.z = other.z;
    }

    // Operator overloading
    Point opBinary(string op)(const Point other) const if (op == "+") {
        return Point(x + other.x, y + other.y, z + other.z);
    }

    Point opBinary(string op)(const Point other) const if (op == "-") {
        return Point(x - other.x, y - other.y, z - other.z);
    }

    Point opBinary(string op)(double scalar) const if (op == "*") {
        return Point(x * scalar, y * scalar, z * scalar);
    }

    // Comparison
    int opCmp(ref const Point other) const {
        if (x != other.x) return x < other.x ? -1 : 1;
        if (y != other.y) return y < other.y ? -1 : 1;
        if (z != other.z) return z < other.z ? -1 : 1;
        return 0;
    }

    bool opEquals(ref const Point other) const {
        return x == other.x && y == other.y && z == other.z;
    }

    // Hash
    size_t toHash() const nothrow @safe {
        return typeid(x).getHash(&x) ^ typeid(y).getHash(&y) ^ typeid(z).getHash(&z);
    }

    // String conversion
    string toString() const {
        return format("Point(%s, %s, %s)", x, y, z);
    }

    // Properties
    @property double magnitude() const pure nothrow @nogc {
        import std.math : sqrt;
        return sqrt(x * x + y * y + z * z);
    }

    // Methods
    double distance(const Point other) const pure nothrow @nogc {
        import std.math : sqrt;
        auto dx = x - other.x;
        auto dy = y - other.y;
        auto dz = z - other.z;
        return sqrt(dx * dx + dy * dy + dz * dz);
    }

    // Static method
    static Point origin() {
        return Point(0, 0, 0);
    }
}

// Class definition
class Shape {
    // Member variables
    protected string name_;
    private Color color_ = Color.red;

    // Constructor
    this(string name) {
        this.name_ = name;
    }

    // Destructor
    ~this() {
        debug writefln("Shape %s destroyed", name_);
    }

    // Abstract methods
    abstract double area() const;
    abstract double perimeter() const;

    // Virtual method
    void draw() const {
        writefln("Drawing %s", name_);
    }

    // Properties
    @property string name() const { return name_; }
    @property void name(string value) { name_ = value; }

    @property Color color() const { return color_; }
    @property void color(Color value) { color_ = value; }

    // Final method
    final string describe() const {
        return format("%s: area=%.2f, perimeter=%.2f", name_, area(), perimeter());
    }

    // Static method
    static Shape createDefault() {
        return new Circle(1.0);
    }
}

// Derived class
class Circle : Shape {
    private double radius_;

    this(double radius) {
        super("Circle");
        this.radius_ = radius;
    }

    override double area() const {
        return PI * radius_ * radius_;
    }

    override double perimeter() const {
        return 2 * PI * radius_;
    }

    override void draw() const {
        super.draw();
        writefln("  radius: %.2f", radius_);
    }

    @property double radius() const { return radius_; }
    @property void radius(double value) { radius_ = value; }
}

// Another derived class
class Rectangle : Shape {
    protected double width_;
    protected double height_;

    this(double width, double height) {
        super("Rectangle");
        this.width_ = width;
        this.height_ = height;
    }

    override double area() const {
        return width_ * height_;
    }

    override double perimeter() const {
        return 2 * (width_ + height_);
    }

    @property double width() const { return width_; }
    @property double height() const { return height_; }
}

// Interface
interface Drawable {
    void draw() const;
    void update(double dt);
}

interface Serializable {
    ubyte[] serialize() const;
    void deserialize(const(ubyte)[] data);
}

// Class implementing interfaces
class Sprite : Drawable, Serializable {
    private Point position_;
    private string texturePath_;

    this(Point position, string texturePath) {
        this.position_ = position;
        this.texturePath_ = texturePath;
    }

    void draw() const {
        writefln("Drawing sprite at %s", position_);
    }

    void update(double dt) {
        // Update logic
    }

    ubyte[] serialize() const {
        return cast(ubyte[]) format("%s,%s,%s,%s",
            position_.x, position_.y, position_.z, texturePath_);
    }

    void deserialize(const(ubyte)[] data) {
        // Deserialize logic
    }
}

// Template struct
struct Container(T) {
    private T[] items_;
    private size_t count_;

    this(size_t initialCapacity) {
        items_ = new T[initialCapacity];
        count_ = 0;
    }

    void add(T item) {
        if (count_ >= items_.length) {
            items_.length = items_.length == 0 ? 8 : items_.length * 2;
        }
        items_[count_++] = item;
    }

    T get(size_t index) const {
        enforce(index < count_, "Index out of bounds");
        return items_[index];
    }

    @property size_t length() const { return count_; }
    @property bool empty() const { return count_ == 0; }

    // Range interface
    auto opSlice() {
        return items_[0 .. count_];
    }

    // Index operator
    ref T opIndex(size_t i) {
        return items_[i];
    }
}

// Template function
T max(T)(T a, T b) if (is(typeof(a > b))) {
    return a > b ? a : b;
}

T min(T)(T a, T b) if (is(typeof(a < b))) {
    return a < b ? a : b;
}

// Variadic template function
void print(Args...)(Args args) {
    foreach (arg; args) {
        write(arg, " ");
    }
    writeln();
}

// Template with constraints
auto sum(Range)(Range range)
if (isInputRange!Range && is(typeof(range.front + range.front))) {
    alias E = ElementType!Range;
    E result = E.init;
    foreach (element; range) {
        result += element;
    }
    return result;
}

// Mixin template
mixin template Logging() {
    void log(string message) {
        writefln("[%s] %s", Clock.currTime(), message);
    }

    void logError(string message) {
        stderr.writefln("[ERROR] %s", message);
    }
}

// Class using mixin
class Logger {
    mixin Logging;

    void doSomething() {
        log("Doing something...");
    }
}

// CTFE (Compile-Time Function Evaluation)
int factorial(int n) pure nothrow @safe {
    return n <= 1 ? 1 : n * factorial(n - 1);
}

enum fact5 = factorial(5); // Computed at compile time

// String mixin
enum generateGetter(string name) = `
    @property auto ` ~ name ~ `() const {
        return ` ~ name ~ `_;
    }
`;

// UFCS (Uniform Function Call Syntax)
int double_(int x) pure nothrow @safe {
    return x * 2;
}

bool isEven(int x) pure nothrow @safe {
    return x % 2 == 0;
}

// Contract programming
int safeDivide(int a, int b)
in {
    assert(b != 0, "Division by zero");
}
out (result) {
    assert(result * b == a || a % b != 0);
}
do {
    return a / b;
}

// Invariant
class BoundedValue {
    private int value_;
    private int min_;
    private int max_;

    this(int min, int max, int initial) {
        min_ = min;
        max_ = max;
        value_ = initial;
    }

    invariant {
        assert(value_ >= min_ && value_ <= max_,
            format("Value %d out of bounds [%d, %d]", value_, min_, max_));
    }

    void setValue(int v) {
        value_ = v < min_ ? min_ : (v > max_ ? max_ : v);
    }

    @property int value() const { return value_; }
}

// Scope guards
void scopeGuardExample() {
    auto file = File("test.txt", "w");
    scope(exit) file.close();
    scope(failure) writeln("An error occurred!");
    scope(success) writeln("Operation successful!");

    file.writeln("Hello, World!");
}

// Lazy evaluation
void lazyExample(lazy int value) {
    writeln("Before evaluation");
    writeln("Value: ", value); // Evaluated here
    writeln("After evaluation");
}

// Range-based programming
void rangeExamples() {
    auto numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10];

    // Filter and map
    auto evenDoubled = numbers
        .filter!(n => n % 2 == 0)
        .map!(n => n * 2)
        .array;

    // Take and drop
    auto firstThree = numbers.take(3).array;
    auto skipTwo = numbers.drop(2).array;

    // Reduce/fold
    auto sumVal = numbers.reduce!((a, b) => a + b);
    auto product = numbers.fold!((a, b) => a * b)(1);

    // Chaining
    auto result = iota(1, 100)
        .filter!(n => n % 3 == 0)
        .map!(n => n * n)
        .take(5)
        .array;

    // Custom range
    auto infinite = generate!(() => 42);
    auto fiveFortyTwos = infinite.take(5).array;

    // Zip
    auto names = ["Alice", "Bob", "Charlie"];
    auto ages = [25, 30, 35];
    foreach (pair; zip(names, ages)) {
        writefln("%s is %d years old", pair[0], pair[1]);
    }

    // Parallel foreach
    foreach (n; parallel(numbers)) {
        writeln("Processing: ", n);
    }
}

// Concurrency
void concurrencyExamples() {
    // Spawn thread
    auto tid = spawn((string msg) {
        writeln("Received: ", msg);
    }, "Hello from main thread");

    // Message passing
    spawn(() {
        receive(
            (int x) { writeln("Got int: ", x); },
            (string s) { writeln("Got string: ", s); },
            (Variant v) { writeln("Got variant"); }
        );
    });

    // Shared data
    shared int counter = 0;
    atomicOp!"+="(counter, 1);

    // Mutex
    auto mutex = new shared Mutex();
    synchronized (mutex) {
        // Critical section
    }

    // Task pool
    auto pool = new TaskPool(4);
    scope(exit) pool.stop();

    auto task = pool.put(task!(() => factorial(10)));
    auto taskResult = task.yieldForce();
}

// Exception handling
class CustomException : Exception {
    int errorCode;

    this(string msg, int code, string file = __FILE__, size_t line = __LINE__) {
        super(msg, file, line);
        this.errorCode = code;
    }
}

void exceptionExamples() {
    try {
        throw new CustomException("Something went wrong", 42);
    }
    catch (CustomException e) {
        writefln("Custom error %d: %s", e.errorCode, e.msg);
    }
    catch (Exception e) {
        writeln("General exception: ", e.msg);
    }
    finally {
        writeln("Cleanup");
    }

    // Enforce
    auto value = enforce(getValue(), "Failed to get value");

    // Assert
    assert(value > 0, "Value must be positive");

    // Nothrow
    auto safeResult = assumeWontThrow(safeOperation());
}

int getValue() { return 42; }
int safeOperation() nothrow { return 1; }

// Attributes
@safe pure nothrow @nogc
int pureFunction(int x) {
    return x * 2;
}

@trusted
void trustedFunction() {
    // Can call @system code
}

@system
void systemFunction() {
    // Low-level operations
}

// User-defined attributes
enum Serialize;
enum Ignore;

struct JsonField {
    string name;
}

struct Person {
    @JsonField("full_name")
    string name;

    @Serialize
    int age;

    @Ignore
    string password;
}

// Compile-time reflection
void reflectionExample(T)(T obj) {
    foreach (member; __traits(allMembers, T)) {
        static if (__traits(compiles, __traits(getMember, obj, member))) {
            writefln("%s = %s", member, __traits(getMember, obj, member));
        }
    }
}

// Operator overloading
struct Vector {
    double x, y;

    // Binary operators
    Vector opBinary(string op)(Vector rhs) const {
        static if (op == "+") return Vector(x + rhs.x, y + rhs.y);
        else static if (op == "-") return Vector(x - rhs.x, y - rhs.y);
        else static if (op == "*") return Vector(x * rhs.x, y * rhs.y);
        else static assert(0, "Operator " ~ op ~ " not implemented");
    }

    // Unary operators
    Vector opUnary(string op)() const {
        static if (op == "-") return Vector(-x, -y);
        else static if (op == "~") return Vector(y, x);
        else static assert(0, "Operator " ~ op ~ " not implemented");
    }

    // Assignment operators
    ref Vector opOpAssign(string op)(Vector rhs) {
        mixin("x " ~ op ~ "= rhs.x;");
        mixin("y " ~ op ~ "= rhs.y;");
        return this;
    }

    // Index operator
    double opIndex(size_t i) const {
        return i == 0 ? x : y;
    }

    // Cast operator
    T opCast(T : double)() const {
        import std.math : sqrt;
        return sqrt(x * x + y * y);
    }
}

// Main function
void main(string[] args) {
    // Variable declarations
    int a = 42;
    double b = 3.14;
    string c = "Hello";
    bool d = true;
    char e = 'X';

    // Type inference
    auto inferredInt = 42;
    auto inferredString = "auto string";
    auto inferredArray = [1, 2, 3];

    // Immutable and const
    immutable int constValue = 100;
    const string constString = "const";

    // Arrays
    int[] dynamicArray = [1, 2, 3, 4, 5];
    int[5] staticArray = [1, 2, 3, 4, 5];
    int[string] assocArray = ["one": 1, "two": 2, "three": 3];

    // Array operations
    dynamicArray ~= 6; // Append
    auto slice = dynamicArray[1 .. 4];
    auto length = dynamicArray.length;

    // Strings
    string str = "Hello, World!";
    wstring wideStr = "Wide string"w;
    dstring dcharStr = "D-char string"d;

    // String operations
    auto upper = str.toUpper();
    auto split = str.split(",");
    auto replaced = str.replace("World", "D");

    // Numeric literals
    int decimal = 42;
    int hex = 0xDEAD_BEEF;
    int octal = 0o755;
    int binary = 0b1010_1010;
    float floatVal = 3.14f;
    double doubleVal = 3.14159;
    real realVal = 3.14159265358979L;

    // Control flow
    if (a > 0) {
        writeln("Positive");
    } else if (a < 0) {
        writeln("Negative");
    } else {
        writeln("Zero");
    }

    // Ternary operator
    auto result = a > 0 ? "positive" : "non-positive";

    // Switch statement
    switch (a) {
        case 0:
            writeln("Zero");
            break;
        case 1: .. case 10:
            writeln("1-10");
            break;
        case 42:
            writeln("The answer");
            break;
        default:
            writeln("Other");
    }

    // Final switch (must cover all cases)
    final switch (Color.red) {
        case Color.red: break;
        case Color.green: break;
        case Color.blue: break;
        case Color.alpha: break;
    }

    // Loops
    for (int i = 0; i < 10; i++) {
        if (i == 5) continue;
        if (i == 8) break;
        writeln(i);
    }

    foreach (item; dynamicArray) {
        writeln(item);
    }

    foreach (i, item; dynamicArray) {
        writefln("[%d] = %d", i, item);
    }

    foreach_reverse (item; dynamicArray) {
        writeln(item);
    }

    while (a > 0) {
        a--;
    }

    do {
        a++;
    } while (a < 10);

    // Labeled loops
    outer: foreach (i; 0 .. 10) {
        foreach (j; 0 .. 10) {
            if (i * j > 50) break outer;
        }
    }

    // Struct usage
    auto point = Point(1, 2, 3);
    writeln(point);
    writeln("Magnitude: ", point.magnitude);

    // Class usage
    Shape shape = new Circle(5);
    shape.draw();
    writeln(shape.describe());

    // Container usage
    auto container = Container!int(10);
    container.add(1);
    container.add(2);
    container.add(3);

    foreach (item; container[]) {
        writeln(item);
    }

    // UFCS
    auto doubled = 5.double_();
    auto isEvenResult = 4.isEven();

    // Lambda/delegate
    auto square = (int x) => x * x;
    auto cube = delegate int(int x) { return x * x * x; };

    // Higher-order functions
    auto numbers = [1, 2, 3, 4, 5];
    auto squared = numbers.map!(x => x * x).array;
    auto evens = numbers.filter!(x => x % 2 == 0).array;

    // Tuple
    auto tuple = tuple(1, "hello", 3.14);
    writeln(tuple[0], " ", tuple[1], " ", tuple[2]);

    // Nullable
    Nullable!int maybeInt;
    if (maybeInt.isNull) {
        maybeInt = 42;
    }
    writeln(maybeInt.get);

    // File I/O
    auto file = File("test.txt", "w");
    file.writeln("Hello, File!");
    file.close();

    // Read file
    if (exists("test.txt")) {
        auto content = readText("test.txt");
        writeln(content);
    }

    // JSON
    auto json = parseJSON(`{"name": "John", "age": 30}`);
    writeln(json["name"].str);
    writeln(json["age"].integer);

    // Regex
    auto pattern = regex(r"\d+");
    auto matched = matchFirst("Hello 123 World", pattern);
    if (matched) {
        writeln("Found: ", matched.hit);
    }

    // Date/Time
    auto now = Clock.currTime();
    writefln("Current time: %s", now);

    auto date = Date(2024, 1, 15);
    writefln("Date: %s", date);

    // Ranges
    rangeExamples();

    // Compile-time
    static assert(fact5 == 120);
    writefln("5! = %d (computed at compile time)", fact5);

    // Memory management
    auto gc = GC.stats;
    writefln("GC used: %d bytes", gc.usedSize);

    // Platform info
    writefln("Platform: %s", PLATFORM);
    writefln("Debug mode: %s", DEBUG_MODE);

    writeln("Program completed successfully!");
}

// Unit tests
unittest {
    assert(factorial(5) == 120);
    assert(max(3, 5) == 5);
    assert(min(3, 5) == 3);

    auto p1 = Point(0, 0, 0);
    auto p2 = Point(3, 4, 0);
    assert(p1.distance(p2) == 5.0);

    auto v1 = Vector(1, 2);
    auto v2 = Vector(3, 4);
    auto v3 = v1 + v2;
    assert(v3.x == 4 && v3.y == 6);
}
