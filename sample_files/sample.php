<?php

declare(strict_types=1);

/**
 * Comprehensive PHP language sample demonstrating all syntax features.
 *
 * PHP is a server-side scripting language widely used for web development.
 *
 * @author Sample Author
 * @version 1.0.0
 * @package Sample
 */

namespace Sample;

use ArrayAccess;
use Countable;
use Exception;
use Iterator;
use JsonSerializable;
use RuntimeException;
use Stringable;
use Throwable;
use WeakReference;

// ============================================================================
// Constants
// ============================================================================

const MAX_BUFFER_SIZE = 1024;
const PI = 3.14159265358979323846;
const GREETING = 'Hello, PHP!';

define('RUNTIME_CONSTANT', time());

// ============================================================================
// Enums (PHP 8.1+)
// ============================================================================

/**
 * Color enumeration with backed values
 */
enum Color: int
{
    case Red = 0xFF0000;
    case Green = 0x00FF00;
    case Blue = 0x0000FF;
    case White = 0xFFFFFF;
    case Black = 0x000000;

    public function toHexString(): string
    {
        return sprintf('#%06X', $this->value);
    }

    public function displayName(): string
    {
        return match ($this) {
            self::Red => 'Red',
            self::Green => 'Green',
            self::Blue => 'Blue',
            self::White => 'White',
            self::Black => 'Black',
        };
    }

    public static function fromHex(int $hex): ?self
    {
        return match ($hex) {
            0xFF0000 => self::Red,
            0x00FF00 => self::Green,
            0x0000FF => self::Blue,
            0xFFFFFF => self::White,
            0x000000 => self::Black,
            default => null,
        };
    }
}

/**
 * Status enumeration (unit enum)
 */
enum Status
{
    case Ok;
    case Error;
    case Pending;
    case Timeout;
}

/**
 * String-backed enum
 */
enum HttpMethod: string
{
    case GET = 'GET';
    case POST = 'POST';
    case PUT = 'PUT';
    case DELETE = 'DELETE';
    case PATCH = 'PATCH';
}

// ============================================================================
// Interfaces
// ============================================================================

/**
 * Shape interface
 */
interface ShapeInterface
{
    public function area(): float;
    public function perimeter(): float;
    public function describe(): string;
}

/**
 * Drawable interface
 */
interface DrawableInterface
{
    public function draw(): void;
    public function update(float $deltaTime): void;
}

/**
 * Generic container interface
 */
interface ContainerInterface extends Countable, Iterator
{
    public function add(mixed $item): void;
    public function get(int $index): mixed;
    public function isEmpty(): bool;
}

// ============================================================================
// Traits
// ============================================================================

/**
 * Logging trait
 */
trait LoggableTrait
{
    public function log(string $message): void
    {
        echo "[" . static::class . "] $message\n";
    }

    public function logError(string $message): void
    {
        error_log("[ERROR] $message");
    }
}

/**
 * Serializable trait
 */
trait SerializableTrait
{
    public function toArray(): array
    {
        $result = [];
        foreach (get_object_vars($this) as $key => $value) {
            $result[$key] = $value;
        }
        return $result;
    }

    public function toJson(): string
    {
        return json_encode($this->toArray(), JSON_THROW_ON_ERROR);
    }
}

/**
 * Singleton trait
 */
trait SingletonTrait
{
    private static ?self $instance = null;

    public static function getInstance(): self
    {
        if (self::$instance === null) {
            self::$instance = new self();
        }
        return self::$instance;
    }

    private function __construct() {}
    private function __clone() {}
    public function __wakeup(): void
    {
        throw new RuntimeException("Cannot unserialize singleton");
    }
}

// ============================================================================
// Classes
// ============================================================================

/**
 * Point class with readonly properties (PHP 8.1+)
 */
readonly class Point implements JsonSerializable, Stringable
{
    public function __construct(
        public float $x = 0.0,
        public float $y = 0.0,
        public float $z = 0.0,
    ) {}

    public function distance(Point $other): float
    {
        $dx = $this->x - $other->x;
        $dy = $this->y - $other->y;
        $dz = $this->z - $other->z;
        return sqrt($dx ** 2 + $dy ** 2 + $dz ** 2);
    }

    public function add(Point $other): Point
    {
        return new Point(
            $this->x + $other->x,
            $this->y + $other->y,
            $this->z + $other->z,
        );
    }

    public function subtract(Point $other): Point
    {
        return new Point(
            $this->x - $other->x,
            $this->y - $other->y,
            $this->z - $other->z,
        );
    }

    public function multiply(float $scalar): Point
    {
        return new Point(
            $this->x * $scalar,
            $this->y * $scalar,
            $this->z * $scalar,
        );
    }

    public static function origin(): Point
    {
        return new Point(0, 0, 0);
    }

    public function jsonSerialize(): array
    {
        return ['x' => $this->x, 'y' => $this->y, 'z' => $this->z];
    }

    public function __toString(): string
    {
        return sprintf("Point(%g, %g, %g)", $this->x, $this->y, $this->z);
    }
}

/**
 * Person class with constructor promotion
 */
class Person implements JsonSerializable, Stringable
{
    use SerializableTrait;

    public function __construct(
        public readonly string $name,
        public int $age,
        public ?string $email = null,
    ) {}

    public function isAdult(): bool
    {
        return $this->age >= 18;
    }

    public function withAge(int $age): self
    {
        return new self($this->name, $age, $this->email);
    }

    public function jsonSerialize(): array
    {
        return [
            'name' => $this->name,
            'age' => $this->age,
            'email' => $this->email,
        ];
    }

    public function __toString(): string
    {
        return "{$this->name} ({$this->age})";
    }
}

/**
 * Abstract shape class
 */
abstract class Shape implements ShapeInterface, DrawableInterface
{
    use LoggableTrait;

    protected string $name = 'Shape';
    protected Color $color = Color::Black;

    public function __construct(string $name)
    {
        $this->name = $name;
    }

    public function getName(): string
    {
        return $this->name;
    }

    public function getColor(): Color
    {
        return $this->color;
    }

    public function setColor(Color $color): void
    {
        $this->color = $color;
    }

    abstract public function area(): float;
    abstract public function perimeter(): float;

    public function describe(): string
    {
        return sprintf("%s: area=%.2f, perimeter=%.2f",
            $this->name, $this->area(), $this->perimeter());
    }

    public function draw(): void
    {
        $this->log("Drawing {$this->name} in {$this->color->displayName()}");
    }

    public function update(float $deltaTime): void
    {
        // Default implementation
    }
}

/**
 * Circle class (final)
 */
final class Circle extends Shape
{
    public function __construct(
        private float $radius,
        Color $color = Color::Black,
    ) {
        parent::__construct('Circle');
        $this->color = $color;
    }

    public function getRadius(): float
    {
        return $this->radius;
    }

    public function setRadius(float $radius): void
    {
        if ($radius < 0) {
            throw new \InvalidArgumentException('Radius cannot be negative');
        }
        $this->radius = $radius;
    }

    public function area(): float
    {
        return PI * $this->radius ** 2;
    }

    public function perimeter(): float
    {
        return 2 * PI * $this->radius;
    }

    public function draw(): void
    {
        parent::draw();
        $this->log("  radius: {$this->radius}");
    }
}

/**
 * Rectangle class
 */
class Rectangle extends Shape
{
    public function __construct(
        protected float $width,
        protected float $height,
        Color $color = Color::Black,
    ) {
        parent::__construct('Rectangle');
        $this->color = $color;
    }

    public function getWidth(): float
    {
        return $this->width;
    }

    public function getHeight(): float
    {
        return $this->height;
    }

    public function area(): float
    {
        return $this->width * $this->height;
    }

    public function perimeter(): float
    {
        return 2 * ($this->width + $this->height);
    }
}

/**
 * Square class
 */
class Square extends Rectangle
{
    public function __construct(float $side, Color $color = Color::Black)
    {
        parent::__construct($side, $side, $color);
        $this->name = 'Square';
    }

    public function getSide(): float
    {
        return $this->width;
    }

    public function setSide(float $side): void
    {
        $this->width = $side;
        $this->height = $side;
    }
}

// ============================================================================
// Generic-like Container
// ============================================================================

/**
 * Generic container class
 */
class Container implements ContainerInterface, ArrayAccess
{
    private array $items = [];
    private int $position = 0;

    public function add(mixed $item): void
    {
        $this->items[] = $item;
    }

    public function get(int $index): mixed
    {
        return $this->items[$index] ?? null;
    }

    public function count(): int
    {
        return count($this->items);
    }

    public function isEmpty(): bool
    {
        return empty($this->items);
    }

    // Iterator implementation
    public function current(): mixed
    {
        return $this->items[$this->position];
    }

    public function key(): int
    {
        return $this->position;
    }

    public function next(): void
    {
        $this->position++;
    }

    public function rewind(): void
    {
        $this->position = 0;
    }

    public function valid(): bool
    {
        return isset($this->items[$this->position]);
    }

    // ArrayAccess implementation
    public function offsetExists(mixed $offset): bool
    {
        return isset($this->items[$offset]);
    }

    public function offsetGet(mixed $offset): mixed
    {
        return $this->items[$offset] ?? null;
    }

    public function offsetSet(mixed $offset, mixed $value): void
    {
        if ($offset === null) {
            $this->items[] = $value;
        } else {
            $this->items[$offset] = $value;
        }
    }

    public function offsetUnset(mixed $offset): void
    {
        unset($this->items[$offset]);
    }
}

// ============================================================================
// Exceptions
// ============================================================================

/**
 * Custom exception
 */
class AppException extends Exception
{
    public function __construct(
        string $message,
        public readonly int $errorCode = 0,
        ?Throwable $previous = null,
    ) {
        parent::__construct($message, 0, $previous);
    }
}

/**
 * Validation exception
 */
class ValidationException extends AppException
{
    public function __construct(string $message)
    {
        parent::__construct($message, 400);
    }
}

// ============================================================================
// Attributes (PHP 8.0+)
// ============================================================================

#[Attribute(Attribute::TARGET_CLASS | Attribute::TARGET_METHOD)]
class Route
{
    public function __construct(
        public string $path,
        public string $method = 'GET',
    ) {}
}

#[Attribute(Attribute::TARGET_PROPERTY)]
class Validate
{
    public function __construct(
        public string $rule,
        public ?string $message = null,
    ) {}
}

#[Attribute(Attribute::TARGET_PARAMETER)]
class FromQuery
{
    public function __construct(public ?string $name = null) {}
}

// ============================================================================
// Functions
// ============================================================================

/**
 * Add two numbers
 */
function add(int|float $a, int|float $b): int|float
{
    return $a + $b;
}

/**
 * Greet someone
 */
function greet(string $name, string $greeting = 'Hello'): string
{
    return "$greeting, $name!";
}

/**
 * Sum all numbers
 */
function sumAll(int|float ...$numbers): int|float
{
    return array_sum($numbers);
}

/**
 * Higher-order function
 */
function applyTwice(callable $func, mixed $value): mixed
{
    return $func($func($value));
}

/**
 * Create multiplier closure
 */
function makeMultiplier(int|float $factor): callable
{
    return fn(int|float $x): int|float => $x * $factor;
}

/**
 * Factorial (recursive)
 */
function factorial(int $n): int
{
    if ($n <= 1) return 1;
    return $n * factorial($n - 1);
}

/**
 * Memoized fibonacci using static
 */
function fibonacci(int $n): int
{
    static $cache = [];

    if ($n < 2) return $n;
    if (isset($cache[$n])) return $cache[$n];

    return $cache[$n] = fibonacci($n - 1) + fibonacci($n - 2);
}

/**
 * Generator function
 */
function range_generator(int $start, int $end): \Generator
{
    for ($i = $start; $i <= $end; $i++) {
        yield $i;
    }
}

/**
 * Fibonacci generator
 */
function fibonacci_generator(): \Generator
{
    $a = 0;
    $b = 1;
    while (true) {
        yield $a;
        [$a, $b] = [$b, $a + $b];
    }
}

// ============================================================================
// Main Script
// ============================================================================

// Variable declarations
$integer = 42;
$float = 3.14;
$string = "Hello, World!";
$bool = true;
$null = null;

// Numeric literals
$decimal = 1_000_000;
$hex = 0xDEADBEEF;
$octal = 0755;
$binary = 0b10101010;
$scientific = 1.23e-4;

// Strings
$singleQuote = 'Hello';
$doubleQuote = "Hello, $integer";
$heredoc = <<<EOT
This is a heredoc
with variable interpolation: $integer
EOT;
$nowdoc = <<<'EOT'
This is a nowdoc
without interpolation: $integer
EOT;

// String interpolation
$complex = "Value: {$integer}";
$expression = "Sum: " . ($integer + 10);

// Arrays
$indexedArray = [1, 2, 3, 4, 5];
$associativeArray = ['one' => 1, 'two' => 2, 'three' => 3];
$mixedArray = [1, 'two' => 2, 3, 'four' => 4];
$multiArray = [
    ['a', 'b', 'c'],
    ['d', 'e', 'f'],
];

// Array operations
$indexedArray[] = 6;           // Append
$first = $indexedArray[0];     // Access
$last = end($indexedArray);    // Last element
$length = count($indexedArray);
$keys = array_keys($associativeArray);
$values = array_values($associativeArray);
$merged = array_merge($indexedArray, [7, 8, 9]);
$filtered = array_filter($indexedArray, fn($n) => $n % 2 === 0);
$mapped = array_map(fn($n) => $n * 2, $indexedArray);
$reduced = array_reduce($indexedArray, fn($acc, $n) => $acc + $n, 0);

// Spread operator
$spread = [...$indexedArray, 7, 8, 9];
$spreadAssoc = [...$associativeArray, 'four' => 4];

// Null coalescing
$default = $nullVariable ?? 'default';
$nullCoalesceAssign ??= 'assigned';

// Nullsafe operator (PHP 8.0+)
$person = new Person('Alice', 30, 'alice@example.com');
$emailLength = $person?->email?->length ?? 0;

// Control flow
if ($integer > 0) {
    echo "Positive\n";
} elseif ($integer < 0) {
    echo "Negative\n";
} else {
    echo "Zero\n";
}

// Ternary
$result = $integer > 0 ? 'positive' : 'non-positive';

// Elvis operator
$elvis = $integer ?: 'default';

// Match expression (PHP 8.0+)
$description = match (true) {
    $integer === 0 => 'Zero',
    $integer > 0 && $integer <= 10 => '1-10',
    $integer > 10 && $integer <= 100 => '11-100',
    $integer < 0 => 'Negative',
    default => 'Large',
};

// Switch
switch ($integer) {
    case 0:
        echo "Zero\n";
        break;
    case 1:
    case 2:
    case 3:
        echo "1-3\n";
        break;
    default:
        echo "Other\n";
}

// Loops
for ($i = 0; $i < 10; $i++) {
    if ($i === 5) continue;
    if ($i === 8) break;
    echo "$i\n";
}

foreach ($indexedArray as $item) {
    echo "$item\n";
}

foreach ($associativeArray as $key => $value) {
    echo "$key => $value\n";
}

$counter = 0;
while ($counter < 5) {
    $counter++;
}

do {
    $counter--;
} while ($counter > 0);

// Exception handling
try {
    throw new AppException('Something went wrong', 42);
} catch (ValidationException $e) {
    echo "Validation error: {$e->getMessage()}\n";
} catch (AppException $e) {
    echo "App error {$e->errorCode}: {$e->getMessage()}\n";
} catch (Exception $e) {
    echo "Error: {$e->getMessage()}\n";
    throw $e;
} finally {
    echo "Cleanup\n";
}

// Objects
$point = new Point(1, 2, 3);
echo "$point\n";
echo "Distance: " . $point->distance(Point::origin()) . "\n";

echo "$person\n";

$circle = new Circle(5, Color::Blue);
$circle->draw();
echo "Area: {$circle->area()}\n";

$rect = new Rectangle(4, 6);
echo $rect->describe() . "\n";

// Container
$container = new Container();
$container->add('Hello');
$container->add('World');
$container[] = '!';

foreach ($container as $item) {
    echo "$item\n";
}

// Closures
$double = fn($x) => $x * 2;
$triple = makeMultiplier(3);

echo "Double 5: {$double(5)}\n";
echo "Triple 5: {$triple(5)}\n";

// Closure with use
$factor = 10;
$multiply = function ($x) use ($factor) {
    return $x * $factor;
};
echo "Multiply 5: {$multiply(5)}\n";

// Arrow functions (PHP 7.4+)
$numbers = [1, 2, 3, 4, 5];
$squared = array_map(fn($n) => $n ** 2, $numbers);
$evens = array_filter($numbers, fn($n) => $n % 2 === 0);

// Generator usage
$fib = fibonacci_generator();
$first10Fib = [];
for ($i = 0; $i < 10; $i++) {
    $first10Fib[] = $fib->current();
    $fib->next();
}
echo "Fibonacci: " . implode(', ', $first10Fib) . "\n";

// Higher-order functions
$result = applyTwice(fn($x) => $x + 1, 5);
echo "Apply twice: $result\n";

// Named arguments (PHP 8.0+)
$greeted = greet(name: 'PHP', greeting: 'Welcome');
echo "$greeted\n";

// Enums
$color = Color::Red;
echo "Color: {$color->displayName()} = {$color->toHexString()}\n";

foreach (Color::cases() as $c) {
    echo "{$c->name}: {$c->value}\n";
}

// Reflection
$reflClass = new \ReflectionClass(Circle::class);
echo "Class: {$reflClass->getName()}\n";

foreach ($reflClass->getMethods(\ReflectionMethod::IS_PUBLIC) as $method) {
    echo "Method: {$method->getName()}\n";
}

// Attributes
$attributes = $reflClass->getAttributes();
foreach ($attributes as $attr) {
    echo "Attribute: {$attr->getName()}\n";
}

// File operations
file_put_contents('test.txt', "Hello, File!\n");
$content = file_get_contents('test.txt');
echo $content;

$lines = file('test.txt', FILE_IGNORE_NEW_LINES);
foreach ($lines as $line) {
    echo "Line: $line\n";
}

if (file_exists('test.txt')) {
    unlink('test.txt');
}

// JSON
$data = ['name' => 'Test', 'values' => [1, 2, 3]];
$jsonString = json_encode($data, JSON_PRETTY_PRINT);
echo $jsonString . "\n";

$decoded = json_decode($jsonString, true);
print_r($decoded);

// Regular expressions
$text = 'Hello, my email is user@example.com';
if (preg_match('/email is (\S+)/', $text, $matches)) {
    echo "Email: {$matches[1]}\n";
}

$replaced = preg_replace('/\d+/', 'X', 'hello123world');
echo "Replaced: $replaced\n";

// Date/Time
$now = new \DateTime();
echo "Now: " . $now->format('Y-m-d H:i:s') . "\n";

$date = new \DateTimeImmutable('2024-01-15');
echo "Date: " . $date->format('F j, Y') . "\n";

// Type checking
echo "Is integer: " . (is_int($integer) ? 'yes' : 'no') . "\n";
echo "Is object: " . (is_object($circle) ? 'yes' : 'no') . "\n";
echo "Instance of Shape: " . ($circle instanceof Shape ? 'yes' : 'no') . "\n";

echo GREETING . "\n";
echo "Program completed successfully!\n";
