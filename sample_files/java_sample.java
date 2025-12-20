/**
 * Comprehensive Java language sample demonstrating all syntax features
 *
 * @author Sample Author
 * @version 1.0.0
 * @since 2024-01-01
 */
package com.example.sample;

import java.io.*;
import java.nio.file.*;
import java.util.*;
import java.util.concurrent.*;
import java.util.function.*;
import java.util.stream.*;
import java.util.regex.*;
import java.time.*;
import java.time.format.*;
import java.lang.annotation.*;
import java.lang.reflect.*;
import java.math.*;
import java.net.*;
import java.net.http.*;
import java.text.*;
import java.security.*;

// ============================================================================
// Annotations
// ============================================================================

@Retention(RetentionPolicy.RUNTIME)
@Target({ElementType.TYPE, ElementType.METHOD, ElementType.FIELD})
@interface CustomAnnotation {
    String value() default "";
    int priority() default 0;
    String[] tags() default {};
}

@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
@interface Test {
    Class<? extends Throwable> expected() default None.class;

    class None extends Throwable {
        private None() {}
    }
}

// ============================================================================
// Enums
// ============================================================================

enum Color {
    RED(0xFF0000, "Red"),
    GREEN(0x00FF00, "Green"),
    BLUE(0x0000FF, "Blue"),
    WHITE(0xFFFFFF, "White"),
    BLACK(0x000000, "Black");

    private final int hexValue;
    private final String displayName;

    Color(int hexValue, String displayName) {
        this.hexValue = hexValue;
        this.displayName = displayName;
    }

    public int getHexValue() {
        return hexValue;
    }

    public String getDisplayName() {
        return displayName;
    }

    public String toHexString() {
        return String.format("#%06X", hexValue);
    }
}

enum Status {
    OK, ERROR, PENDING, TIMEOUT
}

// ============================================================================
// Interfaces
// ============================================================================

interface Shape {
    double area();
    double perimeter();

    default void draw() {
        System.out.println("Drawing shape");
    }

    static Shape createDefault() {
        return new Circle(1.0);
    }
}

interface Drawable {
    void render();
}

@FunctionalInterface
interface Calculator<T extends Number> {
    T calculate(T a, T b);
}

// Sealed interface (Java 17+)
sealed interface Vehicle permits Car, Motorcycle, Truck {
    String getType();
    int getWheels();
}

// ============================================================================
// Records (Java 16+)
// ============================================================================

record Point(double x, double y, double z) {
    // Compact constructor
    public Point {
        if (Double.isNaN(x) || Double.isNaN(y) || Double.isNaN(z)) {
            throw new IllegalArgumentException("Coordinates cannot be NaN");
        }
    }

    // Additional constructor
    public Point(double x, double y) {
        this(x, y, 0.0);
    }

    // Instance method
    public double distance(Point other) {
        double dx = this.x - other.x;
        double dy = this.y - other.y;
        double dz = this.z - other.z;
        return Math.sqrt(dx * dx + dy * dy + dz * dz);
    }

    // Static factory method
    public static Point origin() {
        return new Point(0, 0, 0);
    }
}

record Person(String name, int age, String email) implements Comparable<Person> {
    @Override
    public int compareTo(Person other) {
        return this.name.compareTo(other.name);
    }
}

// ============================================================================
// Abstract Classes
// ============================================================================

abstract class AbstractShape implements Shape, Drawable {
    protected String name;
    protected Color color;

    protected AbstractShape(String name) {
        this.name = name;
        this.color = Color.BLACK;
    }

    public String getName() {
        return name;
    }

    public Color getColor() {
        return color;
    }

    public void setColor(Color color) {
        this.color = color;
    }

    @Override
    public void render() {
        System.out.printf("Rendering %s in %s%n", name, color.getDisplayName());
    }

    public abstract String describe();
}

// ============================================================================
// Concrete Classes
// ============================================================================

class Circle extends AbstractShape {
    private double radius;

    public Circle(double radius) {
        super("Circle");
        this.radius = radius;
    }

    @Override
    public double area() {
        return Math.PI * radius * radius;
    }

    @Override
    public double perimeter() {
        return 2 * Math.PI * radius;
    }

    @Override
    public String describe() {
        return String.format("Circle with radius %.2f", radius);
    }

    public double getRadius() {
        return radius;
    }

    public void setRadius(double radius) {
        this.radius = radius;
    }
}

class Rectangle extends AbstractShape {
    private double width;
    private double height;

    public Rectangle(double width, double height) {
        super("Rectangle");
        this.width = width;
        this.height = height;
    }

    @Override
    public double area() {
        return width * height;
    }

    @Override
    public double perimeter() {
        return 2 * (width + height);
    }

    @Override
    public String describe() {
        return String.format("Rectangle %s x %s", width, height);
    }
}

// Sealed class implementations
final class Car implements Vehicle {
    private final String model;

    public Car(String model) {
        this.model = model;
    }

    @Override
    public String getType() {
        return "Car: " + model;
    }

    @Override
    public int getWheels() {
        return 4;
    }
}

final class Motorcycle implements Vehicle {
    @Override
    public String getType() {
        return "Motorcycle";
    }

    @Override
    public int getWheels() {
        return 2;
    }
}

non-sealed class Truck implements Vehicle {
    @Override
    public String getType() {
        return "Truck";
    }

    @Override
    public int getWheels() {
        return 6;
    }
}

// ============================================================================
// Generic Classes
// ============================================================================

class Container<T> implements Iterable<T> {
    private List<T> items;

    public Container() {
        this.items = new ArrayList<>();
    }

    public void add(T item) {
        items.add(item);
    }

    public T get(int index) {
        return items.get(index);
    }

    public int size() {
        return items.size();
    }

    @Override
    public Iterator<T> iterator() {
        return items.iterator();
    }
}

class Pair<K, V> {
    private final K key;
    private final V value;

    public Pair(K key, V value) {
        this.key = key;
        this.value = value;
    }

    public K getKey() {
        return key;
    }

    public V getValue() {
        return value;
    }

    public static <K, V> Pair<K, V> of(K key, V value) {
        return new Pair<>(key, value);
    }
}

class Result<T, E extends Exception> {
    private final T value;
    private final E error;

    private Result(T value, E error) {
        this.value = value;
        this.error = error;
    }

    public static <T, E extends Exception> Result<T, E> ok(T value) {
        return new Result<>(value, null);
    }

    public static <T, E extends Exception> Result<T, E> error(E error) {
        return new Result<>(null, error);
    }

    public boolean isOk() {
        return error == null;
    }

    public T getValue() {
        return value;
    }

    public E getError() {
        return error;
    }

    public <U> Result<U, E> map(Function<T, U> mapper) {
        if (isOk()) {
            return Result.ok(mapper.apply(value));
        }
        return Result.error(error);
    }
}

// ============================================================================
// Exceptions
// ============================================================================

class AppException extends Exception {
    private final int errorCode;

    public AppException(String message, int errorCode) {
        super(message);
        this.errorCode = errorCode;
    }

    public AppException(String message, int errorCode, Throwable cause) {
        super(message, cause);
        this.errorCode = errorCode;
    }

    public int getErrorCode() {
        return errorCode;
    }
}

class ValidationException extends AppException {
    public ValidationException(String message) {
        super(message, 400);
    }
}

// ============================================================================
// Inner Classes
// ============================================================================

@CustomAnnotation(value = "main", priority = 1, tags = {"sample", "demo"})
public class Sample {

    // Constants
    public static final int MAX_SIZE = 1024;
    public static final double PI = 3.14159265358979323846;
    private static final String GREETING = "Hello, Java!";

    // Instance fields
    private String name;
    private int value;
    private List<String> items;

    // Static fields
    private static int instanceCount = 0;
    private static final Object lock = new Object();

    // Static initializer
    static {
        System.out.println("Static initializer called");
    }

    // Instance initializer
    {
        this.items = new ArrayList<>();
        instanceCount++;
    }

    // Constructors
    public Sample() {
        this("Default", 0);
    }

    public Sample(String name, int value) {
        this.name = name;
        this.value = value;
    }

    // Static nested class
    public static class Builder {
        private String name;
        private int value;

        public Builder name(String name) {
            this.name = name;
            return this;
        }

        public Builder value(int value) {
            this.value = value;
            return this;
        }

        public Sample build() {
            return new Sample(name, value);
        }
    }

    // Inner class
    public class InnerHelper {
        public void help() {
            System.out.println("Helping " + name);
        }
    }

    // Local class example
    public void localClassExample() {
        class LocalClass {
            void doSomething() {
                System.out.println("Local class doing something");
            }
        }
        new LocalClass().doSomething();
    }

    // ============================================================================
    // Methods
    // ============================================================================

    // Getters and setters
    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public int getValue() {
        return value;
    }

    public void setValue(int value) {
        this.value = value;
    }

    // Generic method
    public <T extends Comparable<T>> T max(T a, T b) {
        return a.compareTo(b) > 0 ? a : b;
    }

    // Varargs
    public int sum(int... numbers) {
        int total = 0;
        for (int n : numbers) {
            total += n;
        }
        return total;
    }

    // Static method
    public static int add(int a, int b) {
        return a + b;
    }

    // Synchronized method
    public synchronized void synchronizedMethod() {
        // Thread-safe operation
    }

    // Method with throws
    public void riskyMethod() throws IOException, AppException {
        throw new IOException("Something went wrong");
    }

    // Final method
    public final void finalMethod() {
        System.out.println("Cannot be overridden");
    }

    // ============================================================================
    // Lambda and Functional Programming
    // ============================================================================

    public void functionalExamples() {
        // Lambda expressions
        Runnable runnable = () -> System.out.println("Running");

        Consumer<String> printer = s -> System.out.println(s);

        Function<Integer, Integer> square = x -> x * x;

        BiFunction<Integer, Integer, Integer> add = (a, b) -> a + b;

        Predicate<Integer> isEven = n -> n % 2 == 0;

        Supplier<String> supplier = () -> "Hello";

        // Method references
        Consumer<String> methodRef = System.out::println;
        Function<String, Integer> parseInt = Integer::parseInt;
        Supplier<List<String>> listSupplier = ArrayList::new;

        // Streams
        List<Integer> numbers = List.of(1, 2, 3, 4, 5, 6, 7, 8, 9, 10);

        List<Integer> evens = numbers.stream()
            .filter(n -> n % 2 == 0)
            .collect(Collectors.toList());

        List<Integer> doubled = numbers.stream()
            .map(n -> n * 2)
            .toList();

        int sumVal = numbers.stream()
            .reduce(0, Integer::sum);

        Optional<Integer> max = numbers.stream()
            .max(Integer::compareTo);

        Map<Boolean, List<Integer>> partitioned = numbers.stream()
            .collect(Collectors.partitioningBy(n -> n % 2 == 0));

        String joined = numbers.stream()
            .map(String::valueOf)
            .collect(Collectors.joining(", "));

        // Parallel streams
        long count = numbers.parallelStream()
            .filter(n -> n > 5)
            .count();

        // IntStream, LongStream, DoubleStream
        int[] array = IntStream.range(0, 10).toArray();
        double average = IntStream.of(1, 2, 3, 4, 5).average().orElse(0);

        // FlatMap
        List<List<Integer>> nested = List.of(
            List.of(1, 2, 3),
            List.of(4, 5, 6),
            List.of(7, 8, 9)
        );

        List<Integer> flat = nested.stream()
            .flatMap(Collection::stream)
            .toList();

        // Collectors
        Map<String, Long> frequency = List.of("a", "b", "a", "c", "b", "a")
            .stream()
            .collect(Collectors.groupingBy(
                Function.identity(),
                Collectors.counting()
            ));
    }

    // ============================================================================
    // Optional
    // ============================================================================

    public void optionalExamples() {
        Optional<String> optional = Optional.of("Hello");
        Optional<String> empty = Optional.empty();
        Optional<String> nullable = Optional.ofNullable(null);

        // Check and get
        if (optional.isPresent()) {
            System.out.println(optional.get());
        }

        optional.ifPresent(System.out::println);

        optional.ifPresentOrElse(
            System.out::println,
            () -> System.out.println("Empty")
        );

        // Default values
        String value = empty.orElse("Default");
        String lazyValue = empty.orElseGet(() -> "Lazy Default");

        // Transform
        Optional<Integer> length = optional.map(String::length);
        Optional<String> upper = optional.map(String::toUpperCase);

        // FlatMap
        Optional<String> flatMapped = optional.flatMap(s -> Optional.of(s + "!"));

        // Filter
        Optional<String> filtered = optional.filter(s -> s.length() > 3);

        // Or
        Optional<String> orValue = empty.or(() -> Optional.of("Alternative"));

        // Stream
        List<String> list = optional.stream().toList();
    }

    // ============================================================================
    // Exception Handling
    // ============================================================================

    public void exceptionExamples() {
        // Try-catch-finally
        try {
            riskyMethod();
        } catch (IOException e) {
            System.err.println("IO Error: " + e.getMessage());
        } catch (AppException e) {
            System.err.println("App Error: " + e.getErrorCode());
        } finally {
            System.out.println("Cleanup");
        }

        // Multi-catch
        try {
            riskyMethod();
        } catch (IOException | AppException e) {
            System.err.println("Error: " + e.getMessage());
        }

        // Try-with-resources
        try (var reader = new BufferedReader(new FileReader("file.txt"));
             var writer = new BufferedWriter(new FileWriter("output.txt"))) {
            String line;
            while ((line = reader.readLine()) != null) {
                writer.write(line);
            }
        } catch (IOException e) {
            e.printStackTrace();
        }

        // Nested try-catch
        try {
            try {
                throw new RuntimeException("Inner");
            } catch (RuntimeException e) {
                throw new RuntimeException("Wrapped", e);
            }
        } catch (RuntimeException e) {
            e.printStackTrace();
        }
    }

    // ============================================================================
    // Concurrency
    // ============================================================================

    public void concurrencyExamples() throws InterruptedException, ExecutionException {
        // Thread creation
        Thread thread = new Thread(() -> {
            System.out.println("Running in thread: " + Thread.currentThread().getName());
        });
        thread.start();
        thread.join();

        // ExecutorService
        ExecutorService executor = Executors.newFixedThreadPool(4);

        Future<Integer> future = executor.submit(() -> {
            Thread.sleep(100);
            return 42;
        });

        Integer result = future.get();

        // CompletableFuture
        CompletableFuture<String> cf = CompletableFuture
            .supplyAsync(() -> "Hello")
            .thenApply(s -> s + " World")
            .thenApply(String::toUpperCase);

        CompletableFuture<Void> allOf = CompletableFuture.allOf(
            CompletableFuture.runAsync(() -> {}),
            CompletableFuture.runAsync(() -> {})
        );

        CompletableFuture<Object> anyOf = CompletableFuture.anyOf(
            CompletableFuture.supplyAsync(() -> "First"),
            CompletableFuture.supplyAsync(() -> "Second")
        );

        // Synchronized block
        synchronized (lock) {
            // Critical section
        }

        // Concurrent collections
        ConcurrentHashMap<String, Integer> concurrentMap = new ConcurrentHashMap<>();
        CopyOnWriteArrayList<String> cowList = new CopyOnWriteArrayList<>();
        BlockingQueue<String> blockingQueue = new LinkedBlockingQueue<>();

        // Atomic variables
        var atomicInt = new java.util.concurrent.atomic.AtomicInteger(0);
        atomicInt.incrementAndGet();
        atomicInt.compareAndSet(1, 2);

        // CountDownLatch
        CountDownLatch latch = new CountDownLatch(3);
        for (int i = 0; i < 3; i++) {
            executor.submit(() -> {
                try {
                    Thread.sleep(100);
                } catch (InterruptedException e) {
                    Thread.currentThread().interrupt();
                } finally {
                    latch.countDown();
                }
            });
        }
        latch.await();

        // Semaphore
        Semaphore semaphore = new Semaphore(3);
        semaphore.acquire();
        try {
            // Limited access
        } finally {
            semaphore.release();
        }

        executor.shutdown();
        executor.awaitTermination(1, TimeUnit.SECONDS);
    }

    // ============================================================================
    // Reflection
    // ============================================================================

    public void reflectionExamples() throws Exception {
        Class<?> clazz = Sample.class;

        // Class information
        System.out.println("Name: " + clazz.getName());
        System.out.println("Simple name: " + clazz.getSimpleName());
        System.out.println("Package: " + clazz.getPackageName());
        System.out.println("Superclass: " + clazz.getSuperclass());

        // Fields
        for (Field field : clazz.getDeclaredFields()) {
            System.out.printf("Field: %s %s%n",
                field.getType().getSimpleName(), field.getName());
        }

        // Methods
        for (Method method : clazz.getDeclaredMethods()) {
            System.out.printf("Method: %s %s(%s)%n",
                method.getReturnType().getSimpleName(),
                method.getName(),
                Arrays.toString(method.getParameterTypes()));
        }

        // Annotations
        CustomAnnotation annotation = clazz.getAnnotation(CustomAnnotation.class);
        if (annotation != null) {
            System.out.println("Annotation value: " + annotation.value());
        }

        // Create instance
        Object instance = clazz.getDeclaredConstructor().newInstance();

        // Invoke method
        Method method = clazz.getMethod("getValue");
        Object returnValue = method.invoke(instance);

        // Access private field
        Field privateField = clazz.getDeclaredField("name");
        privateField.setAccessible(true);
        privateField.set(instance, "New Name");
    }

    // ============================================================================
    // Date/Time API
    // ============================================================================

    public void dateTimeExamples() {
        // Current date/time
        LocalDate today = LocalDate.now();
        LocalTime now = LocalTime.now();
        LocalDateTime dateTime = LocalDateTime.now();
        ZonedDateTime zonedDateTime = ZonedDateTime.now();
        Instant instant = Instant.now();

        // Create specific date/time
        LocalDate date = LocalDate.of(2024, Month.JANUARY, 15);
        LocalTime time = LocalTime.of(14, 30, 0);
        LocalDateTime ldt = LocalDateTime.of(date, time);

        // Parsing
        LocalDate parsed = LocalDate.parse("2024-01-15");
        DateTimeFormatter formatter = DateTimeFormatter.ofPattern("dd/MM/yyyy");
        LocalDate customParsed = LocalDate.parse("15/01/2024", formatter);

        // Formatting
        String formatted = dateTime.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME);
        String customFormatted = dateTime.format(
            DateTimeFormatter.ofPattern("yyyy-MM-dd HH:mm:ss")
        );

        // Manipulation
        LocalDate tomorrow = today.plusDays(1);
        LocalDate nextMonth = today.plusMonths(1);
        LocalDate lastYear = today.minusYears(1);

        // Duration and Period
        Duration duration = Duration.ofHours(2).plusMinutes(30);
        Period period = Period.ofMonths(3);

        Duration between = Duration.between(
            LocalTime.of(10, 0),
            LocalTime.of(14, 30)
        );

        // Comparison
        boolean isBefore = date.isBefore(today);
        boolean isAfter = date.isAfter(today);

        // Timezone
        ZoneId zone = ZoneId.of("America/New_York");
        ZonedDateTime nyTime = ZonedDateTime.now(zone);
        ZonedDateTime utcTime = nyTime.withZoneSameInstant(ZoneId.of("UTC"));
    }

    // ============================================================================
    // Pattern Matching (Java 21+)
    // ============================================================================

    public void patternMatchingExamples(Object obj) {
        // instanceof with pattern
        if (obj instanceof String s) {
            System.out.println("String: " + s.toUpperCase());
        }

        // Pattern matching in switch (Java 21+)
        String result = switch (obj) {
            case Integer i -> "Integer: " + i;
            case Long l -> "Long: " + l;
            case Double d -> "Double: " + d;
            case String s -> "String: " + s;
            case null -> "Null";
            default -> "Unknown: " + obj.getClass();
        };

        // Record patterns (Java 21+)
        Object point = new Point(1, 2, 3);
        if (point instanceof Point(double x, double y, double z)) {
            System.out.printf("Point: (%.1f, %.1f, %.1f)%n", x, y, z);
        }

        // Switch with guards
        String description = switch (obj) {
            case Integer i when i > 0 -> "Positive integer";
            case Integer i when i < 0 -> "Negative integer";
            case Integer i -> "Zero";
            case String s when s.isEmpty() -> "Empty string";
            case String s -> "Non-empty string: " + s;
            default -> "Other";
        };
    }

    // ============================================================================
    // Text Blocks (Java 15+)
    // ============================================================================

    public void textBlockExamples() {
        String json = """
            {
                "name": "John",
                "age": 30,
                "email": "john@example.com"
            }
            """;

        String html = """
            <html>
                <body>
                    <h1>Hello, World!</h1>
                </body>
            </html>
            """;

        String sql = """
            SELECT u.name, u.email
            FROM users u
            WHERE u.active = true
            ORDER BY u.name
            """;

        String formatted = """
            Name: %s
            Age: %d
            """.formatted("John", 30);
    }

    // ============================================================================
    // Main Method
    // ============================================================================

    public static void main(String[] args) {
        // Primitive types
        byte b = 127;
        short s = 32767;
        int i = 2147483647;
        long l = 9223372036854775807L;
        float f = 3.14f;
        double d = 3.14159265358979;
        char c = 'A';
        boolean bool = true;

        // Wrapper types
        Integer boxedInt = Integer.valueOf(42);
        Double boxedDouble = Double.valueOf(3.14);
        Boolean boxedBool = Boolean.TRUE;

        // Autoboxing/unboxing
        Integer autoBoxed = 42;
        int autoUnboxed = autoBoxed;

        // String
        String str = "Hello, World!";
        String concat = "Hello" + ", " + "World!";
        String format = String.format("Value: %d", 42);

        // Numeric literals
        int decimal = 42;
        int hex = 0xDEADBEEF;
        int octal = 0755;
        int binary = 0b10101010;
        long longLiteral = 123_456_789L;
        double scientific = 1.23e-4;

        // Arrays
        int[] intArray = {1, 2, 3, 4, 5};
        int[] intArray2 = new int[10];
        int[][] matrix = {{1, 2, 3}, {4, 5, 6}, {7, 8, 9}};
        String[] stringArray = new String[]{"a", "b", "c"};

        // Collections
        List<Integer> list = new ArrayList<>();
        list.add(1);
        list.addAll(List.of(2, 3, 4));

        Set<String> set = new HashSet<>();
        set.add("one");

        Map<String, Integer> map = new HashMap<>();
        map.put("one", 1);
        map.put("two", 2);

        // Immutable collections
        List<Integer> immutableList = List.of(1, 2, 3);
        Set<String> immutableSet = Set.of("a", "b", "c");
        Map<String, Integer> immutableMap = Map.of("one", 1, "two", 2);

        // Control flow
        if (i > 0) {
            System.out.println("Positive");
        } else if (i < 0) {
            System.out.println("Negative");
        } else {
            System.out.println("Zero");
        }

        // Ternary operator
        String result = i > 0 ? "Positive" : "Non-positive";

        // Switch expression (Java 14+)
        String dayType = switch (LocalDate.now().getDayOfWeek()) {
            case SATURDAY, SUNDAY -> "Weekend";
            case MONDAY -> "Start of week";
            case FRIDAY -> "End of week";
            default -> "Weekday";
        };

        // Traditional switch
        switch (i) {
            case 0:
                System.out.println("Zero");
                break;
            case 1:
            case 2:
            case 3:
                System.out.println("1-3");
                break;
            default:
                System.out.println("Other");
        }

        // Loops
        for (int j = 0; j < 10; j++) {
            if (j == 5) continue;
            if (j == 8) break;
            System.out.println(j);
        }

        for (int num : intArray) {
            System.out.println(num);
        }

        int counter = 0;
        while (counter < 5) {
            System.out.println(counter);
            counter++;
        }

        do {
            counter--;
        } while (counter > 0);

        // Labeled loop
        outer:
        for (int x = 0; x < 10; x++) {
            for (int y = 0; y < 10; y++) {
                if (x * y > 50) {
                    break outer;
                }
            }
        }

        // Object creation
        Sample sample = new Sample("Test", 42);
        Sample built = new Sample.Builder()
            .name("Built")
            .value(100)
            .build();

        Circle circle = new Circle(5.0);
        circle.setColor(Color.BLUE);
        System.out.println(circle.describe());
        System.out.printf("Area: %.2f%n", circle.area());

        // Record usage
        Point point = new Point(1, 2, 3);
        System.out.println("Point: " + point);
        System.out.println("Distance: " + point.distance(Point.origin()));

        Person person = new Person("Alice", 30, "alice@example.com");
        System.out.println("Person: " + person);

        // Enum usage
        Color color = Color.RED;
        System.out.println(color.getDisplayName());
        System.out.println(color.toHexString());

        for (Color c2 : Color.values()) {
            System.out.println(c2 + ": " + c2.getHexValue());
        }

        // Generic container
        Container<String> container = new Container<>();
        container.add("Hello");
        container.add("World");

        for (String item : container) {
            System.out.println(item);
        }

        // Pair
        Pair<String, Integer> pair = Pair.of("Answer", 42);
        System.out.println(pair.getKey() + ": " + pair.getValue());

        // var (local variable type inference)
        var inferredList = new ArrayList<String>();
        var inferredMap = Map.of("key", "value");

        // Assert
        assert i > 0 : "i must be positive";

        // Instance methods
        sample.functionalExamples();
        sample.optionalExamples();
        sample.exceptionExamples();
        sample.dateTimeExamples();

        try {
            sample.concurrencyExamples();
            sample.reflectionExamples();
        } catch (Exception e) {
            e.printStackTrace();
        }

        System.out.println("Program completed successfully!");
    }

    @Override
    public String toString() {
        return "Sample{name='" + name + "', value=" + value + "}";
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Sample sample = (Sample) o;
        return value == sample.value && Objects.equals(name, sample.name);
    }

    @Override
    public int hashCode() {
        return Objects.hash(name, value);
    }
}
