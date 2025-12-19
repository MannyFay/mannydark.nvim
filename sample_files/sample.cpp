/**
 * @file sample.cpp
 * @brief Comprehensive C++ language sample demonstrating modern C++ features
 * @author Sample Author
 * @version 1.0.0
 */

#include <iostream>
#include <vector>
#include <map>
#include <unordered_map>
#include <set>
#include <string>
#include <string_view>
#include <memory>
#include <algorithm>
#include <functional>
#include <optional>
#include <variant>
#include <any>
#include <tuple>
#include <array>
#include <span>
#include <ranges>
#include <concepts>
#include <coroutine>
#include <thread>
#include <mutex>
#include <future>
#include <atomic>
#include <chrono>
#include <filesystem>
#include <format>
#include <type_traits>
#include <utility>
#include <numeric>
#include <regex>
#include <fstream>
#include <sstream>

// Namespace declarations
namespace myproject {
namespace utils {
inline namespace v1 {

// Constants and type aliases
constexpr double PI = 3.14159265358979323846;
constexpr auto MAX_SIZE = 1024uz;

using StringList = std::vector<std::string>;
using StringMap = std::unordered_map<std::string, std::string>;

// Template alias
template<typename T>
using Vec = std::vector<T>;

template<typename K, typename V>
using Map = std::map<K, V>;

} // namespace v1
} // namespace utils

// Concepts (C++20)
template<typename T>
concept Numeric = std::is_arithmetic_v<T>;

template<typename T>
concept Printable = requires(T t) {
    { std::cout << t } -> std::same_as<std::ostream&>;
};

template<typename T>
concept Container = requires(T c) {
    { c.begin() } -> std::input_or_output_iterator;
    { c.end() } -> std::input_or_output_iterator;
    { c.size() } -> std::convertible_to<std::size_t>;
};

// Enum class (scoped enumeration)
enum class Color : uint32_t {
    Red   = 0xFF0000,
    Green = 0x00FF00,
    Blue  = 0x0000FF,
    White = 0xFFFFFF,
    Black = 0x000000
};

// Unscoped enum with underlying type
enum Status : int {
    OK = 0,
    Error = -1,
    Pending = 1
};

// Forward declarations
class Shape;
class Circle;
class Rectangle;

// Abstract base class
class Shape {
public:
    // Virtual destructor
    virtual ~Shape() = default;

    // Pure virtual functions
    virtual double area() const = 0;
    virtual double perimeter() const = 0;
    virtual std::string name() const = 0;

    // Virtual function with default implementation
    virtual void draw() const {
        std::cout << "Drawing " << name() << std::endl;
    }

    // Non-virtual interface pattern
    void display() const {
        std::cout << name() << ": Area = " << area()
                  << ", Perimeter = " << perimeter() << std::endl;
    }

protected:
    // Protected constructor for abstract class
    Shape() = default;
    Shape(const Shape&) = default;
    Shape& operator=(const Shape&) = default;
};

// Derived class with override
class Circle final : public Shape {
private:
    double radius_;

public:
    // Explicit constructor
    explicit Circle(double radius) : radius_(radius) {}

    // Override specifier
    [[nodiscard]] double area() const override {
        return utils::PI * radius_ * radius_;
    }

    [[nodiscard]] double perimeter() const override {
        return 2 * utils::PI * radius_;
    }

    [[nodiscard]] std::string name() const override {
        return "Circle";
    }

    // Getter with nodiscard
    [[nodiscard]] double radius() const noexcept { return radius_; }

    // Setter
    void setRadius(double r) noexcept { radius_ = r; }
};

// Another derived class
class Rectangle : public Shape {
protected:
    double width_;
    double height_;

public:
    Rectangle(double w, double h) : width_(w), height_(h) {}

    double area() const override { return width_ * height_; }
    double perimeter() const override { return 2 * (width_ + height_); }
    std::string name() const override { return "Rectangle"; }

    // Additional member functions
    double width() const { return width_; }
    double height() const { return height_; }
};

// Multiple inheritance
class Square : public Rectangle {
public:
    explicit Square(double side) : Rectangle(side, side) {}

    std::string name() const override { return "Square"; }
};

// Template class with type constraints
template<Numeric T>
class Point {
private:
    T x_, y_, z_;

public:
    // Constexpr constructor
    constexpr Point(T x = T{}, T y = T{}, T z = T{})
        : x_(x), y_(y), z_(z) {}

    // Getters
    [[nodiscard]] constexpr T x() const noexcept { return x_; }
    [[nodiscard]] constexpr T y() const noexcept { return y_; }
    [[nodiscard]] constexpr T z() const noexcept { return z_; }

    // Operator overloading
    constexpr Point operator+(const Point& other) const {
        return Point(x_ + other.x_, y_ + other.y_, z_ + other.z_);
    }

    constexpr Point operator-(const Point& other) const {
        return Point(x_ - other.x_, y_ - other.y_, z_ - other.z_);
    }

    template<Numeric U>
    constexpr Point operator*(U scalar) const {
        return Point(x_ * scalar, y_ * scalar, z_ * scalar);
    }

    // Spaceship operator (C++20)
    auto operator<=>(const Point&) const = default;

    // Friend function for stream output
    friend std::ostream& operator<<(std::ostream& os, const Point& p) {
        return os << "(" << p.x_ << ", " << p.y_ << ", " << p.z_ << ")";
    }

    // Distance calculation
    [[nodiscard]] constexpr auto distance(const Point& other) const {
        auto dx = x_ - other.x_;
        auto dy = y_ - other.y_;
        auto dz = z_ - other.z_;
        return std::sqrt(dx*dx + dy*dy + dz*dz);
    }
};

// Template specialization
template<>
class Point<int> {
private:
    int x_, y_, z_;

public:
    constexpr Point(int x = 0, int y = 0, int z = 0)
        : x_(x), y_(y), z_(z) {}

    constexpr int x() const noexcept { return x_; }
    constexpr int y() const noexcept { return y_; }
    constexpr int z() const noexcept { return z_; }

    // Integer-specific method
    constexpr int manhattanDistance(const Point& other) const {
        return std::abs(x_ - other.x_) +
               std::abs(y_ - other.y_) +
               std::abs(z_ - other.z_);
    }
};

// Variadic template
template<typename... Args>
void print(Args&&... args) {
    (std::cout << ... << std::forward<Args>(args)) << std::endl;
}

// Fold expressions
template<typename... Args>
auto sum(Args... args) {
    return (args + ...);
}

template<typename... Args>
auto product(Args... args) {
    return (args * ...);
}

// SFINAE with enable_if
template<typename T>
typename std::enable_if<std::is_integral_v<T>, T>::type
divide_by_two(T value) {
    return value >> 1;
}

template<typename T>
typename std::enable_if<std::is_floating_point_v<T>, T>::type
divide_by_two(T value) {
    return value / 2.0;
}

// Requires clause (C++20)
template<typename T>
requires std::is_arithmetic_v<T>
T square(T value) {
    return value * value;
}

// CRTP (Curiously Recurring Template Pattern)
template<typename Derived>
class Singleton {
protected:
    Singleton() = default;
    ~Singleton() = default;

public:
    Singleton(const Singleton&) = delete;
    Singleton& operator=(const Singleton&) = delete;

    static Derived& instance() {
        static Derived instance;
        return instance;
    }
};

// Class using CRTP
class Logger : public Singleton<Logger> {
    friend class Singleton<Logger>;

private:
    Logger() = default;
    std::mutex mutex_;

public:
    void log(std::string_view message) {
        std::lock_guard lock(mutex_);
        std::cout << "[LOG] " << message << std::endl;
    }
};

// Smart pointer usage
class Resource {
private:
    std::string name_;
    std::vector<int> data_;

public:
    explicit Resource(std::string name) : name_(std::move(name)) {
        std::cout << "Resource " << name_ << " created\n";
    }

    ~Resource() {
        std::cout << "Resource " << name_ << " destroyed\n";
    }

    // Rule of five
    Resource(const Resource& other)
        : name_(other.name_ + "_copy"), data_(other.data_) {}

    Resource(Resource&& other) noexcept
        : name_(std::move(other.name_)), data_(std::move(other.data_)) {}

    Resource& operator=(const Resource& other) {
        if (this != &other) {
            name_ = other.name_ + "_copy";
            data_ = other.data_;
        }
        return *this;
    }

    Resource& operator=(Resource&& other) noexcept {
        if (this != &other) {
            name_ = std::move(other.name_);
            data_ = std::move(other.data_);
        }
        return *this;
    }

    void addData(int value) { data_.push_back(value); }
    const std::string& name() const { return name_; }
};

// Factory function with make_unique
template<typename... Args>
std::unique_ptr<Resource> createResource(Args&&... args) {
    return std::make_unique<Resource>(std::forward<Args>(args)...);
}

// Structured bindings with custom type
struct Employee {
    std::string name;
    int id;
    double salary;

    // For structured bindings
    template<std::size_t I>
    auto& get() & {
        if constexpr (I == 0) return name;
        else if constexpr (I == 1) return id;
        else return salary;
    }
};

} // namespace myproject

// Specializations for structured bindings
namespace std {
    template<>
    struct tuple_size<myproject::Employee> : std::integral_constant<size_t, 3> {};

    template<size_t I>
    struct tuple_element<I, myproject::Employee> {
        using type = std::conditional_t<I == 0, std::string,
                     std::conditional_t<I == 1, int, double>>;
    };
}

// Lambda expressions and functional programming
namespace functional_demo {

auto make_adder(int n) {
    return [n](int x) { return x + n; };
}

auto make_multiplier(int n) {
    return [n](int x) mutable {
        return x * n++;
    };
}

// Generic lambda (C++14)
auto generic_print = [](const auto& value) {
    std::cout << value << std::endl;
};

// Lambda with template parameter (C++20)
auto templated_lambda = []<typename T>(const std::vector<T>& vec) {
    for (const auto& elem : vec) {
        std::cout << elem << " ";
    }
    std::cout << std::endl;
};

// Immediately invoked lambda expression (IIFE)
const auto config = []() {
    std::map<std::string, std::string> cfg;
    cfg["host"] = "localhost";
    cfg["port"] = "8080";
    return cfg;
}();

} // namespace functional_demo

// Coroutines (C++20)
struct Generator {
    struct promise_type {
        int current_value;

        Generator get_return_object() {
            return Generator{std::coroutine_handle<promise_type>::from_promise(*this)};
        }

        std::suspend_always initial_suspend() noexcept { return {}; }
        std::suspend_always final_suspend() noexcept { return {}; }
        std::suspend_always yield_value(int value) noexcept {
            current_value = value;
            return {};
        }
        void return_void() noexcept {}
        void unhandled_exception() { std::terminate(); }
    };

    std::coroutine_handle<promise_type> handle;

    explicit Generator(std::coroutine_handle<promise_type> h) : handle(h) {}
    ~Generator() { if (handle) handle.destroy(); }

    // Move only
    Generator(Generator&& other) noexcept : handle(other.handle) {
        other.handle = nullptr;
    }
    Generator& operator=(Generator&& other) noexcept {
        if (this != &other) {
            if (handle) handle.destroy();
            handle = other.handle;
            other.handle = nullptr;
        }
        return *this;
    }

    int next() {
        handle.resume();
        return handle.promise().current_value;
    }

    bool done() const { return handle.done(); }
};

Generator fibonacci(int n) {
    int a = 0, b = 1;
    for (int i = 0; i < n; ++i) {
        co_yield a;
        auto temp = a;
        a = b;
        b = temp + b;
    }
}

// Ranges (C++20)
namespace ranges_demo {

void demonstrate_ranges() {
    std::vector<int> numbers = {1, 2, 3, 4, 5, 6, 7, 8, 9, 10};

    // Pipe syntax with views
    auto even_squares = numbers
        | std::views::filter([](int n) { return n % 2 == 0; })
        | std::views::transform([](int n) { return n * n; });

    for (int val : even_squares) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    // Take and drop
    auto first_five = numbers | std::views::take(5);
    auto skip_three = numbers | std::views::drop(3);

    // Reverse
    auto reversed = numbers | std::views::reverse;

    // Enumerate (C++23)
    // for (auto [index, value] : std::views::enumerate(numbers)) {
    //     std::cout << index << ": " << value << std::endl;
    // }

    // iota view
    for (int i : std::views::iota(1, 10)) {
        std::cout << i << " ";
    }
    std::cout << std::endl;
}

} // namespace ranges_demo

// Multithreading
namespace threading_demo {

std::mutex cout_mutex;

void thread_function(int id) {
    std::lock_guard lock(cout_mutex);
    std::cout << "Thread " << id << " running" << std::endl;
}

void async_demo() {
    // std::async with launch policy
    auto future1 = std::async(std::launch::async, []() {
        std::this_thread::sleep_for(std::chrono::milliseconds(100));
        return 42;
    });

    auto future2 = std::async(std::launch::deferred, []() {
        return std::string("Hello from deferred");
    });

    std::cout << "Async result: " << future1.get() << std::endl;
    std::cout << "Deferred result: " << future2.get() << std::endl;
}

void promise_demo() {
    std::promise<int> promise;
    std::future<int> future = promise.get_future();

    std::thread t([&promise]() {
        std::this_thread::sleep_for(std::chrono::milliseconds(50));
        promise.set_value(100);
    });

    std::cout << "Waiting for promise..." << std::endl;
    std::cout << "Got: " << future.get() << std::endl;

    t.join();
}

// Atomic operations
void atomic_demo() {
    std::atomic<int> counter{0};

    auto increment = [&counter]() {
        for (int i = 0; i < 1000; ++i) {
            counter.fetch_add(1, std::memory_order_relaxed);
        }
    };

    std::vector<std::thread> threads;
    for (int i = 0; i < 10; ++i) {
        threads.emplace_back(increment);
    }

    for (auto& t : threads) {
        t.join();
    }

    std::cout << "Final counter: " << counter.load() << std::endl;
}

} // namespace threading_demo

// Compile-time programming
namespace constexpr_demo {

constexpr int factorial(int n) {
    return n <= 1 ? 1 : n * factorial(n - 1);
}

consteval int compile_time_only(int n) {
    return n * n;
}

constinit int global_init = 42;

template<int N>
struct Fibonacci {
    static constexpr int value = Fibonacci<N-1>::value + Fibonacci<N-2>::value;
};

template<>
struct Fibonacci<0> {
    static constexpr int value = 0;
};

template<>
struct Fibonacci<1> {
    static constexpr int value = 1;
};

// Compile-time string
template<std::size_t N>
struct FixedString {
    char data[N]{};

    constexpr FixedString(const char (&str)[N]) {
        std::copy_n(str, N, data);
    }
};

template<FixedString Str>
void print_fixed() {
    std::cout << Str.data << std::endl;
}

} // namespace constexpr_demo

// Exception handling
namespace exception_demo {

class CustomException : public std::exception {
private:
    std::string message_;

public:
    explicit CustomException(std::string message)
        : message_(std::move(message)) {}

    [[nodiscard]] const char* what() const noexcept override {
        return message_.c_str();
    }
};

void might_throw(bool do_throw) {
    if (do_throw) {
        throw CustomException("Something went wrong!");
    }
}

void exception_handling_demo() {
    try {
        might_throw(true);
    } catch (const CustomException& e) {
        std::cerr << "Caught custom exception: " << e.what() << std::endl;
    } catch (const std::exception& e) {
        std::cerr << "Caught std exception: " << e.what() << std::endl;
    } catch (...) {
        std::cerr << "Caught unknown exception" << std::endl;
    }

    // noexcept specification
    auto safe_func = []() noexcept {
        return 42;
    };

    static_assert(noexcept(safe_func()));
}

} // namespace exception_demo

// Main function
int main(int argc, char* argv[]) {
    using namespace myproject;
    using namespace std::literals;

    // String literals
    auto str1 = "Hello"s;           // std::string
    auto str2 = "World"sv;          // std::string_view
    auto raw = R"(Raw string with "quotes" and \backslashes\)";
    auto utf8 = u8"UTF-8 string";
    auto utf16 = u"UTF-16 string";
    auto utf32 = U"UTF-32 string";

    // Numeric literals
    auto binary = 0b1010'1010;      // Binary with digit separator
    auto hex = 0xFF'FF'FF'FF;       // Hex with separator
    auto big = 1'000'000'000;       // Large number with separators
    auto sci = 1.23e-4;             // Scientific notation

    // User-defined literal
    // auto duration = 100ms;

    // auto and decltype
    auto vec = std::vector{1, 2, 3, 4, 5};  // CTAD
    decltype(vec) another_vec;
    decltype(auto) ref = vec[0];

    // Structured bindings
    auto [a, b, c] = std::tuple{1, "hello", 3.14};
    std::cout << a << ", " << b << ", " << c << std::endl;

    for (const auto& [key, value] : functional_demo::config) {
        std::cout << key << " = " << value << std::endl;
    }

    // std::optional
    std::optional<int> opt_val;
    if (!opt_val.has_value()) {
        opt_val = 42;
    }
    std::cout << "Optional value: " << opt_val.value_or(-1) << std::endl;

    // std::variant
    std::variant<int, double, std::string> var = "Hello";
    std::visit([](auto&& arg) {
        using T = std::decay_t<decltype(arg)>;
        if constexpr (std::is_same_v<T, int>) {
            std::cout << "int: " << arg << std::endl;
        } else if constexpr (std::is_same_v<T, double>) {
            std::cout << "double: " << arg << std::endl;
        } else {
            std::cout << "string: " << arg << std::endl;
        }
    }, var);

    // std::any
    std::any anything = 42;
    anything = std::string("Now I'm a string");
    if (anything.type() == typeid(std::string)) {
        std::cout << "Any contains: " << std::any_cast<std::string>(anything) << std::endl;
    }

    // Polymorphism with smart pointers
    std::vector<std::unique_ptr<Shape>> shapes;
    shapes.push_back(std::make_unique<Circle>(5.0));
    shapes.push_back(std::make_unique<Rectangle>(4.0, 6.0));
    shapes.push_back(std::make_unique<Square>(3.0));

    for (const auto& shape : shapes) {
        shape->display();
    }

    // Point template
    Point<double> p1{1.0, 2.0, 3.0};
    Point<double> p2{4.0, 5.0, 6.0};
    auto p3 = p1 + p2;
    std::cout << "p1 + p2 = " << p3 << std::endl;

    // Variadic templates
    print("Hello", " ", "World", "!", " ", 42);
    std::cout << "Sum: " << sum(1, 2, 3, 4, 5) << std::endl;
    std::cout << "Product: " << product(1, 2, 3, 4, 5) << std::endl;

    // Lambda expressions
    auto add_ten = functional_demo::make_adder(10);
    std::cout << "add_ten(5) = " << add_ten(5) << std::endl;

    // Algorithm with lambda
    std::vector<int> numbers = {5, 2, 8, 1, 9, 3, 7, 4, 6};

    std::sort(numbers.begin(), numbers.end(), std::greater<>{});

    auto it = std::find_if(numbers.begin(), numbers.end(),
                           [](int n) { return n % 2 == 0; });

    std::transform(numbers.begin(), numbers.end(), numbers.begin(),
                   [](int n) { return n * 2; });

    auto sum_val = std::accumulate(numbers.begin(), numbers.end(), 0);

    // Ranges demo
    ranges_demo::demonstrate_ranges();

    // Coroutine demo
    auto fib = fibonacci(10);
    while (!fib.done()) {
        std::cout << fib.next() << " ";
    }
    std::cout << std::endl;

    // Compile-time computation
    constexpr auto fact5 = constexpr_demo::factorial(5);
    static_assert(fact5 == 120);

    constexpr auto fib10 = constexpr_demo::Fibonacci<10>::value;
    static_assert(fib10 == 55);

    // Threading demo
    threading_demo::async_demo();
    threading_demo::atomic_demo();

    // Exception handling
    exception_demo::exception_handling_demo();

    // Filesystem operations
    namespace fs = std::filesystem;
    auto current_path = fs::current_path();
    std::cout << "Current directory: " << current_path << std::endl;

    // Format (C++20)
    // std::cout << std::format("Formatted: {} + {} = {}\n", 1, 2, 3);

    // Chrono
    auto now = std::chrono::system_clock::now();
    auto duration = std::chrono::hours(24) + std::chrono::minutes(30);

    // Type traits
    static_assert(std::is_same_v<int, int>);
    static_assert(std::is_arithmetic_v<double>);
    static_assert(std::is_class_v<Circle>);
    static_assert(std::is_base_of_v<Shape, Circle>);

    // Attributes
    [[maybe_unused]] int unused_var = 42;

    switch (Status::OK) {
        case Status::OK:
            std::cout << "Status OK" << std::endl;
            [[fallthrough]];
        case Status::Pending:
            std::cout << "Processing..." << std::endl;
            break;
        [[unlikely]] case Status::Error:
            std::cerr << "Error occurred!" << std::endl;
            break;
    }

    // if constexpr
    if constexpr (sizeof(void*) == 8) {
        std::cout << "64-bit platform" << std::endl;
    } else {
        std::cout << "32-bit platform" << std::endl;
    }

    // Span (C++20)
    int arr[] = {1, 2, 3, 4, 5};
    std::span<int> span_view(arr);
    for (int val : span_view) {
        std::cout << val << " ";
    }
    std::cout << std::endl;

    // std::source_location (C++20)
    // auto loc = std::source_location::current();
    // std::cout << "File: " << loc.file_name() << ", Line: " << loc.line() << std::endl;

    std::cout << "\nProgram completed successfully!" << std::endl;

    return 0;
}
