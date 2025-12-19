/// <summary>
/// Comprehensive C# language sample demonstrating all syntax features
/// C# is a modern, object-oriented language for .NET
/// </summary>

#nullable enable
#pragma warning disable CS0219 // Variable is assigned but never used

using System;
using System.Collections.Generic;
using System.Collections.Concurrent;
using System.Collections.Immutable;
using System.Linq;
using System.Text;
using System.Text.RegularExpressions;
using System.Threading;
using System.Threading.Tasks;
using System.IO;
using System.Net.Http;
using System.Text.Json;
using System.Text.Json.Serialization;
using System.Reflection;
using System.Runtime.CompilerServices;
using System.Diagnostics;

// Global using (C# 10+)
global using Console = System.Console;

// File-scoped namespace (C# 10+)
namespace Sample;

#region Constants and Static Classes

/// <summary>
/// Application constants
/// </summary>
public static class Constants
{
    public const int MaxBufferSize = 1024;
    public const double Pi = 3.14159265358979323846;
    public const string Greeting = "Hello, C#!";

    public static readonly DateTime StartTime = DateTime.Now;
}

#endregion

#region Enums

/// <summary>
/// Color enumeration with values
/// </summary>
public enum Color : uint
{
    [JsonPropertyName("red")]
    Red = 0xFF0000,

    [JsonPropertyName("green")]
    Green = 0x00FF00,

    [JsonPropertyName("blue")]
    Blue = 0x0000FF,

    White = 0xFFFFFF,
    Black = 0x000000
}

/// <summary>
/// Status enumeration
/// </summary>
public enum Status
{
    Ok = 0,
    Error = 1,
    Pending = 2,
    Timeout = 3
}

/// <summary>
/// Flags enumeration
/// </summary>
[Flags]
public enum Permissions
{
    None = 0,
    Read = 1 << 0,
    Write = 1 << 1,
    Execute = 1 << 2,
    All = Read | Write | Execute
}

#endregion

#region Records

/// <summary>
/// Record type (C# 9+)
/// </summary>
public record Point(double X, double Y, double Z = 0.0)
{
    public double Distance(Point other)
    {
        var dx = X - other.X;
        var dy = Y - other.Y;
        var dz = Z - other.Z;
        return Math.Sqrt(dx * dx + dy * dy + dz * dz);
    }

    public static Point Origin => new(0, 0, 0);

    // Deconstruct for pattern matching
    public void Deconstruct(out double x, out double y, out double z)
    {
        x = X;
        y = Y;
        z = Z;
    }
}

/// <summary>
/// Record struct (C# 10+)
/// </summary>
public readonly record struct PointStruct(double X, double Y);

/// <summary>
/// Person record with init-only properties
/// </summary>
public record Person
{
    public required string Name { get; init; }
    public required int Age { get; init; }
    public string? Email { get; init; }

    public Person() { }

    public Person(string name, int age)
    {
        Name = name;
        Age = age;
    }
}

#endregion

#region Interfaces

/// <summary>
/// Shape interface
/// </summary>
public interface IShape
{
    double Area { get; }
    double Perimeter { get; }

    // Default interface implementation (C# 8+)
    string Describe() => $"Shape: Area={Area:F2}, Perimeter={Perimeter:F2}";
}

/// <summary>
/// Drawable interface
/// </summary>
public interface IDrawable
{
    void Draw();
    void Update(double deltaTime) { }
}

/// <summary>
/// Generic interface with covariance
/// </summary>
public interface IContainer<out T>
{
    T Get(int index);
    int Count { get; }
}

/// <summary>
/// Generic interface with contravariance
/// </summary>
public interface IComparer<in T>
{
    int Compare(T x, T y);
}

#endregion

#region Abstract Classes

/// <summary>
/// Abstract shape class
/// </summary>
public abstract class Shape : IShape, IDrawable
{
    public string Name { get; protected set; }
    public Color Color { get; set; } = Color.Black;

    protected Shape(string name)
    {
        Name = name;
    }

    public abstract double Area { get; }
    public abstract double Perimeter { get; }

    public virtual void Draw()
    {
        Console.WriteLine($"Drawing {Name} in {Color}");
    }

    public virtual void Update(double deltaTime) { }

    public override string ToString() => $"{Name}: Area={Area:F2}";
}

#endregion

#region Concrete Classes

/// <summary>
/// Circle implementation
/// </summary>
public sealed class Circle : Shape
{
    public double Radius { get; set; }

    public Circle(double radius, Color color = Color.Black) : base("Circle")
    {
        Radius = radius;
        Color = color;
    }

    public override double Area => Math.PI * Radius * Radius;
    public override double Perimeter => 2 * Math.PI * Radius;

    public override void Draw()
    {
        base.Draw();
        Console.WriteLine($"  Radius: {Radius}");
    }
}

/// <summary>
/// Rectangle implementation
/// </summary>
public class Rectangle : Shape
{
    public double Width { get; set; }
    public double Height { get; set; }

    public Rectangle(double width, double height, Color color = Color.Black) : base("Rectangle")
    {
        Width = width;
        Height = height;
        Color = color;
    }

    public override double Area => Width * Height;
    public override double Perimeter => 2 * (Width + Height);
}

/// <summary>
/// Square as derived from Rectangle
/// </summary>
public class Square : Rectangle
{
    public double Side
    {
        get => Width;
        set => Width = Height = value;
    }

    public Square(double side, Color color = Color.Black) : base(side, side, color)
    {
        Name = "Square";
    }
}

#endregion

#region Generic Classes

/// <summary>
/// Generic container class
/// </summary>
public class Container<T> : IEnumerable<T>, IContainer<T> where T : notnull
{
    private readonly List<T> _items = new();

    public int Count => _items.Count;

    public void Add(T item) => _items.Add(item);

    public T Get(int index) => _items[index];

    public T this[int index]
    {
        get => _items[index];
        set => _items[index] = value;
    }

    public IEnumerator<T> GetEnumerator() => _items.GetEnumerator();
    System.Collections.IEnumerator System.Collections.IEnumerable.GetEnumerator() => GetEnumerator();
}

/// <summary>
/// Generic pair class
/// </summary>
public class Pair<TFirst, TSecond>
{
    public TFirst First { get; }
    public TSecond Second { get; }

    public Pair(TFirst first, TSecond second)
    {
        First = first;
        Second = second;
    }

    public void Deconstruct(out TFirst first, out TSecond second)
    {
        first = First;
        second = Second;
    }

    public Pair<TSecond, TFirst> Swap() => new(Second, First);
}

/// <summary>
/// Result type for error handling
/// </summary>
public readonly struct Result<T, TError>
{
    private readonly T? _value;
    private readonly TError? _error;

    public bool IsSuccess { get; }
    public T Value => IsSuccess ? _value! : throw new InvalidOperationException("No value");
    public TError Error => !IsSuccess ? _error! : throw new InvalidOperationException("No error");

    private Result(T value)
    {
        _value = value;
        _error = default;
        IsSuccess = true;
    }

    private Result(TError error, bool _)
    {
        _value = default;
        _error = error;
        IsSuccess = false;
    }

    public static Result<T, TError> Success(T value) => new(value);
    public static Result<T, TError> Failure(TError error) => new(error, false);

    public Result<TNew, TError> Map<TNew>(Func<T, TNew> mapper) =>
        IsSuccess ? Result<TNew, TError>.Success(mapper(Value)) : Result<TNew, TError>.Failure(Error);
}

#endregion

#region Delegates and Events

/// <summary>
/// Custom delegate type
/// </summary>
public delegate TResult Func<in T1, in T2, out TResult>(T1 arg1, T2 arg2);

/// <summary>
/// Event args
/// </summary>
public class ValueChangedEventArgs<T> : EventArgs
{
    public T OldValue { get; }
    public T NewValue { get; }

    public ValueChangedEventArgs(T oldValue, T newValue)
    {
        OldValue = oldValue;
        NewValue = newValue;
    }
}

/// <summary>
/// Class with events
/// </summary>
public class Observable<T>
{
    private T _value;

    public event EventHandler<ValueChangedEventArgs<T>>? ValueChanged;

    public T Value
    {
        get => _value;
        set
        {
            var old = _value;
            _value = value;
            OnValueChanged(old, value);
        }
    }

    public Observable(T initialValue)
    {
        _value = initialValue;
    }

    protected virtual void OnValueChanged(T oldValue, T newValue)
    {
        ValueChanged?.Invoke(this, new ValueChangedEventArgs<T>(oldValue, newValue));
    }
}

#endregion

#region Extension Methods

/// <summary>
/// String extension methods
/// </summary>
public static class StringExtensions
{
    public static IEnumerable<string> Words(this string str) =>
        str.Split(new[] { ' ', '\t', '\n' }, StringSplitOptions.RemoveEmptyEntries);

    public static bool IsPalindrome(this string str)
    {
        var normalized = new string(str.ToLower().Where(char.IsLetterOrDigit).ToArray());
        return normalized.SequenceEqual(normalized.Reverse());
    }

    public static int WordCount(this string str) => str.Words().Count();

    public static string Truncate(this string str, int maxLength) =>
        str.Length <= maxLength ? str : str[..maxLength] + "...";
}

/// <summary>
/// Numeric extension methods
/// </summary>
public static class NumericExtensions
{
    public static long Factorial(this int n)
    {
        if (n < 0) throw new ArgumentException("Must be non-negative");
        return n <= 1 ? 1 : Enumerable.Range(1, n).Aggregate(1L, (acc, x) => acc * x);
    }

    public static bool IsEven(this int n) => n % 2 == 0;
    public static bool IsOdd(this int n) => !n.IsEven();

    public static T Clamp<T>(this T value, T min, T max) where T : IComparable<T> =>
        value.CompareTo(min) < 0 ? min : value.CompareTo(max) > 0 ? max : value;
}

#endregion

#region Attributes

/// <summary>
/// Custom attribute
/// </summary>
[AttributeUsage(AttributeTargets.Class | AttributeTargets.Method, AllowMultiple = true)]
public class CustomAttribute : Attribute
{
    public string Value { get; }
    public int Priority { get; set; }

    public CustomAttribute(string value)
    {
        Value = value;
    }
}

/// <summary>
/// Validation attribute
/// </summary>
[AttributeUsage(AttributeTargets.Property)]
public class RangeAttribute : Attribute
{
    public int Min { get; }
    public int Max { get; }

    public RangeAttribute(int min, int max)
    {
        Min = min;
        Max = max;
    }

    public bool IsValid(int value) => value >= Min && value <= Max;
}

#endregion

#region Exceptions

/// <summary>
/// Custom exception
/// </summary>
public class AppException : Exception
{
    public int ErrorCode { get; }

    public AppException(string message, int errorCode) : base(message)
    {
        ErrorCode = errorCode;
    }

    public AppException(string message, int errorCode, Exception inner) : base(message, inner)
    {
        ErrorCode = errorCode;
    }
}

#endregion

#region Main Program

/// <summary>
/// Main program class
/// </summary>
[Custom("main", Priority = 1)]
public class Program
{
    // Static readonly field
    private static readonly HttpClient _httpClient = new();

    // Thread-local storage
    [ThreadStatic]
    private static int _threadLocalValue;

    // Lazy initialization
    private static readonly Lazy<Program> _instance = new(() => new Program());
    public static Program Instance => _instance.Value;

    // Lock object
    private readonly object _lock = new();

    /// <summary>
    /// Main entry point
    /// </summary>
    public static async Task Main(string[] args)
    {
        #region Variables and Types

        // Primitive types
        bool boolVal = true;
        byte byteVal = 255;
        sbyte sbyteVal = -128;
        short shortVal = -32768;
        ushort ushortVal = 65535;
        int intVal = 42;
        uint uintVal = 42u;
        long longVal = 9223372036854775807L;
        ulong ulongVal = 18446744073709551615UL;
        float floatVal = 3.14f;
        double doubleVal = 3.14159265358979;
        decimal decimalVal = 3.14159265358979323846m;
        char charVal = 'A';
        string stringVal = "Hello";

        // Nullable types
        int? nullableInt = null;
        int? nullableWithValue = 42;

        // Type inference
        var inferredInt = 42;
        var inferredString = "Hello";
        var inferredList = new List<int> { 1, 2, 3 };

        // Numeric literals
        int decimal_ = 42;
        int hex = 0xDEADBEEF;
        int binary = 0b1010_1010;
        int withSeparators = 1_000_000;
        double scientific = 1.23e-4;

        // Strings
        string normal = "Hello, World!";
        string verbatim = @"C:\Users\Name\Documents";
        string interpolated = $"Value: {intVal}";
        string interpolatedVerbatim = $@"Path: C:\{stringVal}";
        string rawString = """
            This is a raw string literal
            that can contain "quotes" and \backslashes\
            """;

        // Span and Memory
        Span<int> span = stackalloc int[10];
        ReadOnlySpan<char> charSpan = "Hello".AsSpan();

        #endregion

        #region Collections

        // Arrays
        int[] array = { 1, 2, 3, 4, 5 };
        int[] array2 = new int[10];
        int[,] multiArray = { { 1, 2 }, { 3, 4 } };
        int[][] jaggedArray = { new[] { 1, 2 }, new[] { 3, 4, 5 } };

        // Lists
        List<int> list = new() { 1, 2, 3, 4, 5 };
        list.Add(6);
        list.AddRange(new[] { 7, 8, 9 });

        // Dictionary
        Dictionary<string, int> dict = new()
        {
            ["one"] = 1,
            ["two"] = 2,
            ["three"] = 3
        };

        // HashSet
        HashSet<int> set = new() { 1, 2, 3, 3 }; // Contains 1, 2, 3

        // Queue and Stack
        Queue<int> queue = new();
        queue.Enqueue(1);
        queue.Enqueue(2);

        Stack<int> stack = new();
        stack.Push(1);
        stack.Push(2);

        // Immutable collections
        var immutableList = ImmutableList.Create(1, 2, 3);
        var immutableDict = ImmutableDictionary<string, int>.Empty
            .Add("one", 1)
            .Add("two", 2);

        // Concurrent collections
        var concurrentDict = new ConcurrentDictionary<string, int>();
        concurrentDict.TryAdd("key", 42);

        #endregion

        #region Control Flow

        // If-else
        if (intVal > 0)
        {
            Console.WriteLine("Positive");
        }
        else if (intVal < 0)
        {
            Console.WriteLine("Negative");
        }
        else
        {
            Console.WriteLine("Zero");
        }

        // Ternary operator
        string result = intVal > 0 ? "Positive" : "Non-positive";

        // Null-coalescing
        string defaulted = nullableInt?.ToString() ?? "Default";
        nullableInt ??= 100; // Null-coalescing assignment

        // Switch expression (C# 8+)
        string description = intVal switch
        {
            0 => "Zero",
            > 0 and < 10 => "1-9",
            >= 10 and <= 100 => "10-100",
            < 0 => "Negative",
            _ => "Large"
        };

        // Pattern matching switch
        object obj = "Hello";
        string typeDescription = obj switch
        {
            null => "Null",
            int i => $"Integer: {i}",
            string s => $"String: {s}",
            IEnumerable<int> nums => $"List with {nums.Count()} items",
            { } o => $"Object: {o.GetType().Name}"
        };

        // Property pattern
        var point = new Point(1, 2, 3);
        string pointDesc = point switch
        {
            { X: 0, Y: 0, Z: 0 } => "Origin",
            { X: var x, Y: var y } when x == y => "Diagonal",
            { Z: 0 } => "2D point",
            _ => "3D point"
        };

        // Traditional switch
        switch (intVal)
        {
            case 0:
                Console.WriteLine("Zero");
                break;
            case 1:
            case 2:
            case 3:
                Console.WriteLine("1-3");
                break;
            case > 100:
                Console.WriteLine("Large");
                break;
            default:
                Console.WriteLine("Other");
                break;
        }

        // Loops
        for (int i = 0; i < 10; i++)
        {
            if (i == 5) continue;
            if (i == 8) break;
            Console.WriteLine(i);
        }

        foreach (var item in list)
        {
            Console.WriteLine(item);
        }

        int counter = 0;
        while (counter < 5)
        {
            counter++;
        }

        do
        {
            counter--;
        } while (counter > 0);

        // Labeled statements
        // outer:
        // for (int i = 0; i < 10; i++)
        // {
        //     for (int j = 0; j < 10; j++)
        //     {
        //         if (i * j > 50) goto outer_end;
        //     }
        // }
        // outer_end:;

        #endregion

        #region LINQ

        var numbers = Enumerable.Range(1, 100);

        // Method syntax
        var evens = numbers.Where(n => n % 2 == 0);
        var doubled = numbers.Select(n => n * 2);
        var sum = numbers.Sum();
        var average = numbers.Average();
        var max = numbers.Max();

        // Query syntax
        var query = from n in numbers
                    where n % 2 == 0
                    orderby n descending
                    select n * 2;

        // Complex LINQ
        var grouped = numbers
            .GroupBy(n => n % 10)
            .Select(g => new { Digit = g.Key, Numbers = g.ToList() });

        var joined = from p in new[] { new { Id = 1, Name = "A" } }
                     join c in new[] { new { ParentId = 1, Value = "X" } }
                         on p.Id equals c.ParentId
                     select new { p.Name, c.Value };

        // Aggregate
        var factorial = Enumerable.Range(1, 5)
            .Aggregate(1L, (acc, n) => acc * n);

        #endregion

        #region Async/Await

        // Async method call
        var data = await FetchDataAsync("https://example.com");

        // Task combinators
        var task1 = Task.FromResult(1);
        var task2 = Task.FromResult(2);

        var results = await Task.WhenAll(task1, task2);
        var first = await Task.WhenAny(task1, task2);

        // ValueTask
        ValueTask<int> valueTask = GetValueAsync();
        int value = await valueTask;

        // Cancellation
        using var cts = new CancellationTokenSource();
        cts.CancelAfter(TimeSpan.FromSeconds(10));

        try
        {
            await LongRunningOperationAsync(cts.Token);
        }
        catch (OperationCanceledException)
        {
            Console.WriteLine("Operation cancelled");
        }

        // IAsyncEnumerable (C# 8+)
        await foreach (var item in GenerateAsync())
        {
            Console.WriteLine(item);
        }

        #endregion

        #region Exception Handling

        try
        {
            throw new AppException("Something went wrong", 42);
        }
        catch (AppException ex) when (ex.ErrorCode == 42)
        {
            Console.WriteLine($"App error {ex.ErrorCode}: {ex.Message}");
        }
        catch (Exception ex)
        {
            Console.WriteLine($"Error: {ex.Message}");
            throw; // Re-throw
        }
        finally
        {
            Console.WriteLine("Cleanup");
        }

        #endregion

        #region Classes and Objects

        // Object creation
        var circle = new Circle(5, Color.Blue);
        circle.Draw();
        Console.WriteLine($"Area: {circle.Area}");

        var rect = new Rectangle(4, 6);
        Console.WriteLine(rect.ToString());

        // Records
        var person = new Person { Name = "Alice", Age = 30, Email = "alice@example.com" };
        var olderPerson = person with { Age = 31 };
        var (name, age, email) = (person.Name, person.Age, person.Email);

        // Pattern matching with records
        if (point is Point(var px, var py, var pz))
        {
            Console.WriteLine($"Point: ({px}, {py}, {pz})");
        }

        // Container
        var container = new Container<string>();
        container.Add("Hello");
        container.Add("World");

        foreach (var item in container)
        {
            Console.WriteLine(item);
        }

        #endregion

        #region Delegates and Events

        // Func and Action
        Func<int, int, int> add = (a, b) => a + b;
        Action<string> print = s => Console.WriteLine(s);
        Predicate<int> isPositive = n => n > 0;

        // Lambda expressions
        Func<int, int> square = x => x * x;
        Func<int, int, int> multiply = (x, y) => x * y;

        // Statement lambda
        Func<int, int> factorial2 = n =>
        {
            int result = 1;
            for (int i = 2; i <= n; i++)
                result *= i;
            return result;
        };

        // Local function
        int Factorial(int n) => n <= 1 ? 1 : n * Factorial(n - 1);

        // Events
        var observable = new Observable<int>(0);
        observable.ValueChanged += (sender, e) =>
        {
            Console.WriteLine($"Value changed from {e.OldValue} to {e.NewValue}");
        };
        observable.Value = 42;

        #endregion

        #region Reflection

        Type type = typeof(Circle);
        Console.WriteLine($"Type: {type.Name}");

        foreach (var property in type.GetProperties())
        {
            Console.WriteLine($"Property: {property.Name} ({property.PropertyType.Name})");
        }

        foreach (var method in type.GetMethods(BindingFlags.Public | BindingFlags.Instance | BindingFlags.DeclaredOnly))
        {
            Console.WriteLine($"Method: {method.Name}");
        }

        // Get custom attributes
        var attrs = type.GetCustomAttributes<CustomAttribute>();
        foreach (var attr in attrs)
        {
            Console.WriteLine($"Attribute: {attr.Value} (Priority: {attr.Priority})");
        }

        // Create instance dynamically
        var instance = Activator.CreateInstance(type, 5.0, Color.Red) as Circle;

        #endregion

        #region Patterns (C# 9+)

        // Type pattern
        if (obj is string str)
        {
            Console.WriteLine($"String: {str}");
        }

        // Relational pattern
        int temperature = 25;
        string weather = temperature switch
        {
            < 0 => "Freezing",
            < 15 => "Cold",
            < 25 => "Comfortable",
            < 35 => "Warm",
            _ => "Hot"
        };

        // Logical patterns
        bool isValidAge = intVal is >= 0 and <= 150;
        bool isSpecial = intVal is 0 or 1 or 42;

        // List patterns (C# 11)
        int[] arr = { 1, 2, 3, 4, 5 };
        if (arr is [var head, .. var middle, var tail])
        {
            Console.WriteLine($"Head: {head}, Tail: {tail}");
        }

        string arrPattern = arr switch
        {
            [] => "Empty",
            [var single] => $"Single: {single}",
            [var first, var second, ..] => $"First two: {first}, {second}"
        };

        #endregion

        #region Memory and Span

        // Stackalloc
        Span<int> stackArray = stackalloc int[10];
        for (int i = 0; i < stackArray.Length; i++)
        {
            stackArray[i] = i;
        }

        // Slice
        Span<int> slice = stackArray[2..5];

        // String as span
        ReadOnlySpan<char> stringSpan = "Hello, World!".AsSpan();
        ReadOnlySpan<char> substring = stringSpan[0..5];

        #endregion

        #region File I/O

        // Modern file operations
        await File.WriteAllTextAsync("test.txt", "Hello, File!");
        string content = await File.ReadAllTextAsync("test.txt");

        // Using statement
        await using var fileStream = new FileStream("test.txt", FileMode.Open);
        using var reader = new StreamReader(fileStream);
        string line = await reader.ReadLineAsync() ?? "";

        // File cleanup
        if (File.Exists("test.txt"))
        {
            File.Delete("test.txt");
        }

        #endregion

        Console.WriteLine(Constants.Greeting);
        Console.WriteLine("Program completed successfully!");
    }

    #region Helper Methods

    private static async Task<string> FetchDataAsync(string url)
    {
        await Task.Delay(100);
        return $"Data from {url}";
    }

    private static async ValueTask<int> GetValueAsync()
    {
        await Task.Delay(10);
        return 42;
    }

    private static async Task LongRunningOperationAsync(CancellationToken token)
    {
        for (int i = 0; i < 10; i++)
        {
            token.ThrowIfCancellationRequested();
            await Task.Delay(100, token);
        }
    }

    private static async IAsyncEnumerable<int> GenerateAsync(
        [EnumeratorCancellation] CancellationToken token = default)
    {
        for (int i = 0; i < 10; i++)
        {
            await Task.Delay(50, token);
            yield return i;
        }
    }

    #endregion
}

#endregion

#pragma warning restore CS0219
