' Comprehensive VB.NET language sample demonstrating all syntax features
' VB.NET is an object-oriented language for .NET with verbose syntax

Option Explicit On
Option Strict On
Option Infer On
Option Compare Text

Imports System
Imports System.IO
Imports System.Text
Imports System.Text.RegularExpressions
Imports System.Collections.Generic
Imports System.Linq
Imports System.Threading
Imports System.Threading.Tasks
Imports System.Reflection
Imports System.Runtime.CompilerServices

' ============================================================================
' Module with Constants
' ============================================================================

''' <summary>
''' Application constants module
''' </summary>
Public Module Constants
    Public Const MaxBufferSize As Integer = 1024
    Public Const Pi As Double = 3.14159265358979323846
    Public Const Greeting As String = "Hello, VB.NET!"

    Public ReadOnly StartTime As DateTime = DateTime.Now
End Module

' ============================================================================
' Enumerations
' ============================================================================

''' <summary>
''' Color enumeration with values
''' </summary>
Public Enum Color As UInteger
    Red = &HFF0000UI
    Green = &H00FF00UI
    Blue = &H0000FFUI
    White = &HFFFFFFUI
    Black = &H000000UI
End Enum

''' <summary>
''' Status enumeration
''' </summary>
Public Enum Status
    Ok = 0
    [Error] = 1
    Pending = 2
    Timeout = 3
End Enum

''' <summary>
''' Flags enumeration
''' </summary>
<Flags>
Public Enum Permissions
    None = 0
    Read = 1
    Write = 2
    Execute = 4
    All = Read Or Write Or Execute
End Enum

' ============================================================================
' Structures
' ============================================================================

''' <summary>
''' Point structure
''' </summary>
Public Structure Point
    Public X As Double
    Public Y As Double
    Public Z As Double

    Public Sub New(x As Double, y As Double, Optional z As Double = 0.0)
        Me.X = x
        Me.Y = y
        Me.Z = z
    End Sub

    Public Function Distance(other As Point) As Double
        Dim dx As Double = X - other.X
        Dim dy As Double = Y - other.Y
        Dim dz As Double = Z - other.Z
        Return Math.Sqrt(dx * dx + dy * dy + dz * dz)
    End Function

    Public Shared ReadOnly Property Origin As Point
        Get
            Return New Point(0, 0, 0)
        End Get
    End Property

    Public Shared Operator +(a As Point, b As Point) As Point
        Return New Point(a.X + b.X, a.Y + b.Y, a.Z + b.Z)
    End Operator

    Public Shared Operator -(a As Point, b As Point) As Point
        Return New Point(a.X - b.X, a.Y - b.Y, a.Z - b.Z)
    End Operator

    Public Shared Operator *(p As Point, scalar As Double) As Point
        Return New Point(p.X * scalar, p.Y * scalar, p.Z * scalar)
    End Operator

    Public Overrides Function ToString() As String
        Return $"Point({X}, {Y}, {Z})"
    End Function
End Structure

' ============================================================================
' Interfaces
' ============================================================================

''' <summary>
''' Shape interface
''' </summary>
Public Interface IShape
    ReadOnly Property Area As Double
    ReadOnly Property Perimeter As Double
    Function Describe() As String
End Interface

''' <summary>
''' Drawable interface
''' </summary>
Public Interface IDrawable
    Sub Draw()
    Sub Update(deltaTime As Double)
End Interface

''' <summary>
''' Generic container interface
''' </summary>
Public Interface IContainer(Of T)
    Function [Get](index As Integer) As T
    ReadOnly Property Count As Integer
End Interface

' ============================================================================
' Abstract Classes
' ============================================================================

''' <summary>
''' Abstract shape class
''' </summary>
Public MustInherit Class Shape
    Implements IShape, IDrawable

    Private _name As String
    Private _color As Color

    Protected Sub New(name As String)
        _name = name
        _color = Color.Black
    End Sub

    Public Property Name As String
        Get
            Return _name
        End Get
        Protected Set(value As String)
            _name = value
        End Set
    End Property

    Public Property ShapeColor As Color
        Get
            Return _color
        End Get
        Set(value As Color)
            _color = value
        End Set
    End Property

    Public MustOverride ReadOnly Property Area As Double Implements IShape.Area
    Public MustOverride ReadOnly Property Perimeter As Double Implements IShape.Perimeter

    Public Overridable Function Describe() As String Implements IShape.Describe
        Return $"{Name}: Area={Area:F2}, Perimeter={Perimeter:F2}"
    End Function

    Public Overridable Sub Draw() Implements IDrawable.Draw
        Console.WriteLine($"Drawing {Name} in {ShapeColor}")
    End Sub

    Public Overridable Sub Update(deltaTime As Double) Implements IDrawable.Update
        ' Default implementation does nothing
    End Sub

    Public Overrides Function ToString() As String
        Return Describe()
    End Function
End Class

' ============================================================================
' Concrete Classes
' ============================================================================

''' <summary>
''' Circle implementation
''' </summary>
Public NotInheritable Class Circle
    Inherits Shape

    Private _radius As Double

    Public Sub New(radius As Double, Optional color As Color = Color.Black)
        MyBase.New("Circle")
        _radius = radius
        ShapeColor = color
    End Sub

    Public Property Radius As Double
        Get
            Return _radius
        End Get
        Set(value As Double)
            _radius = value
        End Set
    End Property

    Public Overrides ReadOnly Property Area As Double
        Get
            Return Math.PI * Radius * Radius
        End Get
    End Property

    Public Overrides ReadOnly Property Perimeter As Double
        Get
            Return 2 * Math.PI * Radius
        End Get
    End Property

    Public Overrides Sub Draw()
        MyBase.Draw()
        Console.WriteLine($"  Radius: {Radius}")
    End Sub
End Class

''' <summary>
''' Rectangle implementation
''' </summary>
Public Class Rectangle
    Inherits Shape

    Private _width As Double
    Private _height As Double

    Public Sub New(width As Double, height As Double, Optional color As Color = Color.Black)
        MyBase.New("Rectangle")
        _width = width
        _height = height
        ShapeColor = color
    End Sub

    Public Property Width As Double
        Get
            Return _width
        End Get
        Set(value As Double)
            _width = value
        End Set
    End Property

    Public Property Height As Double
        Get
            Return _height
        End Get
        Set(value As Double)
            _height = value
        End Set
    End Property

    Public Overrides ReadOnly Property Area As Double
        Get
            Return Width * Height
        End Get
    End Property

    Public Overrides ReadOnly Property Perimeter As Double
        Get
            Return 2 * (Width + Height)
        End Get
    End Property
End Class

''' <summary>
''' Square as derived from Rectangle
''' </summary>
Public Class Square
    Inherits Rectangle

    Public Sub New(side As Double, Optional color As Color = Color.Black)
        MyBase.New(side, side, color)
        Name = "Square"
    End Sub

    Public Property Side As Double
        Get
            Return Width
        End Get
        Set(value As Double)
            Width = value
            Height = value
        End Set
    End Property
End Class

' ============================================================================
' Generic Classes
' ============================================================================

''' <summary>
''' Generic container class
''' </summary>
Public Class Container(Of T)
    Implements IEnumerable(Of T), IContainer(Of T)

    Private ReadOnly _items As New List(Of T)

    Public Sub Add(item As T)
        _items.Add(item)
    End Sub

    Public Function [Get](index As Integer) As T Implements IContainer(Of T).Get
        Return _items(index)
    End Function

    Public ReadOnly Property Count As Integer Implements IContainer(Of T).Count
        Get
            Return _items.Count
        End Get
    End Property

    Default Public Property Item(index As Integer) As T
        Get
            Return _items(index)
        End Get
        Set(value As T)
            _items(index) = value
        End Set
    End Property

    Public Function GetEnumerator() As IEnumerator(Of T) Implements IEnumerable(Of T).GetEnumerator
        Return _items.GetEnumerator()
    End Function

    Private Function IEnumerable_GetEnumerator() As IEnumerator Implements IEnumerable.GetEnumerator
        Return GetEnumerator()
    End Function
End Class

''' <summary>
''' Generic pair class
''' </summary>
Public Class Pair(Of TFirst, TSecond)
    Public ReadOnly Property First As TFirst
    Public ReadOnly Property Second As TSecond

    Public Sub New(first As TFirst, second As TSecond)
        Me.First = first
        Me.Second = second
    End Sub

    Public Function Swap() As Pair(Of TSecond, TFirst)
        Return New Pair(Of TSecond, TFirst)(Second, First)
    End Function

    Public Overrides Function ToString() As String
        Return $"Pair({First}, {Second})"
    End Function
End Class

' ============================================================================
' Delegates and Events
' ============================================================================

''' <summary>
''' Custom delegate type
''' </summary>
Public Delegate Function Calculator(a As Integer, b As Integer) As Integer

''' <summary>
''' Event args class
''' </summary>
Public Class ValueChangedEventArgs(Of T)
    Inherits EventArgs

    Public ReadOnly Property OldValue As T
    Public ReadOnly Property NewValue As T

    Public Sub New(oldValue As T, newValue As T)
        Me.OldValue = oldValue
        Me.NewValue = newValue
    End Sub
End Class

''' <summary>
''' Observable class with events
''' </summary>
Public Class Observable(Of T)
    Private _value As T

    Public Event ValueChanged As EventHandler(Of ValueChangedEventArgs(Of T))

    Public Property Value As T
        Get
            Return _value
        End Get
        Set(value As T)
            Dim oldValue = _value
            _value = value
            OnValueChanged(oldValue, value)
        End Set
    End Property

    Public Sub New(initialValue As T)
        _value = initialValue
    End Sub

    Protected Overridable Sub OnValueChanged(oldValue As T, newValue As T)
        RaiseEvent ValueChanged(Me, New ValueChangedEventArgs(Of T)(oldValue, newValue))
    End Sub
End Class

' ============================================================================
' Extension Methods Module
' ============================================================================

''' <summary>
''' String extension methods
''' </summary>
Public Module StringExtensions
    <Extension>
    Public Function Words(str As String) As IEnumerable(Of String)
        Return str.Split({" "c, ChrW(9), ChrW(10)}, StringSplitOptions.RemoveEmptyEntries)
    End Function

    <Extension>
    Public Function IsPalindrome(str As String) As Boolean
        Dim normalized = New String(str.ToLower().Where(Function(c) Char.IsLetterOrDigit(c)).ToArray())
        Return normalized.SequenceEqual(normalized.Reverse())
    End Function

    <Extension>
    Public Function WordCount(str As String) As Integer
        Return str.Words().Count()
    End Function

    <Extension>
    Public Function Truncate(str As String, maxLength As Integer) As String
        If str.Length <= maxLength Then Return str
        Return str.Substring(0, maxLength) & "..."
    End Function
End Module

''' <summary>
''' Numeric extension methods
''' </summary>
Public Module NumericExtensions
    <Extension>
    Public Function Factorial(n As Integer) As Long
        If n < 0 Then Throw New ArgumentException("Must be non-negative")
        Return If(n <= 1, 1L, Enumerable.Range(1, n).Aggregate(1L, Function(acc, x) acc * x))
    End Function

    <Extension>
    Public Function IsEven(n As Integer) As Boolean
        Return n Mod 2 = 0
    End Function

    <Extension>
    Public Function IsOdd(n As Integer) As Boolean
        Return Not n.IsEven()
    End Function
End Module

' ============================================================================
' Custom Attributes
' ============================================================================

''' <summary>
''' Custom attribute
''' </summary>
<AttributeUsage(AttributeTargets.Class Or AttributeTargets.Method, AllowMultiple:=True)>
Public Class CustomAttribute
    Inherits Attribute

    Public ReadOnly Property Value As String
    Public Property Priority As Integer

    Public Sub New(value As String)
        Me.Value = value
    End Sub
End Class

' ============================================================================
' Exceptions
' ============================================================================

''' <summary>
''' Custom exception
''' </summary>
Public Class AppException
    Inherits Exception

    Public ReadOnly Property ErrorCode As Integer

    Public Sub New(message As String, errorCode As Integer)
        MyBase.New(message)
        Me.ErrorCode = errorCode
    End Sub

    Public Sub New(message As String, errorCode As Integer, innerException As Exception)
        MyBase.New(message, innerException)
        Me.ErrorCode = errorCode
    End Sub
End Class

' ============================================================================
' Main Program
' ============================================================================

''' <summary>
''' Main program class
''' </summary>
<Custom("main", Priority:=1)>
Public Class Program
    ' Shared readonly field
    Private Shared ReadOnly _startTime As DateTime = DateTime.Now

    ' Lazy initialization
    Private Shared ReadOnly _instance As New Lazy(Of Program)(Function() New Program())
    Public Shared ReadOnly Property Instance As Program
        Get
            Return _instance.Value
        End Get
    End Property

    ' Lock object
    Private ReadOnly _lock As New Object()

    ''' <summary>
    ''' Main entry point
    ''' </summary>
    Public Shared Sub Main(args As String())

#Region "Variables and Types"

        ' Primitive types
        Dim boolVal As Boolean = True
        Dim byteVal As Byte = 255
        Dim sbyteVal As SByte = -128
        Dim shortVal As Short = -32768
        Dim ushortVal As UShort = 65535US
        Dim intVal As Integer = 42
        Dim uintVal As UInteger = 42UI
        Dim longVal As Long = 9223372036854775807L
        Dim ulongVal As ULong = 18446744073709551615UL
        Dim singleVal As Single = 3.14F
        Dim doubleVal As Double = 3.14159265358979
        Dim decimalVal As Decimal = 3.14159265358979323846D
        Dim charVal As Char = "A"c
        Dim stringVal As String = "Hello"
        Dim dateVal As Date = #1/1/2024#
        Dim objectVal As Object = 42

        ' Nullable types
        Dim nullableInt As Integer? = Nothing
        Dim nullableWithValue As Integer? = 42

        ' Type inference
        Dim inferredInt = 42
        Dim inferredString = "Hello"
        Dim inferredList = New List(Of Integer) From {1, 2, 3}

        ' Numeric literals
        Dim decimal_ = 42
        Dim hex = &HDEADBEEF
        Dim octal = &O755
        Dim binary = &B10101010

        ' Strings
        Dim normal = "Hello, World!"
        Dim verbatim = "C:\Users\Name\Documents"
        Dim interpolated = $"Value: {intVal}"
        Dim multiline = "This is a " &
                        "multi-line string"

#End Region

#Region "Collections"

        ' Arrays
        Dim array() As Integer = {1, 2, 3, 4, 5}
        Dim array2(9) As Integer
        Dim multiArray(,) As Integer = {{1, 2}, {3, 4}}
        Dim jaggedArray()() As Integer = {New Integer() {1, 2}, New Integer() {3, 4, 5}}

        ' Lists
        Dim list As New List(Of Integer) From {1, 2, 3, 4, 5}
        list.Add(6)
        list.AddRange({7, 8, 9})

        ' Dictionary
        Dim dict As New Dictionary(Of String, Integer) From {
            {"one", 1},
            {"two", 2},
            {"three", 3}
        }

        ' HashSet
        Dim [set] As New HashSet(Of Integer) From {1, 2, 3, 3}

        ' Queue and Stack
        Dim queue As New Queue(Of Integer)
        queue.Enqueue(1)
        queue.Enqueue(2)

        Dim stack As New Stack(Of Integer)
        stack.Push(1)
        stack.Push(2)

#End Region

#Region "Control Flow"

        ' If-Then-Else
        If intVal > 0 Then
            Console.WriteLine("Positive")
        ElseIf intVal < 0 Then
            Console.WriteLine("Negative")
        Else
            Console.WriteLine("Zero")
        End If

        ' Single-line If
        If intVal > 0 Then Console.WriteLine("Positive")

        ' Ternary-like (If function)
        Dim result = If(intVal > 0, "Positive", "Non-positive")

        ' Null-coalescing
        Dim defaulted = If(nullableInt, 0)
        Dim nullCoalescing = If(CType(Nothing, String), "Default")

        ' Select Case
        Select Case intVal
            Case 0
                Console.WriteLine("Zero")
            Case 1 To 10
                Console.WriteLine("1-10")
            Case 11, 12, 13
                Console.WriteLine("11, 12, or 13")
            Case Is > 100
                Console.WriteLine("Large")
            Case Else
                Console.WriteLine("Other")
        End Select

        ' For loop
        For i = 0 To 9
            If i = 5 Then Continue For
            If i = 8 Then Exit For
            Console.WriteLine(i)
        Next

        ' For loop with Step
        For i = 10 To 0 Step -1
            Console.WriteLine(i)
        Next

        ' For Each
        For Each item In list
            Console.WriteLine(item)
        Next

        ' While loop
        Dim counter = 0
        While counter < 5
            counter += 1
        End While

        ' Do While loop
        Do While counter > 0
            counter -= 1
        Loop

        ' Do Until loop
        Do Until counter >= 5
            counter += 1
        Loop

        ' Do Loop While
        Do
            counter -= 1
        Loop While counter > 0

        ' Do Loop Until
        Do
            counter += 1
        Loop Until counter >= 5

#End Region

#Region "LINQ"

        Dim numbers = Enumerable.Range(1, 100)

        ' Method syntax
        Dim evens = numbers.Where(Function(n) n Mod 2 = 0)
        Dim doubled = numbers.Select(Function(n) n * 2)
        Dim sum = numbers.Sum()
        Dim average = numbers.Average()
        Dim max = numbers.Max()

        ' Query syntax
        Dim query = From n In numbers
                    Where n Mod 2 = 0
                    Order By n Descending
                    Select n * 2

        ' Complex LINQ
        Dim grouped = From n In numbers
                      Group n By Key = n Mod 10 Into Group
                      Select Digit = Key, Numbers = Group.ToList()

        ' Aggregate
        Dim factorial = Enumerable.Range(1, 5).Aggregate(1L, Function(acc, n) acc * n)

#End Region

#Region "Classes and Objects"

        ' Object creation
        Dim circle = New Circle(5, Color.Blue)
        circle.Draw()
        Console.WriteLine($"Area: {circle.Area}")

        Dim rect = New Rectangle(4, 6)
        Console.WriteLine(rect.ToString())

        ' Structure
        Dim point = New Point(1, 2, 3)
        Console.WriteLine($"Point: {point}")
        Console.WriteLine($"Distance from origin: {point.Distance(Point.Origin):F2}")

        ' Container
        Dim container = New Container(Of String)
        container.Add("Hello")
        container.Add("World")

        For Each item In container
            Console.WriteLine(item)
        Next

        ' Pair
        Dim pair = New Pair(Of String, Integer)("Answer", 42)
        Console.WriteLine($"{pair.First}: {pair.Second}")

#End Region

#Region "Delegates and Events"

        ' Delegate
        Dim add As Calculator = Function(a, b) a + b
        Console.WriteLine($"Add: {add(1, 2)}")

        ' Lambda expressions
        Dim square As Func(Of Integer, Integer) = Function(x) x * x
        Dim multiply As Func(Of Integer, Integer, Integer) = Function(x, y) x * y

        ' Multi-line lambda
        Dim factorial2 As Func(Of Integer, Long) = Function(n)
                                                       Dim result As Long = 1
                                                       For i = 2 To n
                                                           result *= i
                                                       Next
                                                       Return result
                                                   End Function

        ' Action (Sub lambda)
        Dim print As Action(Of String) = Sub(s) Console.WriteLine(s)

        ' Events
        Dim observable = New Observable(Of Integer)(0)
        AddHandler observable.ValueChanged, Sub(sender, e)
                                                 Console.WriteLine($"Value changed from {e.OldValue} to {e.NewValue}")
                                             End Sub
        observable.Value = 42

#End Region

#Region "Exception Handling"

        Try
            Throw New AppException("Something went wrong", 42)
        Catch ex As AppException When ex.ErrorCode = 42
            Console.WriteLine($"App error {ex.ErrorCode}: {ex.Message}")
        Catch ex As Exception
            Console.WriteLine($"Error: {ex.Message}")
            Throw
        Finally
            Console.WriteLine("Cleanup")
        End Try

#End Region

#Region "With Statement"

        Dim person As New With {.Name = "Alice", .Age = 30}
        With person
            Console.WriteLine(.Name)
            Console.WriteLine(.Age)
        End With

#End Region

#Region "Using Statement"

        Using reader As New StreamReader("test.txt")
            Dim content = reader.ReadToEnd()
        End Using

#End Region

#Region "Operators"

        ' Arithmetic operators
        Console.WriteLine($"10 + 3 = {10 + 3}")
        Console.WriteLine($"10 - 3 = {10 - 3}")
        Console.WriteLine($"10 * 3 = {10 * 3}")
        Console.WriteLine($"10 / 3 = {10 / 3}")
        Console.WriteLine($"10 \ 3 = {10 \ 3}")  ' Integer division
        Console.WriteLine($"10 Mod 3 = {10 Mod 3}")
        Console.WriteLine($"2 ^ 10 = {2 ^ 10}")  ' Power

        ' Comparison operators
        Console.WriteLine($"10 = 10: {10 = 10}")
        Console.WriteLine($"10 <> 5: {10 <> 5}")
        Console.WriteLine($"10 > 5: {10 > 5}")
        Console.WriteLine($"10 < 5: {10 < 5}")
        Console.WriteLine($"10 >= 10: {10 >= 10}")
        Console.WriteLine($"10 <= 10: {10 <= 10}")

        ' Logical operators
        Console.WriteLine($"True And False: {True And False}")
        Console.WriteLine($"True Or False: {True Or False}")
        Console.WriteLine($"Not True: {Not True}")
        Console.WriteLine($"True Xor False: {True Xor False}")

        ' Short-circuit operators
        Console.WriteLine($"True AndAlso False: {True AndAlso False}")
        Console.WriteLine($"True OrElse False: {True OrElse False}")

        ' String concatenation
        Console.WriteLine("Hello" & " " & "World")

        ' Type operators
        Dim obj As Object = "Hello"
        Console.WriteLine($"Is String: {TypeOf obj Is String}")
        Console.WriteLine($"TypeName: {TypeName(obj)}")

        ' Type conversion
        Dim intFromString = CInt("42")
        Dim stringFromInt = CStr(42)
        Dim doubleFromInt = CDbl(42)

        ' DirectCast and TryCast
        Dim str = DirectCast(obj, String)
        Dim maybeStr = TryCast(obj, String)

#End Region

#Region "Reflection"

        Dim type = GetType(Circle)
        Console.WriteLine($"Type: {type.Name}")

        For Each prop In type.GetProperties()
            Console.WriteLine($"Property: {prop.Name} ({prop.PropertyType.Name})")
        Next

        For Each method In type.GetMethods(BindingFlags.Public Or BindingFlags.Instance Or BindingFlags.DeclaredOnly)
            Console.WriteLine($"Method: {method.Name}")
        Next

#End Region

        Console.WriteLine(Greeting)
        Console.WriteLine("Program completed successfully!")
    End Sub

#Region "Helper Methods"

    Private Shared Async Function FetchDataAsync(url As String) As Task(Of String)
        Await Task.Delay(100)
        Return $"Data from {url}"
    End Function

    Private Shared Iterator Function GenerateNumbers() As IEnumerable(Of Integer)
        For i = 1 To 10
            Yield i
        Next
    End Function

#End Region

End Class
