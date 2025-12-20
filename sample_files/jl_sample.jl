# ==============================================================================
# Comprehensive Julia Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This file demonstrates all major Julia language features
# for syntax highlighting purposes.

# ==============================================================================
# Comments
# ==============================================================================

# Single line comment

#=
Multi-line
comment (block comment)
=#

#= Nested #= block =# comments are supported =#

# ==============================================================================
# Variables and Constants
# ==============================================================================

# Variable assignment
x = 42
y = 3.14
message = "Hello, Julia!"

# Unicode variable names
α = 0.5
β = 1.0
∑ = sum
π_approx = 3.14159
x₁ = 1
x² = 4

# Constants
const GRAVITY = 9.81
const MAX_ITERATIONS = 1000
const APP_NAME = "MyApp"

# Type annotations
typed_var::Int64 = 42
float_var::Float64 = 3.14

# ==============================================================================
# Primitive Types
# ==============================================================================

# Integers
int8_val::Int8 = 127
int16_val::Int16 = 32767
int32_val::Int32 = 2147483647
int64_val::Int64 = 9223372036854775807
int128_val::Int128 = 170141183460469231731687303715884105727

uint8_val::UInt8 = 255
uint64_val::UInt64 = 18446744073709551615

# Integer literals
decimal = 42
hex = 0xDEADBEEF
octal = 0o755
binary = 0b11010110
with_underscore = 1_000_000

# Floating-point
float16_val::Float16 = Float16(1.5)
float32_val::Float32 = 3.14f0
float64_val::Float64 = 3.14159265358979

# Special float values
pos_inf = Inf
neg_inf = -Inf
not_a_number = NaN

# Scientific notation
scientific = 6.022e23
scientific_neg = 1.0e-10

# BigInt and BigFloat
big_int = big"123456789012345678901234567890"
big_float = big"3.141592653589793238462643383279502884197"

# Rational numbers
rational = 3//4
rational_neg = -1//2

# Complex numbers
complex_val = 3 + 4im
complex_float = 1.5 + 2.5im

# Characters
char_val = 'A'
unicode_char = 'α'
emoji_char = '😀'
escape_char = '\n'
hex_char = '\x41'
unicode_escape = '\u03B1'

# Booleans
bool_true = true
bool_false = false

# Nothing and Missing
nothing_val = nothing
missing_val = missing

# ==============================================================================
# Strings
# ==============================================================================

# String literals
simple_string = "Hello, Julia!"
with_escapes = "Line 1\nLine 2\tTabbed"
with_quotes = "She said, \"Hello!\""
unicode_string = "Hello, 世界! α β γ"

# String interpolation
name = "Alice"
age = 30
greeting = "Hello, $name! You are $age years old."
expression = "2 + 2 = $(2 + 2)"

# Multi-line strings
multiline = """
This is a multi-line
string in Julia.
It preserves newlines.
"""

# Raw strings (no escape processing)
raw_string = raw"No \n escape here"
raw_multiline = raw"""
Raw multi-line
with \t and \n literally
"""

# Regex
regex = r"^\d{3}-\d{4}$"
regex_multiline = r"^start.*end$"ms
regex_case_insensitive = r"hello"i

# String operations
str = "Hello, World!"
length(str)
sizeof(str)
uppercase(str)
lowercase(str)
titlecase(str)
reverse(str)
replace(str, "World" => "Julia")
split(str, ", ")
join(["a", "b", "c"], "-")
strip("  hello  ")
startswith(str, "Hello")
endswith(str, "!")
occursin("World", str)
findfirst("World", str)
SubString(str, 1, 5)
str[1]  # First character (1-indexed!)
str[1:5]  # Substring
str[end]  # Last character

# String multiplication and power
repeated = "ab" ^ 3  # "ababab"

# ==============================================================================
# Arrays
# ==============================================================================

# 1D Arrays (Vectors)
vec = [1, 2, 3, 4, 5]
vec_typed = Int64[1, 2, 3]
vec_float = [1.0, 2.0, 3.0]
vec_any = Any[1, "two", 3.0]
vec_empty = Int64[]

# Array comprehension
squares = [x^2 for x in 1:10]
filtered = [x for x in 1:20 if x % 2 == 0]
nested = [x * y for x in 1:3 for y in 1:3]

# Generator expressions
gen = (x^2 for x in 1:10)
sum_gen = sum(x^2 for x in 1:10)

# Ranges
range1 = 1:10
range_step = 1:2:10
range_float = 0.0:0.1:1.0
range_obj = range(0, 10, length=11)

# Array creation functions
zeros_arr = zeros(5)
zeros_int = zeros(Int64, 5)
ones_arr = ones(3, 3)
fill_arr = fill(42, 5)
trues_arr = trues(5)
falses_arr = falses(5)
similar_arr = similar(vec)
copy_arr = copy(vec)
deepcopy_arr = deepcopy(vec)

# Array indexing (1-based!)
vec[1]          # First element
vec[end]        # Last element
vec[end-1]      # Second to last
vec[2:4]        # Slice
vec[1:2:5]      # Every other element
vec[[1, 3, 5]]  # Specific indices
vec[vec .> 2]   # Boolean indexing

# 2D Arrays (Matrices)
mat = [1 2 3; 4 5 6; 7 8 9]
mat_typed = Float64[1 2; 3 4]
mat_zeros = zeros(3, 4)
mat_ones = ones(2, 3)
mat_rand = rand(3, 3)
mat_randn = randn(3, 3)
mat_eye = Matrix{Float64}(I, 3, 3)
mat_diag = diagm([1, 2, 3])

# Matrix indexing
mat[1, 2]       # Element at row 1, col 2
mat[1, :]       # First row
mat[:, 2]       # Second column
mat[1:2, 2:3]   # Submatrix

# Array operations
push!(vec, 6)           # Add to end
pop!(vec)               # Remove from end
pushfirst!(vec, 0)      # Add to beginning
popfirst!(vec)          # Remove from beginning
insert!(vec, 2, 10)     # Insert at position
deleteat!(vec, 2)       # Delete at position
append!(vec, [6, 7, 8]) # Append array
prepend!(vec, [-1, 0])  # Prepend array
sort!(vec)              # Sort in place
reverse!(vec)           # Reverse in place
unique!(vec)            # Remove duplicates

# Array functions
length(vec)
size(mat)
ndims(mat)
eltype(vec)
sum(vec)
prod(vec)
mean(vec)
std(vec)
var(vec)
minimum(vec)
maximum(vec)
extrema(vec)
argmin(vec)
argmax(vec)
findmin(vec)
findmax(vec)
any(x -> x > 3, vec)
all(x -> x > 0, vec)
count(x -> x > 3, vec)
filter(x -> x > 3, vec)
map(x -> x^2, vec)
reduce(+, vec)
foldl(-, vec)
foldr(-, vec)
accumulate(+, vec)

# Broadcasting (dot syntax)
vec .+ 1
vec .* 2
sin.(vec)
vec .^ 2
vec1 .+ vec2
broadcast(+, vec, 1)
@. sin(vec) + cos(vec)

# Linear algebra
using LinearAlgebra
mat * mat       # Matrix multiplication
mat'            # Transpose/adjoint
transpose(mat)  # Transpose
det(mat)        # Determinant
inv(mat)        # Inverse
tr(mat)         # Trace
eigvals(mat)    # Eigenvalues
eigvecs(mat)    # Eigenvectors
svd(mat)        # SVD
qr(mat)         # QR decomposition
lu(mat)         # LU decomposition
cholesky(mat'mat)  # Cholesky
norm(vec)       # Norm
dot(vec, vec)   # Dot product
cross([1,2,3], [4,5,6])  # Cross product

# ==============================================================================
# Tuples and Named Tuples
# ==============================================================================

# Tuples (immutable)
tup = (1, 2, 3)
tup_mixed = (1, "hello", 3.14)
tup_single = (42,)  # Note the comma
tup_empty = ()

# Tuple indexing
tup[1]
tup[end]
tup[2:3]

# Tuple unpacking
a, b, c = tup
first, rest... = tup

# Named tuples
nt = (name="Alice", age=30, city="Seattle")
nt.name
nt[:age]
keys(nt)
values(nt)

# ==============================================================================
# Dictionaries
# ==============================================================================

# Create dictionary
dict = Dict("one" => 1, "two" => 2, "three" => 3)
dict_typed = Dict{String, Int64}()
dict_from_pairs = Dict([("a", 1), ("b", 2)])
dict_comprehension = Dict(x => x^2 for x in 1:5)

# Dictionary operations
dict["one"]         # Access
dict["four"] = 4    # Add/update
get(dict, "five", 0)  # Get with default
get!(dict, "five", 5) # Get or create
delete!(dict, "five") # Delete
pop!(dict, "four")    # Pop
haskey(dict, "one")   # Check key

# Dictionary iteration
keys(dict)
values(dict)
pairs(dict)
for (k, v) in dict
    println("$k => $v")
end

# Merge dictionaries
merge(dict, Dict("four" => 4))
merge!(dict, Dict("five" => 5))

# ==============================================================================
# Sets
# ==============================================================================

# Create set
set = Set([1, 2, 3, 4, 5])
set_typed = Set{Int64}()
set_from_array = Set([1, 2, 2, 3, 3, 3])  # Duplicates removed

# Set operations
push!(set, 6)
pop!(set, 6)
delete!(set, 5)
in(3, set)
3 ∈ set  # Unicode ∈

# Set algebra
set1 = Set([1, 2, 3])
set2 = Set([2, 3, 4])
union(set1, set2)           # ∪
intersect(set1, set2)       # ∩
setdiff(set1, set2)         # \
symdiff(set1, set2)         # Symmetric difference
issubset(set1, set2)        # ⊆
issetequal(set1, set2)

# ==============================================================================
# Control Flow
# ==============================================================================

# If-else
x = 10
if x > 0
    println("Positive")
elseif x < 0
    println("Negative")
else
    println("Zero")
end

# Ternary operator
result = x > 0 ? "Positive" : "Non-positive"

# Short-circuit evaluation
x > 0 && println("Positive")
x < 0 || println("Not negative")

# For loops
for i in 1:5
    println(i)
end

for i ∈ 1:5  # Unicode ∈
    println(i)
end

for (i, val) in enumerate([10, 20, 30])
    println("$i: $val")
end

for (k, v) in pairs(dict)
    println("$k => $v")
end

for i in 1:3, j in 1:3
    println("($i, $j)")
end

# While loops
count = 0
while count < 5
    println(count)
    count += 1
end

# Loop control
for i in 1:10
    i == 3 && continue
    i == 8 && break
    println(i)
end

# Comprehension with condition
[x for x in 1:10 if x % 2 == 0]

# ==============================================================================
# Functions
# ==============================================================================

# Basic function
function greet(name)
    return "Hello, $name!"
end

# Compact form (assignment form)
square(x) = x^2

# Anonymous functions
f = x -> x^2
g = (x, y) -> x + y
h = function(x)
    x^2
end

# Multiple return values
function minmax(arr)
    return minimum(arr), maximum(arr)
end
lo, hi = minmax([1, 5, 3, 2, 4])

# Default arguments
function greet(name, greeting="Hello")
    return "$greeting, $name!"
end

# Keyword arguments
function create_user(; name, age, city="Unknown")
    return (name=name, age=age, city=city)
end
user = create_user(name="Alice", age=30)

# Positional and keyword arguments
function plot(x, y; color="blue", linewidth=1)
    println("Plotting with $color, width=$linewidth")
end

# Varargs
function sum_all(args...)
    return sum(args)
end
sum_all(1, 2, 3, 4, 5)

# Splatting
arr = [1, 2, 3]
sum_all(arr...)

# Keyword varargs
function config(; kwargs...)
    for (k, v) in kwargs
        println("$k = $v")
    end
end
config(debug=true, verbose=false)

# Function with type annotations
function typed_add(x::Int64, y::Int64)::Int64
    return x + y
end

# Mutating functions (convention: end with !)
function double!(arr)
    arr .*= 2
    return arr
end

# Do-block syntax
map(1:5) do x
    x^2
end

filter(1:10) do x
    x % 2 == 0
end

# Function composition
f ∘ g  # Compose f and g
(sin ∘ cos)(π)

# Function piping
1:10 |> collect |> sum

# Broadcasting functions
square.(1:5)

# ==============================================================================
# Multiple Dispatch
# ==============================================================================

# Generic function with multiple methods
function area(shape)
    error("Unknown shape")
end

function area(r::Float64)  # Circle
    return π * r^2
end

function area(l::Float64, w::Float64)  # Rectangle
    return l * w
end

function area(s::Float64, ::Val{:square})  # Square
    return s^2
end

# Parametric method
function identity_typed(x::T) where T
    return x
end

function first_and_type(arr::Vector{T}) where T
    return arr[1], T
end

# Method with constraints
function compare(x::T, y::T) where T <: Number
    return x > y ? x : y
end

# Check methods
methods(area)

# ==============================================================================
# Types
# ==============================================================================

# Abstract types
abstract type Shape end
abstract type Animal end

# Concrete types (structs)
struct Point
    x::Float64
    y::Float64
end

# Mutable struct
mutable struct MutablePoint
    x::Float64
    y::Float64
end

# Struct with default constructor
p = Point(1.0, 2.0)
p.x
p.y

# Mutable modification
mp = MutablePoint(1.0, 2.0)
mp.x = 3.0

# Parametric types
struct Container{T}
    value::T
end
int_container = Container(42)
str_container = Container("hello")

# Parametric with constraints
struct NumberContainer{T <: Number}
    value::T
end

# Inheritance with abstract types
abstract type Animal end

struct Dog <: Animal
    name::String
    age::Int
end

struct Cat <: Animal
    name::String
    age::Int
end

# Inner constructor
struct PositiveInt
    value::Int
    function PositiveInt(v)
        v > 0 || error("Value must be positive")
        new(v)
    end
end

# Outer constructor
function Point(x::Int, y::Int)
    return Point(Float64(x), Float64(y))
end

# Type aliases
const IntVector = Vector{Int64}
const StringDict = Dict{String, String}

# Union types
IntOrString = Union{Int, String}

# Type queries
typeof(42)
typeof(3.14)
supertype(Int64)
subtypes(Number)
isa(42, Int)
42 isa Int

# ==============================================================================
# Modules
# ==============================================================================

module MyModule

export greet, MyType

# Constants
const VERSION = "1.0.0"

# Types
struct MyType
    value::Int
end

# Functions
function greet(name)
    return "Hello, $name from MyModule!"
end

# Private function (not exported)
function _helper()
    return "I'm private"
end

end  # module

# Using modules
using .MyModule
using .MyModule: greet
import .MyModule
import .MyModule: greet as my_greet

# Standard library modules
using LinearAlgebra
using Statistics
using Random
using Dates
using Printf

# ==============================================================================
# Macros
# ==============================================================================

# Using macros
@show x
@info "Information message"
@warn "Warning message"
@error "Error message"
@debug "Debug message"
@assert x > 0 "x must be positive"
@time sum(1:1000000)
@elapsed sum(1:1000000)
@allocated sum(1:1000000)
@inbounds arr[1]
@simd for i in 1:length(arr)
    arr[i] *= 2
end
@inline function fast_func(x)
    x^2
end
@noinline function slow_func(x)
    x^2
end
@generated function gen_func(x)
    :(x^2)
end

# Define simple macro
macro sayhello()
    return :(println("Hello, World!"))
end
@sayhello

# Macro with arguments
macro twice(expr)
    quote
        $(esc(expr))
        $(esc(expr))
    end
end
@twice println("Hello")

# Macro with interpolation
macro debug(var)
    varname = string(var)
    quote
        println($varname, " = ", $(esc(var)))
    end
end
@debug x

# String macros
r"regex"              # Regex
b"bytes"              # Byte array
v"1.0"                # Version
raw"raw string"       # Raw string
html"<b>bold</b>"     # HTML (if defined)
sql"SELECT * FROM t"  # SQL (if defined)

# ==============================================================================
# Metaprogramming
# ==============================================================================

# Symbols
sym = :hello
sym2 = Symbol("world")

# Expressions
expr = :(1 + 2)
expr2 = quote
    x = 1
    y = 2
    x + y
end

# Expression manipulation
dump(expr)
typeof(expr)
expr.head
expr.args

# Evaluate expression
eval(expr)
eval(:(x = 42))

# Interpolation in expressions
a = 5
interpolated = :($a + 1)
eval(interpolated)

# Parse string to expression
parsed = Meta.parse("1 + 2")

# Expression to string
string(expr)

# ==============================================================================
# Exception Handling
# ==============================================================================

# Try-catch
try
    error("Something went wrong")
catch e
    println("Caught error: $e")
finally
    println("Cleanup")
end

# Catch specific exception types
try
    arr = [1, 2, 3]
    arr[10]
catch e
    if e isa BoundsError
        println("Index out of bounds")
    elseif e isa MethodError
        println("Method error")
    else
        rethrow(e)
    end
end

# Throwing exceptions
function divide(a, b)
    b == 0 && throw(DivideError())
    return a / b
end

# Custom exceptions
struct MyError <: Exception
    msg::String
end
Base.showerror(io::IO, e::MyError) = print(io, "MyError: ", e.msg)

# Error types
# ArgumentError
# BoundsError
# DivideError
# DomainError
# EOFError
# ErrorException
# InexactError
# InterruptException
# KeyError
# MethodError
# OutOfMemoryError
# OverflowError
# StackOverflowError
# SystemError
# TypeError
# UndefRefError
# UndefVarError

# ==============================================================================
# I/O
# ==============================================================================

# Print functions
print("No newline")
println("With newline")
printstyled("Colored", color=:red)
@printf("Formatted: %d, %.2f, %s\n", 42, 3.14, "hello")
@sprintf("To string: %d", 42)

# Read from stdin
# line = readline()
# lines = readlines()

# File I/O
# Writing
open("output.txt", "w") do io
    write(io, "Hello, File!")
    println(io, "Another line")
end

# Reading
# content = read("file.txt", String)
# lines = readlines("file.txt")
# open("file.txt") do io
#     for line in eachline(io)
#         println(line)
#     end
# end

# File operations
# isfile("path")
# isdir("path")
# ispath("path")
# readdir(".")
# pwd()
# cd("path")
# mkdir("dir")
# mkpath("a/b/c")
# rm("file")
# cp("src", "dst")
# mv("src", "dst")
# filesize("file")
# mtime("file")

# Serialization
using Serialization
# serialize("data.jls", data)
# data = deserialize("data.jls")

# JSON
using JSON
# json_str = JSON.json(dict)
# parsed = JSON.parse(json_str)

# ==============================================================================
# Dates and Times
# ==============================================================================

using Dates

# Current date/time
today_date = today()
now_datetime = now()
utc_now = now(UTC)

# Create dates
d = Date(2024, 1, 15)
dt = DateTime(2024, 1, 15, 12, 30, 45)
t = Time(12, 30, 45)

# Parse dates
Date("2024-01-15")
DateTime("2024-01-15T12:30:45")
Date("15/01/2024", dateformat"dd/mm/yyyy")

# Date components
year(d)
month(d)
day(d)
dayofweek(d)
dayname(d)
monthname(d)
dayofyear(d)
week(d)
quarter(d)

# DateTime components
hour(dt)
minute(dt)
second(dt)
millisecond(dt)

# Date arithmetic
d + Day(7)
d - Month(1)
d + Year(1)
dt + Hour(3)
dt - Minute(30)

# Date ranges
Date(2024, 1, 1):Day(1):Date(2024, 1, 31)
Date(2024, 1, 1):Month(1):Date(2024, 12, 1)

# Duration
d2 - d  # Returns Day
dt - DateTime(2024, 1, 1, 0, 0, 0)  # Returns Millisecond

# Formatting
Dates.format(d, "yyyy-mm-dd")
Dates.format(dt, "yyyy-mm-dd HH:MM:SS")

# ==============================================================================
# Random Numbers
# ==============================================================================

using Random

# Basic random
rand()              # Uniform [0, 1)
rand(5)             # Vector of 5
rand(3, 3)          # 3x3 matrix
rand(1:10)          # Random integer
rand(1:10, 5)       # Vector of random integers
rand(["a", "b", "c"])  # Random element

# Other distributions
randn()             # Standard normal
randn(5)
randexp()           # Exponential
randexp(5)

# Seeding
Random.seed!(42)

# Shuffling
shuffle([1, 2, 3, 4, 5])
shuffle!([1, 2, 3, 4, 5])

# Sampling
sample(1:10, 5)
sample(1:10, 5, replace=false)

# ==============================================================================
# Parallel and Concurrent Programming
# ==============================================================================

# Threads
Threads.nthreads()
Threads.threadid()

Threads.@threads for i in 1:10
    println("Thread $(Threads.threadid()): $i")
end

# Atomic operations
counter = Threads.Atomic{Int}(0)
Threads.atomic_add!(counter, 1)

# Locks
lock = ReentrantLock()
lock(lock) do
    # Critical section
end

# Tasks (coroutines)
task = @task begin
    for i in 1:5
        println(i)
        sleep(0.1)
    end
end
schedule(task)
wait(task)

# Async
@async begin
    sleep(1)
    println("Done")
end

# Channels
ch = Channel{Int}(10)
put!(ch, 42)
take!(ch)
close(ch)

# Producer-consumer
function producer(ch)
    for i in 1:5
        put!(ch, i)
    end
    close(ch)
end

function consumer(ch)
    for item in ch
        println(item)
    end
end

# Distributed computing
# using Distributed
# addprocs(4)
# @distributed for i in 1:100
#     # parallel work
# end
# pmap(x -> x^2, 1:100)

# ==============================================================================
# Performance and Optimization
# ==============================================================================

# Type stability
function unstable(x)
    if x > 0
        return x
    else
        return "negative"
    end
end

function stable(x)::Float64
    if x > 0
        return Float64(x)
    else
        return -1.0
    end
end

# Performance macros
@time sum(1:1000000)
@timev sum(1:1000000)
@elapsed sum(1:1000000)
@allocated sum(1:1000000)
@btime sum(1:1000000)  # From BenchmarkTools

# Code introspection
@code_lowered sum(1:10)
@code_typed sum(1:10)
@code_llvm sum(1:10)
@code_native sum(1:10)
@code_warntype sum(1:10)

# Memory layout
sizeof(Int64)
sizeof([1, 2, 3])
Base.summarysize([1, 2, 3])

# ==============================================================================
# Interfaces and Protocols
# ==============================================================================

# Iteration interface
struct Squares
    n::Int
end

Base.iterate(s::Squares, state=1) = state > s.n ? nothing : (state^2, state+1)
Base.length(s::Squares) = s.n

for x in Squares(5)
    println(x)
end

# Indexing interface
struct MyArray
    data::Vector{Int}
end

Base.getindex(a::MyArray, i) = a.data[i]
Base.setindex!(a::MyArray, v, i) = (a.data[i] = v)
Base.length(a::MyArray) = length(a.data)
Base.size(a::MyArray) = (length(a.data),)

# Comparison interface
Base.:(==)(p1::Point, p2::Point) = p1.x == p2.x && p1.y == p2.y
Base.hash(p::Point, h::UInt) = hash(p.x, hash(p.y, h))
Base.isless(p1::Point, p2::Point) = p1.x < p2.x || (p1.x == p2.x && p1.y < p2.y)

# Show/display
Base.show(io::IO, p::Point) = print(io, "Point($(p.x), $(p.y))")
Base.show(io::IO, ::MIME"text/plain", p::Point) = print(io, "Point:\n  x = $(p.x)\n  y = $(p.y)")

# ==============================================================================
# Documentation
# ==============================================================================

"""
    greet(name)

Greet someone by their name.

# Arguments
- `name::String`: The name of the person to greet.

# Returns
- `String`: A greeting message.

# Examples
```julia
julia> greet("Alice")
"Hello, Alice!"
```

See also: [`farewell`](@ref)
"""
function greet(name::String)
    return "Hello, $name!"
end

# Access documentation
# ?greet
# @doc greet

# ==============================================================================
# End of Julia Sample
# ==============================================================================
