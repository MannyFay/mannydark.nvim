#!/usr/bin/env lua
--- Comprehensive Lua language sample demonstrating all syntax features.
-- Lua is a lightweight, high-level, multi-paradigm programming language
-- designed primarily for embedded use in applications.
--
-- @module sample
-- @author Sample Author
-- @version 1.0.0

-- ============================================================================
-- Module Setup
-- ============================================================================

local M = {}

-- ============================================================================
-- Constants
-- ============================================================================

local MAX_BUFFER_SIZE = 1024
local PI = 3.14159265358979323846
local GREETING = "Hello, Lua!"

-- Read-only table (using metatable)
local Constants = setmetatable({
    VERSION = "1.0.0",
    AUTHOR = "Sample Author",
}, {
    __newindex = function()
        error("Cannot modify constants")
    end,
    __metatable = false,
})

-- ============================================================================
-- Basic Types and Literals
-- ============================================================================

-- Nil
local nil_value = nil

-- Boolean
local bool_true = true
local bool_false = false

-- Numbers
local integer = 42
local float = 3.14
local negative = -42
local hex = 0xDEADBEEF
local scientific = 1.23e-4
local large = 1e308

-- Strings
local single_quote = "Hello"
local double_quote = "Hello"
local escaped = "Tab:\t Newline:\n Quote:\""
local long_string = [[
This is a long string
that spans multiple lines
without escape characters
]]

local long_string_level = [==[
This is a level 2 long string
with [[brackets]] inside
]==]

-- Concatenation
local concat = "Hello" .. ", " .. "World!"
local with_number = "Value: " .. tostring(42)

-- ============================================================================
-- Tables (Arrays, Dictionaries, Objects)
-- ============================================================================

-- Array-like table (1-indexed)
local array = {1, 2, 3, 4, 5}
local first = array[1]
local length = #array

-- Dictionary-like table
local dict = {
    name = "Alice",
    age = 30,
    email = "alice@example.com",
}
local name = dict.name
local age = dict["age"]

-- Mixed table
local mixed = {
    "first",                    -- [1]
    "second",                   -- [2]
    key = "value",
    ["another-key"] = "another-value",
    [true] = "boolean key",
    [42] = "numeric key",
}

-- Nested tables
local nested = {
    person = {
        name = "Bob",
        address = {
            city = "New York",
            zip = "10001",
        },
    },
    items = {1, 2, 3},
}

-- Access nested
local city = nested.person.address.city
local zip = nested.person.address["zip"]

-- Table operations
table.insert(array, 6)
table.insert(array, 1, 0)  -- Insert at beginning
local removed = table.remove(array)
local sorted = {5, 2, 8, 1, 9}
table.sort(sorted)
local reversed = {5, 4, 3, 2, 1}
table.sort(reversed, function(a, b) return a > b end)

-- ============================================================================
-- Functions
-- ============================================================================

-- Basic function
local function add(a, b)
    return a + b
end

-- Function expression
local multiply = function(a, b)
    return a * b
end

-- Multiple return values
local function divmod(a, b)
    return math.floor(a / b), a % b
end

local quotient, remainder = divmod(17, 5)

-- Variadic function
local function sum(...)
    local total = 0
    for _, v in ipairs({...}) do
        total = total + v
    end
    return total
end

-- Named arguments via table
local function greet(opts)
    opts = opts or {}
    local name = opts.name or "World"
    local greeting = opts.greeting or "Hello"
    return greeting .. ", " .. name .. "!"
end

-- Recursive function
local function factorial(n)
    if n <= 1 then
        return 1
    else
        return n * factorial(n - 1)
    end
end

-- Tail-recursive
local function factorial_tail(n, acc)
    acc = acc or 1
    if n <= 1 then
        return acc
    else
        return factorial_tail(n - 1, n * acc)
    end
end

-- Closures
local function make_counter()
    local count = 0
    return function()
        count = count + 1
        return count
    end
end

local function make_multiplier(factor)
    return function(x)
        return x * factor
    end
end

-- Higher-order functions
local function apply_twice(f, x)
    return f(f(x))
end

local function map(tbl, f)
    local result = {}
    for i, v in ipairs(tbl) do
        result[i] = f(v)
    end
    return result
end

local function filter(tbl, predicate)
    local result = {}
    for _, v in ipairs(tbl) do
        if predicate(v) then
            table.insert(result, v)
        end
    end
    return result
end

local function reduce(tbl, f, initial)
    local acc = initial
    for _, v in ipairs(tbl) do
        acc = f(acc, v)
    end
    return acc
end

-- ============================================================================
-- Control Flow
-- ============================================================================

local function control_flow_examples()
    local value = 42

    -- if-then-else
    if value > 0 then
        print("Positive")
    elseif value < 0 then
        print("Negative")
    else
        print("Zero")
    end

    -- Ternary-like (using and/or)
    local result = value > 0 and "positive" or "non-positive"

    -- Numeric for loop
    for i = 1, 10 do
        print(i)
    end

    -- With step
    for i = 10, 1, -1 do
        print(i)
    end

    -- Generic for (ipairs for arrays)
    local arr = {"a", "b", "c"}
    for i, v in ipairs(arr) do
        print(i, v)
    end

    -- Generic for (pairs for tables)
    local tbl = {a = 1, b = 2, c = 3}
    for k, v in pairs(tbl) do
        print(k, v)
    end

    -- While loop
    local counter = 0
    while counter < 5 do
        counter = counter + 1
    end

    -- Repeat-until
    counter = 0
    repeat
        counter = counter + 1
    until counter >= 5

    -- Break
    for i = 1, 100 do
        if i > 10 then
            break
        end
        print(i)
    end

    -- Goto (Lua 5.2+)
    for i = 1, 10 do
        for j = 1, 10 do
            if i * j > 50 then
                goto outer
            end
        end
    end
    ::outer::
end

-- ============================================================================
-- Metatables and Metamethods
-- ============================================================================

-- Point class using metatables
local Point = {}
Point.__index = Point

function Point.new(x, y, z)
    local self = setmetatable({}, Point)
    self.x = x or 0
    self.y = y or 0
    self.z = z or 0
    return self
end

function Point:distance(other)
    local dx = self.x - other.x
    local dy = self.y - other.y
    local dz = self.z - other.z
    return math.sqrt(dx^2 + dy^2 + dz^2)
end

function Point:__add(other)
    return Point.new(
        self.x + other.x,
        self.y + other.y,
        self.z + other.z
    )
end

function Point:__sub(other)
    return Point.new(
        self.x - other.x,
        self.y - other.y,
        self.z - other.z
    )
end

function Point:__mul(scalar)
    return Point.new(
        self.x * scalar,
        self.y * scalar,
        self.z * scalar
    )
end

function Point:__unm()
    return Point.new(-self.x, -self.y, -self.z)
end

function Point:__eq(other)
    return self.x == other.x and self.y == other.y and self.z == other.z
end

function Point:__lt(other)
    local origin = Point.new(0, 0, 0)
    return self:distance(origin) < other:distance(origin)
end

function Point:__tostring()
    return string.format("Point(%g, %g, %g)", self.x, self.y, self.z)
end

function Point.origin()
    return Point.new(0, 0, 0)
end

-- Shape class (abstract-like)
local Shape = {}
Shape.__index = Shape

function Shape.new(name)
    local self = setmetatable({}, Shape)
    self.name = name or "Shape"
    self.color = "black"
    return self
end

function Shape:area()
    error("Subclass must implement area()")
end

function Shape:perimeter()
    error("Subclass must implement perimeter()")
end

function Shape:describe()
    return string.format("%s: area=%.2f, perimeter=%.2f",
        self.name, self:area(), self:perimeter())
end

function Shape:draw()
    print("Drawing " .. self.name .. " in " .. self.color)
end

-- Circle class (inheritance)
local Circle = setmetatable({}, {__index = Shape})
Circle.__index = Circle

function Circle.new(radius, color)
    local self = setmetatable(Shape.new("Circle"), Circle)
    self.radius = radius or 1
    self.color = color or "black"
    return self
end

function Circle:area()
    return PI * self.radius^2
end

function Circle:perimeter()
    return 2 * PI * self.radius
end

function Circle:draw()
    Shape.draw(self)
    print("  radius: " .. self.radius)
end

-- Rectangle class
local Rectangle = setmetatable({}, {__index = Shape})
Rectangle.__index = Rectangle

function Rectangle.new(width, height, color)
    local self = setmetatable(Shape.new("Rectangle"), Rectangle)
    self.width = width or 1
    self.height = height or 1
    self.color = color or "black"
    return self
end

function Rectangle:area()
    return self.width * self.height
end

function Rectangle:perimeter()
    return 2 * (self.width + self.height)
end

-- ============================================================================
-- Proxy Tables
-- ============================================================================

local function create_readonly_table(tbl)
    return setmetatable({}, {
        __index = tbl,
        __newindex = function()
            error("Table is read-only")
        end,
        __pairs = function()
            return pairs(tbl)
        end,
        __len = function()
            return #tbl
        end,
    })
end

local function create_default_table(default)
    return setmetatable({}, {
        __index = function()
            return default
        end,
    })
end

-- ============================================================================
-- Coroutines
-- ============================================================================

local function coroutine_examples()
    -- Producer-consumer pattern
    local producer = coroutine.create(function()
        for i = 1, 5 do
            coroutine.yield(i)
        end
    end)

    while coroutine.status(producer) ~= "dead" do
        local _, value = coroutine.resume(producer)
        if value then
            print("Produced: " .. value)
        end
    end

    -- Fibonacci generator
    local function fib_generator()
        local a, b = 0, 1
        while true do
            coroutine.yield(a)
            a, b = b, a + b
        end
    end

    local fib = coroutine.create(fib_generator)
    local first_10 = {}
    for i = 1, 10 do
        local _, value = coroutine.resume(fib)
        table.insert(first_10, value)
    end
    print("Fibonacci: " .. table.concat(first_10, ", "))

    -- Coroutine wrapper
    local wrapped = coroutine.wrap(function()
        for i = 1, 3 do
            coroutine.yield(i * 2)
        end
    end)

    print(wrapped())  -- 2
    print(wrapped())  -- 4
    print(wrapped())  -- 6
end

-- ============================================================================
-- Error Handling
-- ============================================================================

local function error_handling_examples()
    -- pcall (protected call)
    local status, result = pcall(function()
        error("Something went wrong")
    end)

    if not status then
        print("Error caught: " .. result)
    end

    -- xpcall with error handler
    local function error_handler(err)
        return "Handled: " .. tostring(err)
    end

    xpcall(function()
        error("Another error")
    end, error_handler)

    -- assert
    local function divide(a, b)
        assert(b ~= 0, "Division by zero")
        return a / b
    end

    local ok, res = pcall(divide, 10, 0)
    if not ok then
        print("Division failed: " .. res)
    end

    -- Custom error with traceback
    local function traced_error(msg)
        error(debug.traceback(msg, 2))
    end
end

-- ============================================================================
-- String Operations
-- ============================================================================

local function string_examples()
    local str = "Hello, World!"

    -- Length
    local len = #str
    local len2 = string.len(str)

    -- Substring
    local sub = string.sub(str, 1, 5)  -- "Hello"

    -- Upper/lower
    local upper = string.upper(str)
    local lower = string.lower(str)

    -- Find
    local start, finish = string.find(str, "World")

    -- Match (pattern matching)
    local matched = string.match(str, "(%w+)")  -- "Hello"

    -- Gmatch (iterator)
    for word in string.gmatch(str, "%w+") do
        print(word)
    end

    -- Gsub (replace)
    local replaced = string.gsub(str, "World", "Lua")

    -- Format
    local formatted = string.format("Value: %d, Float: %.2f, String: %s",
        42, 3.14, "test")

    -- Rep (repeat)
    local repeated = string.rep("-", 10)

    -- Reverse
    local reversed = string.reverse(str)

    -- Byte and char
    local byte = string.byte("A")
    local char = string.char(65)

    -- Pattern examples
    local email = "user@example.com"
    local user, domain = string.match(email, "(%w+)@(%w+%.%w+)")

    print(formatted)
end

-- ============================================================================
-- File I/O
-- ============================================================================

local function file_io_examples()
    -- Write
    local file = io.open("test.txt", "w")
    if file then
        file:write("Hello, File!\n")
        file:write("Second line\n")
        file:close()
    end

    -- Read all
    file = io.open("test.txt", "r")
    if file then
        local content = file:read("*all")
        print(content)
        file:close()
    end

    -- Read line by line
    file = io.open("test.txt", "r")
    if file then
        for line in file:lines() do
            print("Line: " .. line)
        end
        file:close()
    end

    -- io.lines shortcut
    for line in io.lines("test.txt") do
        print(line)
    end

    -- Append
    file = io.open("test.txt", "a")
    if file then
        file:write("Appended line\n")
        file:close()
    end

    -- Delete
    os.remove("test.txt")
end

-- ============================================================================
-- Module Pattern
-- ============================================================================

-- Module definition
local MyModule = {}

MyModule.VERSION = "1.0.0"

local private_data = "secret"

local function private_function()
    return private_data
end

function MyModule.public_function()
    return "Public: " .. private_function()
end

function MyModule.greet(name)
    return "Hello, " .. name .. "!"
end

-- ============================================================================
-- Main
-- ============================================================================

-- Variable scoping
local global_like = "accessible in file"

do
    local block_scoped = "only in this block"
    print(block_scoped)
end

-- Main execution
print(GREETING)

-- Basic types
print("Integer:", integer)
print("Float:", float)
print("String:", single_quote)
print("Boolean:", bool_true)
print("Nil:", nil_value)

-- Functions
print("Add:", add(1, 2))
print("Sum:", sum(1, 2, 3, 4, 5))
print("Factorial:", factorial(5))

local div, mod = divmod(17, 5)
print("Divmod:", div, mod)

-- Closures
local counter = make_counter()
print("Counter:", counter(), counter(), counter())

local double = make_multiplier(2)
local triple = make_multiplier(3)
print("Double 5:", double(5))
print("Triple 5:", triple(5))

-- Higher-order
print("Apply twice:", apply_twice(function(x) return x + 1 end, 5))

local numbers = {1, 2, 3, 4, 5}
local squared = map(numbers, function(x) return x * x end)
print("Squared:", table.concat(squared, ", "))

local evens = filter(numbers, function(x) return x % 2 == 0 end)
print("Evens:", table.concat(evens, ", "))

local total = reduce(numbers, function(a, b) return a + b end, 0)
print("Total:", total)

-- Control flow
control_flow_examples()

-- OOP
local point = Point.new(1, 2, 3)
print("Point:", point)
print("Distance:", point:distance(Point.origin()))

local point2 = Point.new(4, 5, 6)
local sum_point = point + point2
print("Sum:", sum_point)

local circle = Circle.new(5, "blue")
circle:draw()
print("Area:", circle:area())

local rect = Rectangle.new(4, 6)
print(rect:describe())

-- Coroutines
coroutine_examples()

-- Error handling
error_handling_examples()

-- Strings
string_examples()

-- File I/O
-- file_io_examples()  -- Uncomment to test

-- Module
print("Module:", MyModule.public_function())
print("Greet:", MyModule.greet("Lua"))

-- Table operations
local t = {3, 1, 4, 1, 5, 9, 2, 6}
table.sort(t)
print("Sorted:", table.concat(t, ", "))

-- Math
print("Pi:", math.pi)
print("Sqrt:", math.sqrt(16))
print("Sin:", math.sin(math.pi / 2))
print("Random:", math.random(1, 100))

-- OS
print("Time:", os.time())
print("Date:", os.date("%Y-%m-%d %H:%M:%S"))
print("Clock:", os.clock())

-- Type checking
print("Type of 42:", type(42))
print("Type of "hello":", type("hello"))
print("Type of table:", type({}))
print("Type of function:", type(print))

print("Program completed successfully!")

-- Export module
M.Point = Point
M.Circle = Circle
M.Rectangle = Rectangle
M.add = add
M.multiply = multiply
M.factorial = factorial
M.map = map
M.filter = filter
M.reduce = reduce

return M
