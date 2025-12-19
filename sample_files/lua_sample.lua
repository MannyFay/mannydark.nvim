-------------------------------------------------------------------------------
-- Lua Sample File
-- Tests all highlight groups for the mannydark colorscheme
-------------------------------------------------------------------------------

--[[
  This is a multi-line comment (luaCommentLong)
  Used to test long comment highlighting
]]

-- TODO: This should be bold red (Todo)
-- FIXME: This too (Todo)
-- NOTE: This should be highlighted (comment.note)
-- HACK: And this (comment.warning)


-------------------------------------------------------------------------------
-- Keywords & Statements
-------------------------------------------------------------------------------

-- local keyword (luaLocal, @keyword)
local my_variable = "test"

-- function keyword (luaFunction, @keyword.function)
local function my_function()
  return true
end

-- Anonymous function
local anonymous = function() end

-- Conditional keywords (luaCond, @keyword.conditional)
if my_variable then
  -- then keyword
  print("truthy")
elseif my_variable == "test" then
  -- elseif keyword
  print("is test")
else
  -- else keyword
  print("falsy")
end  -- end keyword (luaEnd)

-- Loop keywords (luaRepeat, @keyword.repeat)
for i = 1, 10 do
  -- for, do keywords
  if i > 5 then
    break  -- break keyword (luaBreak)
  end
end

-- while loop
while false do
  -- while keyword
end

-- repeat until loop
repeat
  -- repeat keyword
until true  -- until keyword

-- return keyword (luaReturn, @keyword.return)
local function returns_value()
  return 42
end

-- goto and labels (luaGoto, luaLabel, @label)
::my_label::
-- goto my_label  -- uncomment to test goto


-------------------------------------------------------------------------------
-- Operators
-------------------------------------------------------------------------------

-- Word operators (luaOperator - blue, @keyword.operator)
local a = true and false   -- and
local b = true or false    -- or
local c = not true         -- not

-- Symbol operators (luaSymbolOperator - white, @operator)
local sum = 1 + 2          -- addition
local diff = 5 - 3         -- subtraction
local prod = 4 * 2         -- multiplication
local quot = 8 / 2         -- division
local mod = 10 % 3         -- modulo
local pow = 2 ^ 8          -- exponentiation
local floor_div = 7 // 2   -- floor division (Lua 5.3+)

-- Relational operators
local eq = (1 == 1)        -- equal
local neq = (1 ~= 2)       -- not equal (luaNotEqOperator)
local lt = (1 < 2)         -- less than
local gt = (2 > 1)         -- greater than
local lte = (1 <= 1)       -- less than or equal
local gte = (2 >= 2)       -- greater than or equal

-- String concatenation (luaConcatOperator)
local greeting = "Hello" .. " " .. "World"

-- Length operator (luaLengthOperator)
local len = #greeting

-- Bitwise operators (Lua 5.3+)
local band = 5 & 3         -- bitwise and
local bor = 5 | 3          -- bitwise or
local bxor = 5 ~ 3         -- bitwise xor
local bnot = ~5            -- bitwise not
local lshift = 1 << 4      -- left shift
local rshift = 16 >> 2     -- right shift


-------------------------------------------------------------------------------
-- Strings
-------------------------------------------------------------------------------

-- Regular strings (luaString, @string)
local single_quoted = 'single quotes'
local double_quoted = "double quotes"

-- Escape sequences (luaStringSpecial, @string.escape)
local with_escapes = "tab:\there\nnewline"
local with_unicode = "unicode: \u{1F600}"
local with_hex = "hex: \x41\x42\x43"

-- Long strings (luaStringLong)
local long_string = [[
This is a long string.
It can span multiple lines.
No escape sequences: \n \t
]]

-- Long string with equals (for nesting)
local nested_long = [==[
This can contain [[brackets]]
]==]


-------------------------------------------------------------------------------
-- Numbers
-------------------------------------------------------------------------------

-- Integers (luaNumber, @number)
local int = 42
local negative = -17
local big = 1000000

-- Floats (luaFloat, @number.float)
local float = 3.14159
local scientific = 6.022e23
local small = 1.6e-19

-- Hexadecimal (luaHexNumber)
local hex = 0xFF
local hex_lower = 0xabcdef
local hex_float = 0x1.5p10  -- hex float (Lua 5.3+)


-------------------------------------------------------------------------------
-- Booleans & Nil
-------------------------------------------------------------------------------

-- Booleans (luaBoolean, @boolean) - should be BLUE
local is_true = true
local is_false = false

-- Nil (luaNil, @constant.builtin)
local nothing = nil

-- Boolean in conditions
if true then
  print("true is blue")
end

if false then
  print("false is also blue")
end


-------------------------------------------------------------------------------
-- Constants
-------------------------------------------------------------------------------

-- Global constants (luaConstant, @constant)
local CONSTANT_VALUE = 100
local MAX_SIZE = 1024
local PI = 3.14159265359


-------------------------------------------------------------------------------
-- Functions
-------------------------------------------------------------------------------

-- Function definition (luaFuncName, @function)
local function calculate_sum(a, b)
  return a + b
end

-- Function call (luaFuncCall, @function.call)
local result = calculate_sum(10, 20)
print(result)

-- Built-in functions (luaFunc, @function.builtin)
print("Hello")
type(result)
tostring(42)
tonumber("123")
pairs({})
ipairs({})
next({})
select(1, "a", "b", "c")
rawget({}, "key")
rawset({}, "key", "value")
rawequal({}, {})
pcall(function() end)
xpcall(function() end, function() end)
error("error message")
assert(true, "assertion failed")
collectgarbage()
loadstring("return 1")
loadfile("file.lua")
dofile("file.lua")
require("module")
setmetatable({}, {})
getmetatable({})

-- Method call syntax (@function.method.call)
local str = "hello"
local upper = str:upper()
local sub = str:sub(1, 3)
local find = str:find("ell")

-- Variadic functions
local function variadic(...)
  local args = {...}
  return select("#", ...)
end

-- Function with multiple returns
local function multi_return()
  return 1, 2, 3
end

local x, y, z = multi_return()


-------------------------------------------------------------------------------
-- Tables
-------------------------------------------------------------------------------

-- Table constructor (luaTableConstructor, @constructor)
local empty_table = {}

-- Table with array part
local array = { 1, 2, 3, 4, 5 }

-- Table with hash part (luaTableField, @property)
local hash = {
  name = "John",
  age = 30,
  active = true,
}

-- Mixed table
local mixed = {
  "array_element",
  key = "value",
  ["string-key"] = "with dashes",
  [1] = "explicit index",
}

-- Nested tables
local nested = {
  inner = {
    deep = {
      value = 42
    }
  }
}

-- Table access
local value = hash.name          -- dot notation
local value2 = hash["age"]       -- bracket notation
local deep = nested.inner.deep.value

-- Special tables (luaSpecialTable, @variable.builtin)
local global_table = _G
local env_table = _ENV
local version = _VERSION


-------------------------------------------------------------------------------
-- Metatables & Metamethods
-------------------------------------------------------------------------------

-- Metatable definition
local mt = {
  -- Metamethods (luaMetaMethod)
  __index = function(t, k)
    return "default"
  end,

  __newindex = function(t, k, v)
    rawset(t, k, v)
  end,

  __call = function(t, ...)
    return "called"
  end,

  __tostring = function(t)
    return "MyObject"
  end,

  __add = function(a, b)
    return a.value + b.value
  end,

  __sub = function(a, b) return a.value - b.value end,
  __mul = function(a, b) return a.value * b.value end,
  __div = function(a, b) return a.value / b.value end,
  __mod = function(a, b) return a.value % b.value end,
  __pow = function(a, b) return a.value ^ b.value end,
  __unm = function(a) return -a.value end,
  __concat = function(a, b) return tostring(a) .. tostring(b) end,
  __len = function(a) return #a.items end,
  __eq = function(a, b) return a.value == b.value end,
  __lt = function(a, b) return a.value < b.value end,
  __le = function(a, b) return a.value <= b.value end,
  __gc = function(t) print("garbage collected") end,
  __mode = "kv",
  __metatable = "protected",
}

local obj = setmetatable({}, mt)


-------------------------------------------------------------------------------
-- Modules & Require
-------------------------------------------------------------------------------

-- Require (luaSpecialValue, @keyword.import)
local json = require("json")
local utils = require("my_project.utils")
local sub_module = require("my_project.sub.module")

-- Module pattern
local my_module = {}

function my_module.public_function()
  return "public"
end

local function private_function()
  return "private"
end

return my_module


-------------------------------------------------------------------------------
-- Error Handling
-------------------------------------------------------------------------------

-- Protected call
local success, err = pcall(function()
  error("something went wrong")
end)

-- Extended protected call with error handler
local success2, result = xpcall(
  function()
    return risky_operation()
  end,
  function(err)
    return debug.traceback(err)
  end
)

-- Assert
assert(type(result) == "number", "Expected number")


-------------------------------------------------------------------------------
-- Coroutines
-------------------------------------------------------------------------------

-- Coroutine creation (@keyword.coroutine)
local co = coroutine.create(function()
  for i = 1, 10 do
    coroutine.yield(i)
  end
end)

-- Coroutine operations
coroutine.resume(co)
coroutine.status(co)
coroutine.running()
coroutine.wrap(function() end)


-------------------------------------------------------------------------------
-- String Patterns (Regex-like)
-------------------------------------------------------------------------------

-- Pattern matching (@string.regexp)
local pattern = "^%s*(%w+)%s*=%s*(.-)%s*$"
local match = string.match("  key = value  ", pattern)
local gmatch_iter = string.gmatch("a1b2c3", "%a%d")
local gsub_result = string.gsub("hello", "l", "L")
local find_pos = string.find("hello world", "world")


-------------------------------------------------------------------------------
-- OOP Pattern (Class-like)
-------------------------------------------------------------------------------

-- Class definition using metatables
local Animal = {}
Animal.__index = Animal

function Animal:new(name)
  local instance = setmetatable({}, self)
  instance.name = name
  return instance
end

function Animal:speak()
  print(self.name .. " makes a sound")
end

-- Inheritance
local Dog = setmetatable({}, { __index = Animal })
Dog.__index = Dog

function Dog:new(name, breed)
  local instance = Animal.new(self, name)
  instance.breed = breed
  return instance
end

function Dog:speak()
  print(self.name .. " barks!")
end

local my_dog = Dog:new("Buddy", "Labrador")
my_dog:speak()


-------------------------------------------------------------------------------
-- LuaDoc / EmmyLua Annotations (if supported)
-------------------------------------------------------------------------------

---@class Person
---@field name string The person's name
---@field age number The person's age
---@field active boolean Whether the person is active

---@param name string The name parameter
---@param age number The age parameter
---@return Person person The created person
local function create_person(name, age)
  return {
    name = name,
    age = age,
    active = true,
  }
end

---@type string[]
local string_array = { "a", "b", "c" }

---@type table<string, number>
local string_to_number = {
  one = 1,
  two = 2,
}

---@alias Callback fun(data: any): boolean

---@type Callback
local my_callback = function(data)
  return data ~= nil
end


-------------------------------------------------------------------------------
-- Neovim Specific (if applicable)
-------------------------------------------------------------------------------

-- vim.* namespace (@module.builtin, @variable.builtin)
-- vim.api.nvim_* functions
-- vim.fn.* vimscript functions
-- vim.opt.* options
-- vim.keymap.set()
-- vim.cmd()
-- vim.notify()

-- Example (commented to avoid errors outside Neovim):
-- local bufnr = vim.api.nvim_get_current_buf()
-- local lines = vim.api.nvim_buf_get_lines(bufnr, 0, -1, false)
-- vim.opt.number = true
-- vim.keymap.set('n', '<leader>x', function() print("pressed") end)


-------------------------------------------------------------------------------
-- Punctuation & Delimiters
-------------------------------------------------------------------------------

-- Parentheses (luaParens, @punctuation.bracket)
local grouped = (1 + 2) * 3

-- Braces (luaBraces, @punctuation.bracket)
local tbl = { a = 1 }

-- Brackets (luaBrackets, @punctuation.bracket)
local arr = tbl["a"]

-- Comma (luaComma, @punctuation.delimiter)
local a1, b1, c1 = 1, 2, 3

-- Semicolon (luaSemiCol, @punctuation.delimiter)
local x1 = 1; local y1 = 2;

-- Colon (method call)
local s = "test":upper()

-- Dot (luaNoise, @punctuation.delimiter)
local pi = math.pi
