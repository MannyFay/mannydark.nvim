#!/usr/bin/env ruby
# frozen_string_literal: true

# encoding: utf-8

=begin
Comprehensive Ruby language sample demonstrating all syntax features.

Ruby is a dynamic, object-oriented programming language with
elegant syntax and powerful metaprogramming capabilities.

@author Sample Author
@version 1.0.0
=end

require 'date'
require 'json'
require 'set'
require 'singleton'
require 'forwardable'
require 'ostruct'
require 'benchmark'
require 'fiber'
require 'timeout'
require 'thread'

# ============================================================================
# Constants and Module Configuration
# ============================================================================

# Module for constants
module Constants
  MAX_BUFFER_SIZE = 1024
  PI = 3.14159265358979323846
  GREETING = 'Hello, Ruby!'

  # Freeze to prevent modification
  COLORS = %i[red green blue white black].freeze
  WEEKDAYS = %w[Monday Tuesday Wednesday Thursday Friday Saturday Sunday].freeze
end

# ============================================================================
# Modules (Mixins)
# ============================================================================

# Mixin for comparable functionality
module Comparable
  def <=>(other)
    raise NotImplementedError
  end

  def <(other)
    (self <=> other) < 0
  end

  def >(other)
    (self <=> other) > 0
  end

  def ==(other)
    (self <=> other).zero?
  end
end

# Mixin for logging
module Loggable
  def log(message)
    puts "[#{self.class.name}] #{message}"
  end

  def log_error(message)
    warn "[ERROR] #{message}"
  end
end

# Mixin for serialization
module Serializable
  def to_hash
    instance_variables.each_with_object({}) do |var, hash|
      hash[var.to_s.delete('@')] = instance_variable_get(var)
    end
  end

  def to_json(*_args)
    to_hash.to_json
  end
end

# ============================================================================
# Classes
# ============================================================================

# Point class with full implementation
class Point
  include Comparable
  include Serializable

  attr_accessor :x, :y, :z

  # Class variable
  @@instance_count = 0

  # Class instance variable
  @origin = nil

  class << self
    attr_reader :origin

    def create_origin
      @origin ||= new(0, 0, 0)
    end

    def instance_count
      @@instance_count
    end
  end

  def initialize(x = 0, y = 0, z = 0)
    @x = x.to_f
    @y = y.to_f
    @z = z.to_f
    @@instance_count += 1
  end

  def distance(other)
    dx = @x - other.x
    dy = @y - other.y
    dz = @z - other.z
    Math.sqrt(dx**2 + dy**2 + dz**2)
  end

  def +(other)
    Point.new(@x + other.x, @y + other.y, @z + other.z)
  end

  def -(other)
    Point.new(@x - other.x, @y - other.y, @z - other.z)
  end

  def *(scalar)
    Point.new(@x * scalar, @y * scalar, @z * scalar)
  end

  def -@
    Point.new(-@x, -@y, -@z)
  end

  def <=>(other)
    distance(Point.create_origin) <=> other.distance(Point.create_origin)
  end

  def to_s
    "Point(#{@x}, #{@y}, #{@z})"
  end

  def inspect
    "#<Point x=#{@x} y=#{@y} z=#{@z}>"
  end

  def to_a
    [@x, @y, @z]
  end

  def deconstruct
    [@x, @y, @z]
  end

  def deconstruct_keys(_keys)
    { x: @x, y: @y, z: @z }
  end
end

# Struct-based class
Person = Struct.new(:name, :age, :email, keyword_init: true) do
  def adult?
    age >= 18
  end

  def to_s
    "#{name} (#{age})"
  end
end

# Abstract base class
class Shape
  include Loggable

  attr_accessor :name, :color

  def initialize(name)
    @name = name
    @color = :black
  end

  def area
    raise NotImplementedError, "#{self.class} must implement #area"
  end

  def perimeter
    raise NotImplementedError, "#{self.class} must implement #perimeter"
  end

  def describe
    "#{@name}: area=#{area.round(2)}, perimeter=#{perimeter.round(2)}"
  end

  def draw
    log "Drawing #{@name} in #{@color}"
  end
end

# Circle class
class Circle < Shape
  attr_accessor :radius

  def initialize(radius, color = :black)
    super('Circle')
    @radius = radius
    @color = color
  end

  def area
    Constants::PI * @radius**2
  end

  def perimeter
    2 * Constants::PI * @radius
  end

  def draw
    super
    log "  radius: #{@radius}"
  end
end

# Rectangle class
class Rectangle < Shape
  attr_accessor :width, :height

  def initialize(width, height, color = :black)
    super('Rectangle')
    @width = width
    @height = height
    @color = color
  end

  def area
    @width * @height
  end

  def perimeter
    2 * (@width + @height)
  end
end

# Square class (inheritance)
class Square < Rectangle
  def initialize(side, color = :black)
    super(side, side, color)
    @name = 'Square'
  end

  def side
    @width
  end

  def side=(value)
    @width = value
    @height = value
  end
end

# ============================================================================
# Singleton Pattern
# ============================================================================

class AppConfig
  include Singleton

  attr_accessor :settings

  def initialize
    @settings = {}
  end

  def [](key)
    @settings[key]
  end

  def []=(key, value)
    @settings[key] = value
  end
end

# ============================================================================
# Delegation
# ============================================================================

class Container
  extend Forwardable

  def_delegators :@items, :size, :empty?, :first, :last, :each

  def initialize
    @items = []
  end

  def add(item)
    @items << item
    self
  end

  alias << add

  def [](index)
    @items[index]
  end

  def []=(index, value)
    @items[index] = value
  end

  include Enumerable
end

# ============================================================================
# Metaprogramming
# ============================================================================

class DynamicClass
  # Define methods dynamically
  %i[red green blue].each do |color|
    define_method("set_#{color}") do
      @color = color
    end

    define_method("#{color}?") do
      @color == color
    end
  end

  # Method missing
  def method_missing(method_name, *args, &block)
    if method_name.to_s.start_with?('get_')
      attr = method_name.to_s.sub('get_', '')
      instance_variable_get("@#{attr}")
    else
      super
    end
  end

  def respond_to_missing?(method_name, include_private = false)
    method_name.to_s.start_with?('get_') || super
  end

  # Class eval
  class_eval do
    def dynamic_method
      'I was defined with class_eval'
    end
  end

  # Instance eval
  def configure(&block)
    instance_eval(&block)
  end
end

# Class macro
class Module
  def attr_validated(name, &validator)
    define_method(name) do
      instance_variable_get("@#{name}")
    end

    define_method("#{name}=") do |value|
      raise ArgumentError, "Invalid #{name}" unless validator.call(value)

      instance_variable_set("@#{name}", value)
    end
  end
end

# ============================================================================
# Blocks, Procs, and Lambdas
# ============================================================================

# Method with block
def with_timing
  start = Time.now
  result = yield
  elapsed = Time.now - start
  puts "Elapsed: #{elapsed.round(4)} seconds"
  result
end

# Method accepting block explicitly
def transform_each(array, &block)
  array.map(&block)
end

# Return Proc
def make_multiplier(factor)
  ->(x) { x * factor }
end

# Proc examples
double = proc { |x| x * 2 }
triple = ->(x) { x * 3 }
sum = ->(a, b) { a + b }

# Lambda with multiple args
compare = lambda do |a, b|
  a <=> b
end

# Currying
add = ->(a, b, c) { a + b + c }
add_5 = add.curry[5]
add_5_and_10 = add_5[10]

# ============================================================================
# Iterators and Enumerables
# ============================================================================

def iterator_examples
  numbers = [1, 2, 3, 4, 5, 6, 7, 8, 9, 10]

  # Map/collect
  squared = numbers.map { |n| n**2 }

  # Select/filter
  evens = numbers.select(&:even?)

  # Reject
  odds = numbers.reject(&:even?)

  # Reduce/inject
  sum = numbers.reduce(0, :+)
  product = numbers.inject(1, :*)

  # Each with object
  doubled_hash = numbers.each_with_object({}) { |n, h| h[n] = n * 2 }

  # Group by
  grouped = numbers.group_by { |n| n % 3 }

  # Partition
  evens_odds = numbers.partition(&:even?)

  # Take and drop
  first_3 = numbers.take(3)
  rest = numbers.drop(3)

  # Take/drop while
  small = numbers.take_while { |n| n < 5 }

  # Find/detect
  first_even = numbers.find(&:even?)

  # All/any/none/one
  all_positive = numbers.all?(&:positive?)
  any_even = numbers.any?(&:even?)

  # Zip
  letters = %w[a b c]
  zipped = letters.zip([1, 2, 3])

  # Flatten
  nested = [[1, 2], [3, [4, 5]]]
  flat = nested.flatten

  # Compact
  with_nils = [1, nil, 2, nil, 3]
  without_nils = with_nils.compact

  # Uniq
  with_dups = [1, 2, 2, 3, 3, 3]
  unique = with_dups.uniq

  # Sort
  sorted = numbers.sort { |a, b| b <=> a }
  sorted_by = numbers.sort_by { |n| -n }

  # Chain
  result = numbers
           .select { |n| n > 3 }
           .map { |n| n * 2 }
           .take(3)
           .sum

  result
end

# Custom Enumerator
def fibonacci
  Enumerator.new do |yielder|
    a, b = 0, 1
    loop do
      yielder << a
      a, b = b, a + b
    end
  end
end

# Lazy enumeration
def lazy_example
  (1..Float::INFINITY)
    .lazy
    .select(&:even?)
    .map { |n| n * 2 }
    .take(10)
    .to_a
end

# ============================================================================
# Exception Handling
# ============================================================================

# Custom exception
class AppError < StandardError
  attr_reader :code

  def initialize(message, code = 1)
    super(message)
    @code = code
  end
end

class ValidationError < AppError; end

def exception_examples
  begin
    raise AppError.new('Something went wrong', 42)
  rescue ValidationError => e
    puts "Validation error: #{e.message}"
  rescue AppError => e
    puts "App error #{e.code}: #{e.message}"
  rescue StandardError => e
    puts "Error: #{e.message}"
    raise # Re-raise
  else
    puts 'No exception occurred'
  ensure
    puts 'Cleanup'
  end

  # Retry
  attempts = 0
  begin
    attempts += 1
    raise 'Temporary error' if attempts < 3
  rescue StandardError
    retry if attempts < 3
    raise
  end

  # Throw and catch
  result = catch(:done) do
    (1..100).each do |i|
      throw(:done, i) if i > 50
    end
  end

  result
end

# ============================================================================
# Regular Expressions
# ============================================================================

def regex_examples
  text = 'Hello, my email is user@example.com and phone is 555-1234'

  # Match
  if text =~ /email is (\S+)/
    email = ::Regexp.last_match(1)
  end

  # Match with named groups
  pattern = /(?<email>\S+@\S+)/
  match = text.match(pattern)
  email = match[:email] if match

  # Scan
  words = text.scan(/\w+/)

  # Gsub
  replaced = text.gsub(/\d+/, 'X')

  # Split
  parts = text.split(/[,\s]+/)

  # Case-insensitive
  if text =~ /hello/i
    puts 'Found hello'
  end

  email
end

# ============================================================================
# File I/O
# ============================================================================

def file_examples
  # Write
  File.write('test.txt', "Hello, File!\n")

  # Read
  content = File.read('test.txt')

  # Read lines
  lines = File.readlines('test.txt')

  # Block form (auto-close)
  File.open('test.txt', 'r') do |file|
    file.each_line do |line|
      puts line
    end
  end

  # Append
  File.open('test.txt', 'a') do |file|
    file.puts 'Appended line'
  end

  # Check existence
  exists = File.exist?('test.txt')

  # Delete
  File.delete('test.txt') if File.exist?('test.txt')

  content
end

# ============================================================================
# Concurrency
# ============================================================================

def concurrency_examples
  # Threads
  threads = 5.times.map do |i|
    Thread.new(i) do |num|
      sleep(rand * 0.1)
      Thread.current[:result] = num * 2
    end
  end

  results = threads.map do |t|
    t.join
    t[:result]
  end

  # Mutex
  mutex = Mutex.new
  counter = 0

  threads = 10.times.map do
    Thread.new do
      1000.times do
        mutex.synchronize { counter += 1 }
      end
    end
  end

  threads.each(&:join)

  # Queue
  queue = Queue.new
  producer = Thread.new do
    5.times { |i| queue << i }
  end

  consumer = Thread.new do
    5.times { puts queue.pop }
  end

  producer.join
  consumer.join

  # Fiber
  fiber = Fiber.new do
    (1..3).each do |i|
      Fiber.yield i
    end
    'done'
  end

  3.times { fiber.resume }

  results
end

# ============================================================================
# Pattern Matching (Ruby 3.0+)
# ============================================================================

def pattern_matching_examples(value)
  case value
  in 0
    'zero'
  in Integer if value.positive?
    "positive: #{value}"
  in Integer
    "negative: #{value}"
  in String => s
    "string: #{s}"
  in []
    'empty array'
  in [x]
    "single: #{x}"
  in [x, y]
    "pair: #{x}, #{y}"
  in [head, *tail]
    "head: #{head}, tail: #{tail}"
  in { name:, age: }
    "person: #{name}, #{age}"
  in Point(x: 0, y: 0, z: 0)
    'origin'
  in Point => p
    "point: #{p}"
  else
    'unknown'
  end
end

# Rightward assignment
def rightward_assignment
  { name: 'Alice', age: 30 } => { name:, age: }
  [1, 2, 3] => [first, *rest]

  "Name: #{name}, Age: #{age}"
end

# ============================================================================
# DSL Building
# ============================================================================

class HTMLBuilder
  def initialize
    @content = []
  end

  def method_missing(tag, content = nil, **attrs, &block)
    attr_str = attrs.map { |k, v| " #{k}=\"#{v}\"" }.join
    if block
      @content << "<#{tag}#{attr_str}>"
      instance_eval(&block)
      @content << "</#{tag}>"
    else
      @content << "<#{tag}#{attr_str}>#{content}</#{tag}>"
    end
    self
  end

  def respond_to_missing?(method_name, include_private = false)
    true
  end

  def to_s
    @content.join("\n")
  end
end

def html_example
  builder = HTMLBuilder.new
  builder.html do
    head do
      title 'Hello'
    end
    body do
      h1 'Welcome', class: 'header'
      p 'This is a paragraph'
    end
  end
  builder.to_s
end

# ============================================================================
# Main
# ============================================================================

# Variable declarations
local_var = 42
@instance_var = 'instance'
@@class_var = 'class'
$global_var = 'global'

# Numeric literals
decimal = 1_000_000
hexadecimal = 0xDEADBEEF
octal = 0o755
binary = 0b10101010
float = 3.14
rational = 22r / 7
complex = 3 + 4i

# Strings
single = 'Hello'
double = "Hello, #{local_var}"
heredoc = <<~HEREDOC
  This is a heredoc
  with multiple lines
HEREDOC
char = ?A

# Symbols
symbol = :my_symbol
interpolated_symbol = :"dynamic_#{local_var}"

# Arrays
array = [1, 2, 3, 4, 5]
words = %w[one two three]
symbols = %i[a b c]

# Hashes
hash = { one: 1, two: 2, three: 3 }
string_keys = { 'key' => 'value' }

# Range
range = 1..10
exclusive = 1...10
endless = (1..)
beginless = (..10)

# Set
set = Set[1, 2, 3, 3]

# Control flow
if local_var > 0
  puts 'Positive'
elsif local_var < 0
  puts 'Negative'
else
  puts 'Zero'
end

# Single-line if
puts 'Positive' if local_var.positive?

# Unless
puts 'Not zero' unless local_var.zero?

# Ternary
result = local_var > 0 ? 'positive' : 'non-positive'

# Case/when
case local_var
when 0 then puts 'Zero'
when 1..10 then puts '1-10'
when Integer then puts 'Integer'
else puts 'Other'
end

# Case/in (pattern matching)
case { name: 'Alice', age: 30 }
in { name:, age: } if age >= 18
  puts "Adult: #{name}"
in { name: }
  puts "Minor: #{name}"
end

# Loops
for i in 0..9
  puts i
end

array.each do |item|
  puts item
end

array.each_with_index do |item, index|
  puts "[#{index}] #{item}"
end

5.times { |i| puts i }

1.upto(5) { |i| puts i }

10.downto(1) { |i| puts i }

counter = 0
while counter < 5
  counter += 1
end

counter = 10
until counter.zero?
  counter -= 1
end

loop do
  break if counter > 5

  counter += 1
end

# Objects
point = Point.new(1, 2, 3)
puts point
puts "Distance: #{point.distance(Point.create_origin)}"

person = Person.new(name: 'Alice', age: 30, email: 'alice@example.com')
puts person

circle = Circle.new(5, :blue)
circle.draw
puts "Area: #{circle.area}"

rect = Rectangle.new(4, 6)
puts rect.describe

# Container
container = Container.new
container << 'Hello' << 'World'
container.each { |item| puts item }

# Blocks
with_timing { sleep(0.1) }

# Procs and lambdas
puts double.call(5)
puts triple.call(5)
puts sum.call(2, 3)

# Iterators
puts "Sum: #{iterator_examples}"

# Lazy
puts "Lazy: #{lazy_example}"

# Fibonacci
fib = fibonacci
puts "Fib: #{fib.take(10).to_a}"

# Pattern matching
puts pattern_matching_examples(42)
puts pattern_matching_examples([1, 2, 3])
puts pattern_matching_examples(Point.new(0, 0, 0))

# Metaprogramming
dynamic = DynamicClass.new
dynamic.set_red
puts "Is red? #{dynamic.red?}"

dynamic.configure do
  @custom_value = 'Configured!'
end
puts dynamic.get_custom_value

# Singleton
config = AppConfig.instance
config[:host] = 'localhost'
puts "Config: #{config[:host]}"

# DSL
puts html_example

# Exception handling
exception_examples rescue nil

# Regex
regex_examples

# File I/O
file_examples rescue nil

# Concurrency
concurrency_examples

# Benchmark
Benchmark.bm do |x|
  x.report('test') { 1_000_000.times { 1 + 1 } }
end

puts Constants::GREETING
puts 'Program completed successfully!'
