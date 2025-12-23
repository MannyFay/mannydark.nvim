# ==============================================================================
# Comprehensive Starlark Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This file demonstrates all major Starlark language features
# (used by Bazel, Buck, and other build systems)
# for syntax highlighting purposes.

# ==============================================================================
# Comments
# ==============================================================================

# Single line comment

# Multi-line comments are just
# multiple single line comments

# ==============================================================================
# Variables and Basic Types
# ==============================================================================

# Numbers (integers only, no floats in core Starlark)
integer_val = 42
negative_val = -17
zero = 0
hex_val = 0xDEADBEEF
octal_val = 0o755
binary_val = 0b11010110

# Booleans
true_val = True
false_val = False

# None
none_val = None

# Strings
simple_string = "Hello, Starlark!"
single_quoted = 'Also valid'
with_escapes = "Line 1\nLine 2\tTabbed"
with_quotes = "She said \"Hello!\""
raw_string = r"No \n escape processing"

# Multi-line strings
multi_line = """
This is a multi-line
string in Starlark.
"""

triple_single = '''
Another multi-line
string format.
'''

# String operations
concatenated = "Hello" + " " + "World"
repeated = "ab" * 3  # "ababab"
length = len("hello")
upper = "hello".upper()
lower = "HELLO".lower()
stripped = "  hello  ".strip()
split_result = "a,b,c".split(",")
joined = ",".join(["a", "b", "c"])
replaced = "hello world".replace("world", "starlark")
formatted = "Name: {}, Age: {}".format("Alice", 30)
percent_format = "Value: %d" % 42

# String methods
starts = "hello".startswith("he")
ends = "hello".endswith("lo")
found = "hello".find("ll")
count = "hello".count("l")
index = "hello".index("l")

# ==============================================================================
# Lists
# ==============================================================================

# Empty list
empty_list = []

# List literals
numbers = [1, 2, 3, 4, 5]
strings = ["a", "b", "c"]
mixed = [1, "two", True, None]
nested = [[1, 2], [3, 4], [5, 6]]

# List operations
first = numbers[0]
last = numbers[-1]
slice1 = numbers[1:3]
slice2 = numbers[::2]
reversed_slice = numbers[::-1]

# List methods
numbers.append(6)  # Mutates list (only in certain contexts)
length = len(numbers)
concatenated_list = [1, 2] + [3, 4]
repeated_list = [1, 2] * 3
contains = 2 in numbers
not_contains = 10 not in numbers
index_of = numbers.index(3)
count_of = numbers.count(2)

# List comprehension
doubled = [x * 2 for x in numbers]
filtered = [x for x in numbers if x > 2]
combined = [x + y for x in [1, 2] for y in [10, 100]]
pairs = [(x, y) for x in range(3) for y in range(3) if x != y]

# ==============================================================================
# Tuples
# ==============================================================================

# Tuple literals
simple_tuple = (1, 2, 3)
single_element = (42,)  # Note the comma
empty_tuple = ()
without_parens = 1, 2, 3  # Also a tuple

# Tuple unpacking
a, b, c = simple_tuple
first, *rest = [1, 2, 3, 4, 5]
*start, last = [1, 2, 3, 4, 5]

# Tuple operations
tuple_length = len(simple_tuple)
tuple_concat = (1, 2) + (3, 4)
tuple_contains = 2 in simple_tuple

# ==============================================================================
# Dictionaries
# ==============================================================================

# Empty dict
empty_dict = {}

# Dict literals
person = {
    "name": "Alice",
    "age": 30,
    "email": "alice@example.com",
}

# Dict with various key types
mixed_keys = {
    "string_key": 1,
    42: "int_key",
    (1, 2): "tuple_key",
}

# Dict operations
name = person["name"]
default = person.get("missing", "default")
keys = person.keys()
values = person.values()
items = person.items()
length = len(person)
contains_key = "name" in person

# Dict methods
person.update({"city": "Seattle"})
popped = person.pop("city", None)
person.setdefault("country", "USA")
cleared = dict(person)  # Copy before clear

# Dict comprehension
squares = {x: x * x for x in range(5)}
filtered_dict = {k: v for k, v in person.items() if v != None}

# ==============================================================================
# Control Flow
# ==============================================================================

# If-elif-else
x = 10

if x > 0:
    sign = "positive"
elif x < 0:
    sign = "negative"
else:
    sign = "zero"

# Conditional expression (ternary)
result = "positive" if x > 0 else "non-positive"

# For loops
for i in range(5):
    print(i)

for item in ["a", "b", "c"]:
    print(item)

for key, value in person.items():
    print(key, value)

for i, item in enumerate(["a", "b", "c"]):
    print(i, item)

# While loops (not available in pure Starlark, but in some variants)
# count = 0
# while count < 5:
#     print(count)
#     count += 1

# Loop control
for i in range(10):
    if i == 3:
        continue
    if i == 7:
        break
    print(i)

# ==============================================================================
# Functions
# ==============================================================================

# Simple function
def double(x):
    return x * 2

# Function with default arguments
def greet(name, greeting = "Hello"):
    return greeting + ", " + name + "!"

# Function with *args
def sum_all(*args):
    total = 0
    for arg in args:
        total += arg
    return total

# Function with **kwargs
def create_person(**kwargs):
    return kwargs

# Mixed arguments
def mixed_args(required, optional = None, *args, **kwargs):
    return {
        "required": required,
        "optional": optional,
        "args": args,
        "kwargs": kwargs,
    }

# Function calls
doubled = double(21)
greeting1 = greet("World")
greeting2 = greet("Alice", greeting = "Hi")
total = sum_all(1, 2, 3, 4, 5)
person = create_person(name = "Bob", age = 25)

# Nested functions
def outer(x):
    def inner(y):
        return x + y
    return inner(10)

# Lambda (anonymous function)
square = lambda x: x * x
mapped = [square(x) for x in range(5)]

# Higher-order functions
def apply_twice(f, x):
    return f(f(x))

result = apply_twice(double, 5)

# ==============================================================================
# Built-in Functions
# ==============================================================================

# Type functions
type_str = type("hello")
type_int = type(42)
type_list = type([])
type_dict = type({})

# Conversion functions
str_val = str(42)
int_val = int("42")
bool_val = bool(1)
list_val = list("abc")
dict_val = dict([("a", 1), ("b", 2)])
tuple_val = tuple([1, 2, 3])

# Sequence functions
length = len([1, 2, 3])
sorted_list = sorted([3, 1, 2])
reversed_list = list(reversed([1, 2, 3]))
enumerated = list(enumerate(["a", "b", "c"]))
zipped = list(zip([1, 2], ["a", "b"]))
range_list = list(range(5))
range_step = list(range(0, 10, 2))

# Math functions
max_val = max(1, 2, 3)
min_val = min(1, 2, 3)
max_list = max([1, 2, 3])
min_list = min([1, 2, 3])
abs_val = abs(-5)

# Boolean functions
any_true = any([False, True, False])
all_true = all([True, True, True])

# String functions
repr_val = repr("hello")
hash_val = hash("hello")

# Print function
print("Hello, World!")
print("Multiple", "arguments", 123)

# ==============================================================================
# Operators
# ==============================================================================

# Arithmetic
add = 10 + 5
sub = 10 - 5
mul = 10 * 5
div = 10 // 3  # Integer division only
mod = 10 % 3
neg = -10

# Comparison
eq = 1 == 1
ne = 1 != 2
lt = 1 < 2
le = 1 <= 2
gt = 2 > 1
ge = 2 >= 1

# Boolean
and_op = True and False
or_op = True or False
not_op = not True

# Membership
in_op = 2 in [1, 2, 3]
not_in_op = 4 not in [1, 2, 3]

# Identity (in some Starlark implementations)
# is_op = x is None
# is_not_op = x is not None

# Bitwise
bit_and = 0xFF & 0x0F
bit_or = 0xF0 | 0x0F
bit_xor = 0xFF ^ 0x0F
bit_not = ~0xFF
left_shift = 1 << 4
right_shift = 16 >> 2

# ==============================================================================
# Bazel/Buck Specific (Build Rules)
# ==============================================================================

# Load statement (import)
load("@rules_python//python:defs.bzl", "py_binary", "py_library", "py_test")
load(":defs.bzl", "my_rule", "my_macro")
load("//path/to:file.bzl", name = "alias", "other_symbol")

# Package function
# package(default_visibility = ["//visibility:public"])

# Common Bazel rules

# cc_library
# cc_library(
#     name = "mylib",
#     srcs = ["mylib.cc"],
#     hdrs = ["mylib.h"],
#     deps = [
#         "//other:lib",
#         "@external//:dependency",
#     ],
#     visibility = ["//visibility:public"],
# )

# cc_binary
# cc_binary(
#     name = "myapp",
#     srcs = ["main.cc"],
#     deps = [":mylib"],
# )

# py_library
# py_library(
#     name = "my_python_lib",
#     srcs = ["lib.py"],
#     deps = [
#         requirement("requests"),
#     ],
# )

# py_binary
# py_binary(
#     name = "my_python_app",
#     srcs = ["main.py"],
#     deps = [":my_python_lib"],
#     python_version = "PY3",
# )

# py_test
# py_test(
#     name = "my_test",
#     srcs = ["test_lib.py"],
#     deps = [":my_python_lib"],
# )

# java_library
# java_library(
#     name = "my_java_lib",
#     srcs = glob(["src/**/*.java"]),
#     deps = [
#         "//other:java_lib",
#         "@maven//:com_google_guava_guava",
#     ],
# )

# genrule
# genrule(
#     name = "generate_file",
#     srcs = ["input.txt"],
#     outs = ["output.txt"],
#     cmd = "cp $< $@",
# )

# filegroup
# filegroup(
#     name = "data_files",
#     srcs = glob(["data/**/*"]),
# )

# exports_files
# exports_files(["file1.txt", "file2.txt"])

# config_setting
# config_setting(
#     name = "linux",
#     constraint_values = ["@platforms//os:linux"],
# )

# select (configurable attributes)
# cc_binary(
#     name = "platform_specific",
#     srcs = ["main.cc"],
#     deps = select({
#         ":linux": ["//linux:lib"],
#         ":macos": ["//macos:lib"],
#         "//conditions:default": ["//generic:lib"],
#     }),
# )

# ==============================================================================
# Macros (Functions that generate rules)
# ==============================================================================

def my_cc_library(name, srcs, hdrs = [], deps = [], **kwargs):
    """A custom macro wrapping cc_library.

    Args:
        name: The name of the target
        srcs: Source files
        hdrs: Header files
        deps: Dependencies
        **kwargs: Additional arguments passed to cc_library
    """
    native.cc_library(
        name = name,
        srcs = srcs,
        hdrs = hdrs,
        deps = deps + ["//common:base"],
        copts = ["-Wall", "-Werror"],
        **kwargs
    )

def generate_tests(name, test_files):
    """Generate test targets for each test file."""
    for test_file in test_files:
        test_name = test_file.replace(".py", "").replace("/", "_")
        native.py_test(
            name = test_name,
            srcs = [test_file],
            deps = ["//testing:framework"],
        )

# ==============================================================================
# Rule Definition (Provider-based rules)
# ==============================================================================

# Provider definition
# MyInfo = provider(
#     doc = "Information about my custom rule",
#     fields = {
#         "files": "depset of output files",
#         "data": "string data",
#     },
# )

# Rule implementation
def _my_rule_impl(ctx):
    # Access attributes
    name = ctx.attr.name
    srcs = ctx.files.srcs
    deps = ctx.attr.deps

    # Create output file
    output = ctx.actions.declare_file(ctx.label.name + ".out")

    # Run action
    ctx.actions.run(
        inputs = srcs,
        outputs = [output],
        executable = ctx.executable._tool,
        arguments = [
            "--output", output.path,
        ] + [src.path for src in srcs],
        mnemonic = "MyRule",
        progress_message = "Processing %s" % ctx.label,
    )

    # Alternative: run_shell
    # ctx.actions.run_shell(
    #     inputs = srcs,
    #     outputs = [output],
    #     command = "cat $@ > $1",
    #     arguments = [output.path] + [src.path for src in srcs],
    # )

    # Alternative: write
    # ctx.actions.write(
    #     output = output,
    #     content = "Generated content\n",
    # )

    # Return providers
    return [
        DefaultInfo(
            files = depset([output]),
            runfiles = ctx.runfiles(files = srcs),
        ),
        # MyInfo(
        #     files = depset([output]),
        #     data = "custom data",
        # ),
    ]

# Rule definition
# my_rule = rule(
#     implementation = _my_rule_impl,
#     attrs = {
#         "srcs": attr.label_list(
#             allow_files = True,
#             mandatory = True,
#             doc = "Source files",
#         ),
#         "deps": attr.label_list(
#             providers = [DefaultInfo],
#             doc = "Dependencies",
#         ),
#         "data": attr.string(
#             default = "",
#             doc = "Additional data",
#         ),
#         "output_name": attr.string(
#             doc = "Output file name",
#         ),
#         "_tool": attr.label(
#             default = "//tools:processor",
#             executable = True,
#             cfg = "exec",
#         ),
#     },
#     outputs = {
#         "out": "%{name}.generated",
#     },
#     doc = "My custom rule documentation",
# )

# ==============================================================================
# Aspect Definition
# ==============================================================================

def _my_aspect_impl(target, ctx):
    # Access target information
    files = target[DefaultInfo].files

    # Create output
    output = ctx.actions.declare_file(target.label.name + ".aspect_out")

    ctx.actions.write(
        output = output,
        content = "Aspect output for %s\n" % target.label,
    )

    return [
        OutputGroupInfo(
            aspect_outputs = depset([output]),
        ),
    ]

# my_aspect = aspect(
#     implementation = _my_aspect_impl,
#     attr_aspects = ["deps"],
#     attrs = {
#         "_tool": attr.label(
#             default = "//tools:aspect_tool",
#             executable = True,
#             cfg = "exec",
#         ),
#     },
# )

# ==============================================================================
# Repository Rules
# ==============================================================================

def _my_repository_impl(ctx):
    # Download and extract
    ctx.download_and_extract(
        url = ctx.attr.url,
        sha256 = ctx.attr.sha256,
        stripPrefix = ctx.attr.strip_prefix,
    )

    # Or download single file
    # ctx.download(
    #     url = ctx.attr.url,
    #     output = "file.txt",
    #     sha256 = ctx.attr.sha256,
    # )

    # Execute command
    result = ctx.execute(["ls", "-la"])
    if result.return_code != 0:
        fail("Command failed: " + result.stderr)

    # Create BUILD file
    ctx.file(
        "BUILD.bazel",
        content = """
load("@rules_cc//cc:defs.bzl", "cc_library")

cc_library(
    name = "lib",
    srcs = glob(["src/**/*.cc"]),
    hdrs = glob(["include/**/*.h"]),
    visibility = ["//visibility:public"],
)
""",
    )

# my_repository = repository_rule(
#     implementation = _my_repository_impl,
#     attrs = {
#         "url": attr.string(mandatory = True),
#         "sha256": attr.string(mandatory = True),
#         "strip_prefix": attr.string(default = ""),
#     },
# )

# ==============================================================================
# Struct and Depset
# ==============================================================================

# Struct (immutable named tuple)
my_struct = struct(
    name = "example",
    value = 42,
    data = ["a", "b", "c"],
)

# Access struct fields
struct_name = my_struct.name
struct_value = my_struct.value

# Struct method
struct_dict = my_struct.to_json()

# Depset (efficient set for collecting transitive dependencies)
# depset_example = depset(
#     direct = ["file1.txt", "file2.txt"],
#     transitive = [dep_depset],
#     order = "postorder",  # or "preorder", "topological", "default"
# )

# Depset operations
# depset_list = depset_example.to_list()

# ==============================================================================
# String Interpolation and Formatting
# ==============================================================================

name = "Alice"
age = 30

# Percent formatting
msg1 = "Name: %s" % name
msg2 = "Name: %s, Age: %d" % (name, age)
msg3 = "Float: %.2f" % 3.14159

# Format method
msg4 = "Name: {}".format(name)
msg5 = "Name: {}, Age: {}".format(name, age)
msg6 = "Name: {name}, Age: {age}".format(name = name, age = age)
msg7 = "First: {0}, Second: {1}, First again: {0}".format("a", "b")

# ==============================================================================
# Error Handling
# ==============================================================================

# Fail function (terminates execution)
def validate_positive(x):
    if x <= 0:
        fail("Value must be positive, got: %d" % x)
    return x

# Assertions (in test code)
def _test_impl(ctx):
    # Using unittest (from bazel-skylib)
    # asserts.equals(env, expected, actual)
    # asserts.true(env, condition)
    # asserts.false(env, condition)
    pass

# ==============================================================================
# End of Starlark Sample
# ==============================================================================
