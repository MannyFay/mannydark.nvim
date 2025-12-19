#!/usr/bin/env python3
# -*- coding: utf-8 -*-
"""
Comprehensive Python language sample demonstrating all syntax features.

This module showcases Python 3.10+ features including:
- Type hints and annotations
- Pattern matching
- Async/await
- Dataclasses
- Decorators
- And much more...

Author: Sample Author
Version: 1.0.0
"""

from __future__ import annotations

import abc
import asyncio
import contextlib
import dataclasses
import enum
import functools
import inspect
import itertools
import json
import logging
import operator
import os
import re
import sys
import threading
import time
import typing
from collections import defaultdict, namedtuple, deque, Counter
from collections.abc import Callable, Iterator, Generator, Sequence, Mapping
from concurrent.futures import ThreadPoolExecutor, ProcessPoolExecutor
from dataclasses import dataclass, field
from datetime import datetime, date, timedelta
from decimal import Decimal
from fractions import Fraction
from functools import wraps, lru_cache, partial, reduce
from io import StringIO, BytesIO
from pathlib import Path
from typing import (
    Any, ClassVar, Final, Generic, Literal, Optional, Protocol,
    TypeAlias, TypeVar, Union, overload, runtime_checkable,
    Self, TypedDict, Annotated, ParamSpec, Concatenate
)

# Type variables
T = TypeVar('T')
K = TypeVar('K')
V = TypeVar('V')
T_co = TypeVar('T_co', covariant=True)
T_contra = TypeVar('T_contra', contravariant=True)
P = ParamSpec('P')

# Type aliases
Number: TypeAlias = int | float
StringList: TypeAlias = list[str]
Callback: TypeAlias = Callable[[int], int]
JsonValue: TypeAlias = dict[str, Any] | list[Any] | str | int | float | bool | None

# Constants
MAX_BUFFER_SIZE: Final[int] = 1024
PI: Final[float] = 3.14159265358979323846
GREETING: Final[str] = "Hello, Python!"


# ============================================================================
# Enums
# ============================================================================

class Color(enum.Enum):
    """Color enumeration with values."""
    RED = 0xFF0000
    GREEN = 0x00FF00
    BLUE = 0x0000FF
    WHITE = 0xFFFFFF
    BLACK = 0x000000

    @property
    def hex_string(self) -> str:
        return f"#{self.value:06X}"


class Status(enum.IntEnum):
    """Status enumeration."""
    OK = 0
    ERROR = 1
    PENDING = 2
    TIMEOUT = 3


class Permissions(enum.Flag):
    """Flags enumeration."""
    NONE = 0
    READ = 1
    WRITE = 2
    EXECUTE = 4
    ALL = READ | WRITE | EXECUTE


class AutoName(enum.Enum):
    """Enum with auto-generated values."""
    def _generate_next_value_(name, start, count, last_values):
        return name.lower()

    ITEM_A = enum.auto()
    ITEM_B = enum.auto()
    ITEM_C = enum.auto()


# ============================================================================
# Dataclasses
# ============================================================================

@dataclass
class Point:
    """Point dataclass with default values and methods."""
    x: float
    y: float
    z: float = 0.0

    def distance(self, other: Point) -> float:
        """Calculate Euclidean distance to another point."""
        dx = self.x - other.x
        dy = self.y - other.y
        dz = self.z - other.z
        return (dx**2 + dy**2 + dz**2) ** 0.5

    def __add__(self, other: Point) -> Point:
        return Point(self.x + other.x, self.y + other.y, self.z + other.z)

    def __sub__(self, other: Point) -> Point:
        return Point(self.x - other.x, self.y - other.y, self.z - other.z)

    def __mul__(self, scalar: float) -> Point:
        return Point(self.x * scalar, self.y * scalar, self.z * scalar)

    @classmethod
    def origin(cls) -> Point:
        """Return the origin point."""
        return cls(0, 0, 0)


@dataclass(frozen=True)
class ImmutablePoint:
    """Immutable point using frozen dataclass."""
    x: float
    y: float
    z: float = 0.0


@dataclass
class Person:
    """Person dataclass with complex fields."""
    name: str
    age: int
    email: str | None = None
    tags: list[str] = field(default_factory=list)
    _id: int = field(default_factory=lambda: id(object()), repr=False)

    def __post_init__(self):
        if self.age < 0:
            raise ValueError("Age cannot be negative")


@dataclass(slots=True)
class SlottedClass:
    """Dataclass using __slots__ for memory efficiency."""
    x: int
    y: int


# ============================================================================
# Protocols and ABCs
# ============================================================================

@runtime_checkable
class Drawable(Protocol):
    """Protocol for drawable objects."""
    def draw(self) -> None: ...


class Serializable(Protocol):
    """Protocol for serializable objects."""
    def serialize(self) -> bytes: ...
    def deserialize(self, data: bytes) -> Self: ...


class Shape(abc.ABC):
    """Abstract base class for shapes."""

    @abc.abstractmethod
    def area(self) -> float:
        """Calculate the area."""
        pass

    @abc.abstractmethod
    def perimeter(self) -> float:
        """Calculate the perimeter."""
        pass

    def describe(self) -> str:
        """Return a description of the shape."""
        return f"Shape: area={self.area():.2f}, perimeter={self.perimeter():.2f}"


# ============================================================================
# Classes
# ============================================================================

class Circle(Shape):
    """Circle implementation."""

    def __init__(self, radius: float, color: Color = Color.BLACK):
        self._radius = radius
        self.color = color

    @property
    def radius(self) -> float:
        return self._radius

    @radius.setter
    def radius(self, value: float) -> None:
        if value < 0:
            raise ValueError("Radius cannot be negative")
        self._radius = value

    def area(self) -> float:
        return PI * self._radius ** 2

    def perimeter(self) -> float:
        return 2 * PI * self._radius

    def draw(self) -> None:
        print(f"Drawing circle with radius {self._radius} in {self.color.name}")

    def __repr__(self) -> str:
        return f"Circle(radius={self._radius}, color={self.color})"


class Rectangle(Shape):
    """Rectangle implementation."""

    def __init__(self, width: float, height: float, color: Color = Color.BLACK):
        self.width = width
        self.height = height
        self.color = color

    def area(self) -> float:
        return self.width * self.height

    def perimeter(self) -> float:
        return 2 * (self.width + self.height)

    def draw(self) -> None:
        print(f"Drawing rectangle {self.width}x{self.height} in {self.color.name}")


class Square(Rectangle):
    """Square as special case of Rectangle."""

    def __init__(self, side: float, color: Color = Color.BLACK):
        super().__init__(side, side, color)

    @property
    def side(self) -> float:
        return self.width

    @side.setter
    def side(self, value: float) -> None:
        self.width = value
        self.height = value


# ============================================================================
# Generic Classes
# ============================================================================

class Container(Generic[T]):
    """Generic container class."""

    def __init__(self):
        self._items: list[T] = []

    def add(self, item: T) -> None:
        self._items.append(item)

    def get(self, index: int) -> T:
        return self._items[index]

    def __len__(self) -> int:
        return len(self._items)

    def __iter__(self) -> Iterator[T]:
        return iter(self._items)

    def __getitem__(self, index: int) -> T:
        return self._items[index]


class Pair(Generic[K, V]):
    """Generic pair/tuple class."""

    def __init__(self, first: K, second: V):
        self.first = first
        self.second = second

    def swap(self) -> Pair[V, K]:
        return Pair(self.second, self.first)

    def __repr__(self) -> str:
        return f"Pair({self.first!r}, {self.second!r})"


class Result(Generic[T]):
    """Result type for error handling."""

    def __init__(self, value: T | None = None, error: Exception | None = None):
        self._value = value
        self._error = error

    @classmethod
    def ok(cls, value: T) -> Result[T]:
        return cls(value=value)

    @classmethod
    def err(cls, error: Exception) -> Result[T]:
        return cls(error=error)

    @property
    def is_ok(self) -> bool:
        return self._error is None

    def unwrap(self) -> T:
        if self._error:
            raise self._error
        return self._value  # type: ignore

    def unwrap_or(self, default: T) -> T:
        return self._value if self.is_ok else default


# ============================================================================
# Decorators
# ============================================================================

def timing(func: Callable[P, T]) -> Callable[P, T]:
    """Decorator to measure execution time."""
    @wraps(func)
    def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
        start = time.perf_counter()
        result = func(*args, **kwargs)
        elapsed = time.perf_counter() - start
        print(f"{func.__name__} took {elapsed:.4f} seconds")
        return result
    return wrapper


def retry(attempts: int = 3, delay: float = 1.0):
    """Decorator factory for retrying failed function calls."""
    def decorator(func: Callable[P, T]) -> Callable[P, T]:
        @wraps(func)
        def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
            last_error: Exception | None = None
            for attempt in range(attempts):
                try:
                    return func(*args, **kwargs)
                except Exception as e:
                    last_error = e
                    if attempt < attempts - 1:
                        time.sleep(delay)
            raise last_error  # type: ignore
        return wrapper
    return decorator


def debug(func: Callable[P, T]) -> Callable[P, T]:
    """Decorator to print function calls."""
    @wraps(func)
    def wrapper(*args: P.args, **kwargs: P.kwargs) -> T:
        args_repr = [repr(a) for a in args]
        kwargs_repr = [f"{k}={v!r}" for k, v in kwargs.items()]
        signature = ", ".join(args_repr + kwargs_repr)
        print(f"Calling {func.__name__}({signature})")
        result = func(*args, **kwargs)
        print(f"{func.__name__} returned {result!r}")
        return result
    return wrapper


def singleton(cls: type[T]) -> type[T]:
    """Class decorator to make a singleton."""
    instances: dict[type, T] = {}

    @wraps(cls)
    def get_instance(*args, **kwargs) -> T:
        if cls not in instances:
            instances[cls] = cls(*args, **kwargs)
        return instances[cls]

    return get_instance  # type: ignore


# ============================================================================
# Context Managers
# ============================================================================

class Timer:
    """Context manager for timing code blocks."""

    def __enter__(self) -> Timer:
        self.start = time.perf_counter()
        return self

    def __exit__(self, *args) -> None:
        self.elapsed = time.perf_counter() - self.start
        print(f"Elapsed: {self.elapsed:.4f} seconds")


@contextlib.contextmanager
def temporary_directory() -> Generator[Path, None, None]:
    """Context manager for temporary directory."""
    import tempfile
    import shutil

    path = Path(tempfile.mkdtemp())
    try:
        yield path
    finally:
        shutil.rmtree(path)


# ============================================================================
# Generators and Iterators
# ============================================================================

def fibonacci() -> Generator[int, None, None]:
    """Infinite Fibonacci sequence generator."""
    a, b = 0, 1
    while True:
        yield a
        a, b = b, a + b


def chunked(iterable: Sequence[T], size: int) -> Generator[Sequence[T], None, None]:
    """Yield successive chunks of given size."""
    for i in range(0, len(iterable), size):
        yield iterable[i:i + size]


def countdown(n: int) -> Generator[int, None, None]:
    """Countdown generator with send capability."""
    while n > 0:
        value = yield n
        if value is not None:
            n = value
        else:
            n -= 1


class InfiniteCounter:
    """Custom iterator class."""

    def __init__(self, start: int = 0, step: int = 1):
        self.current = start
        self.step = step

    def __iter__(self) -> InfiniteCounter:
        return self

    def __next__(self) -> int:
        current = self.current
        self.current += self.step
        return current


# ============================================================================
# Async/Await
# ============================================================================

async def fetch_data(url: str) -> str:
    """Simulate async data fetching."""
    await asyncio.sleep(0.1)
    return f"Data from {url}"


async def parallel_fetch(urls: list[str]) -> list[str]:
    """Fetch multiple URLs in parallel."""
    tasks = [fetch_data(url) for url in urls]
    return await asyncio.gather(*tasks)


async def async_generator() -> typing.AsyncGenerator[int, None]:
    """Async generator example."""
    for i in range(10):
        await asyncio.sleep(0.01)
        yield i


async def process_stream():
    """Process async generator."""
    async for item in async_generator():
        print(f"Received: {item}")


class AsyncContextManager:
    """Async context manager example."""

    async def __aenter__(self) -> AsyncContextManager:
        print("Entering async context")
        return self

    async def __aexit__(self, *args) -> None:
        print("Exiting async context")


# ============================================================================
# Pattern Matching (Python 3.10+)
# ============================================================================

def pattern_match_example(value: Any) -> str:
    """Demonstrate structural pattern matching."""
    match value:
        case 0:
            return "zero"
        case int(n) if n > 0:
            return f"positive int: {n}"
        case int(n):
            return f"negative int: {n}"
        case str(s):
            return f"string: {s}"
        case []:
            return "empty list"
        case [x]:
            return f"single item: {x}"
        case [x, y]:
            return f"pair: {x}, {y}"
        case [x, *rest]:
            return f"list starting with {x}"
        case {"name": name, "age": age}:
            return f"person: {name}, {age}"
        case Point(x=0, y=0, z=0):
            return "origin"
        case Point(x=x, y=y, z=z):
            return f"point: ({x}, {y}, {z})"
        case Color.RED:
            return "red color"
        case _:
            return "unknown"


def process_command(command: str) -> str:
    """Pattern matching with guards."""
    match command.split():
        case ["quit" | "exit" | "q"]:
            return "Exiting..."
        case ["hello", name]:
            return f"Hello, {name}!"
        case ["add", *numbers] if all(n.isdigit() for n in numbers):
            return str(sum(int(n) for n in numbers))
        case _:
            return "Unknown command"


# ============================================================================
# Descriptors
# ============================================================================

class Validated:
    """Descriptor for validated attributes."""

    def __init__(self, validator: Callable[[Any], bool], error_message: str):
        self.validator = validator
        self.error_message = error_message
        self.name = ""

    def __set_name__(self, owner: type, name: str) -> None:
        self.name = name

    def __get__(self, obj: Any, objtype: type | None = None) -> Any:
        if obj is None:
            return self
        return obj.__dict__.get(self.name)

    def __set__(self, obj: Any, value: Any) -> None:
        if not self.validator(value):
            raise ValueError(self.error_message)
        obj.__dict__[self.name] = value


class PositiveNumber:
    """Descriptor ensuring positive numbers."""

    def __set_name__(self, owner: type, name: str) -> None:
        self.name = f"_{name}"

    def __get__(self, obj: Any, objtype: type | None = None) -> float:
        if obj is None:
            return self  # type: ignore
        return getattr(obj, self.name, 0.0)

    def __set__(self, obj: Any, value: float) -> None:
        if value < 0:
            raise ValueError("Value must be positive")
        setattr(obj, self.name, value)


# ============================================================================
# Metaclasses
# ============================================================================

class SingletonMeta(type):
    """Metaclass for singleton pattern."""
    _instances: dict[type, Any] = {}

    def __call__(cls, *args, **kwargs):
        if cls not in cls._instances:
            cls._instances[cls] = super().__call__(*args, **kwargs)
        return cls._instances[cls]


class AutoRegisterMeta(type):
    """Metaclass that auto-registers subclasses."""
    registry: dict[str, type] = {}

    def __new__(mcs, name: str, bases: tuple, namespace: dict):
        cls = super().__new__(mcs, name, bases, namespace)
        if bases:  # Don't register the base class
            mcs.registry[name] = cls
        return cls


# ============================================================================
# TypedDict
# ============================================================================

class PersonDict(TypedDict):
    """Typed dictionary for person data."""
    name: str
    age: int
    email: str | None


class PersonDictTotal(TypedDict, total=False):
    """Typed dictionary with optional fields."""
    name: str
    age: int
    email: str
    tags: list[str]


# ============================================================================
# NamedTuple
# ============================================================================

class Coordinate(typing.NamedTuple):
    """Named tuple for coordinates."""
    x: float
    y: float
    label: str = ""


# ============================================================================
# Functions
# ============================================================================

def add(a: int, b: int) -> int:
    """Add two integers."""
    return a + b


def greet(name: str, *, greeting: str = "Hello") -> str:
    """Greet someone with keyword-only argument."""
    return f"{greeting}, {name}!"


def sum_all(*args: int, initial: int = 0) -> int:
    """Sum all arguments with variadic args."""
    return sum(args, initial)


def process(**kwargs: Any) -> dict[str, Any]:
    """Process keyword arguments."""
    return {k: v for k, v in kwargs.items() if v is not None}


@overload
def parse(value: str) -> str: ...
@overload
def parse(value: int) -> int: ...
@overload
def parse(value: list[Any]) -> list[Any]: ...

def parse(value: str | int | list[Any]) -> str | int | list[Any]:
    """Overloaded parse function."""
    if isinstance(value, str):
        return value.strip()
    elif isinstance(value, int):
        return value * 2
    else:
        return [x for x in value if x]


@lru_cache(maxsize=128)
def expensive_computation(n: int) -> int:
    """Memoized expensive computation."""
    time.sleep(0.01)
    return n ** 2


def higher_order(func: Callable[[int], int]) -> Callable[[int], int]:
    """Higher-order function."""
    def wrapper(x: int) -> int:
        return func(x) + 1
    return wrapper


# Lambda functions
square: Callable[[int], int] = lambda x: x ** 2
add_lambda: Callable[[int, int], int] = lambda x, y: x + y


# ============================================================================
# Main
# ============================================================================

def main() -> None:
    """Main function demonstrating all features."""

    # Basic types
    integer: int = 42
    floating: float = 3.14
    string: str = "Hello, World!"
    boolean: bool = True
    none_value: None = None

    # Numeric literals
    decimal = 1_000_000
    hexadecimal = 0xDEADBEEF
    octal = 0o755
    binary = 0b10101010
    scientific = 1.23e-4
    complex_num = 3 + 4j

    # Strings
    single_quote = 'Hello'
    double_quote = "Hello"
    multiline = """
    This is a
    multi-line string
    """
    raw_string = r"Raw \n string"
    f_string = f"Value: {integer}"
    byte_string = b"Bytes"

    # Collections
    list_val: list[int] = [1, 2, 3, 4, 5]
    tuple_val: tuple[int, str, float] = (1, "hello", 3.14)
    set_val: set[int] = {1, 2, 3, 3}
    dict_val: dict[str, int] = {"one": 1, "two": 2}
    frozenset_val: frozenset[int] = frozenset({1, 2, 3})

    # Comprehensions
    list_comp = [x ** 2 for x in range(10)]
    set_comp = {x % 3 for x in range(10)}
    dict_comp = {x: x ** 2 for x in range(5)}
    gen_exp = (x ** 2 for x in range(10))

    # Conditional expressions
    walrus := 42  # Walrus operator
    result = "positive" if walrus > 0 else "non-positive"

    # Control flow
    if integer > 0:
        print("Positive")
    elif integer < 0:
        print("Negative")
    else:
        print("Zero")

    # Match expression (Python 3.10+)
    match integer:
        case 0:
            print("Zero")
        case n if n > 0:
            print(f"Positive: {n}")
        case _:
            print("Negative")

    # Loops
    for i in range(10):
        if i == 5:
            continue
        if i == 8:
            break
        print(i)
    else:
        print("Loop completed")

    for index, value in enumerate(list_val):
        print(f"[{index}] = {value}")

    counter = 0
    while counter < 5:
        counter += 1

    # Exception handling
    try:
        result = 10 / 0
    except ZeroDivisionError as e:
        print(f"Error: {e}")
    except (TypeError, ValueError) as e:
        print(f"Type/Value error: {e}")
    except Exception as e:
        print(f"General error: {e}")
        raise
    else:
        print("No exception occurred")
    finally:
        print("Cleanup")

    # Context managers
    with Timer() as timer:
        time.sleep(0.1)

    # Multiple context managers
    with open("test.txt", "w") as f1, StringIO() as f2:
        f1.write("Hello")
        f2.write("World")

    # Classes
    point = Point(1, 2, 3)
    print(f"Point: {point}")
    print(f"Distance: {point.distance(Point.origin())}")

    person = Person("Alice", 30, "alice@example.com")
    print(f"Person: {person}")

    circle = Circle(5, Color.BLUE)
    circle.draw()
    print(f"Area: {circle.area()}")

    # Generic container
    container: Container[str] = Container()
    container.add("Hello")
    container.add("World")
    for item in container:
        print(item)

    # Decorators
    @timing
    def slow_function():
        time.sleep(0.1)
        return 42

    slow_function()

    # Generators
    fib = fibonacci()
    first_10 = [next(fib) for _ in range(10)]
    print(f"First 10 Fibonacci: {first_10}")

    # Pattern matching
    print(pattern_match_example(Point(1, 2, 3)))
    print(pattern_match_example({"name": "Bob", "age": 25}))

    # Async (would need to run in event loop)
    # asyncio.run(process_stream())

    # Functional programming
    numbers = [1, 2, 3, 4, 5]
    squared = list(map(lambda x: x ** 2, numbers))
    evens = list(filter(lambda x: x % 2 == 0, numbers))
    total = reduce(lambda a, b: a + b, numbers)

    print(f"Squared: {squared}")
    print(f"Evens: {evens}")
    print(f"Total: {total}")

    # Itertools
    pairs = list(itertools.combinations(range(4), 2))
    chained = list(itertools.chain([1, 2], [3, 4]))
    grouped = {k: list(v) for k, v in itertools.groupby([1, 1, 2, 2, 3], key=lambda x: x)}

    # Type checking at runtime
    assert isinstance(circle, Shape)
    assert isinstance(circle, Drawable)

    # Reflection
    print(f"Type: {type(circle).__name__}")
    print(f"MRO: {[c.__name__ for c in type(circle).__mro__]}")
    print(f"Dict: {circle.__dict__}")

    # Cleanup
    if Path("test.txt").exists():
        Path("test.txt").unlink()

    print(GREETING)
    print("Program completed successfully!")


if __name__ == "__main__":
    main()
