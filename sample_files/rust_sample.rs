//! Comprehensive Rust language sample demonstrating all syntax features
//!
//! # Example
//! ```rust
//! let sample = Sample::new();
//! sample.run();
//! ```

#![allow(dead_code, unused_variables, unused_imports)]
#![feature(never_type)]

// External crate imports
use std::collections::{HashMap, HashSet, BTreeMap, VecDeque};
use std::io::{self, Read, Write, BufReader, BufWriter};
use std::fs::{File, OpenOptions};
use std::path::{Path, PathBuf};
use std::sync::{Arc, Mutex, RwLock, mpsc};
use std::thread;
use std::time::{Duration, Instant};
use std::fmt::{self, Display, Debug, Formatter};
use std::ops::{Add, Sub, Mul, Div, Deref, DerefMut, Index, IndexMut};
use std::marker::PhantomData;
use std::cell::{Cell, RefCell};
use std::rc::Rc;
use std::cmp::{Ordering, PartialOrd, Ord};
use std::hash::{Hash, Hasher};
use std::convert::{From, Into, TryFrom, TryInto};
use std::iter::{Iterator, IntoIterator, FromIterator};
use std::error::Error;
use std::pin::Pin;
use std::future::Future;
use std::task::{Context, Poll};

// Module declarations
mod submodule {
    pub fn public_function() {
        println!("Called from submodule");
    }

    pub(crate) fn crate_visible() {
        println!("Visible within crate");
    }

    pub(super) fn parent_visible() {
        println!("Visible to parent module");
    }
}

// Constants and statics
const MAX_BUFFER_SIZE: usize = 1024;
const PI: f64 = 3.14159265358979323846;
const GREETING: &str = "Hello, World!";
const ARRAY: [i32; 5] = [1, 2, 3, 4, 5];

static COUNTER: std::sync::atomic::AtomicUsize = std::sync::atomic::AtomicUsize::new(0);
static mut MUTABLE_STATIC: i32 = 0;

// Type aliases
type Result<T> = std::result::Result<T, Box<dyn Error>>;
type Callback = fn(i32) -> i32;
type AsyncCallback = Box<dyn Fn(i32) -> Pin<Box<dyn Future<Output = i32>>>>;

// Enums
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash)]
pub enum Color {
    Red,
    Green,
    Blue,
    Rgb(u8, u8, u8),
    Rgba { r: u8, g: u8, b: u8, a: u8 },
}

#[derive(Debug)]
pub enum Message {
    Quit,
    Move { x: i32, y: i32 },
    Write(String),
    ChangeColor(Color),
}

#[repr(u8)]
pub enum Status {
    Ok = 0,
    Error = 1,
    Pending = 2,
}

// Structs
#[derive(Debug, Clone, PartialEq)]
pub struct Point<T> {
    pub x: T,
    pub y: T,
    pub z: T,
}

#[derive(Debug, Default)]
pub struct Rectangle {
    width: f64,
    height: f64,
}

// Tuple struct
#[derive(Debug, Clone, Copy)]
pub struct Meters(f64);

// Unit struct
#[derive(Debug)]
pub struct Unit;

// Newtype pattern
pub struct Wrapper<T>(pub T);

// Struct with lifetime parameters
#[derive(Debug)]
pub struct Ref<'a, T: 'a> {
    data: &'a T,
}

// Struct with const generics
#[derive(Debug)]
pub struct Array<T, const N: usize> {
    data: [T; N],
}

// Implementation blocks
impl<T> Point<T> {
    pub fn new(x: T, y: T, z: T) -> Self {
        Self { x, y, z }
    }
}

impl<T: Add<Output = T> + Copy> Point<T> {
    pub fn add(&self, other: &Point<T>) -> Point<T> {
        Point {
            x: self.x + other.x,
            y: self.y + other.y,
            z: self.z + other.z,
        }
    }
}

impl<T: Display> Display for Point<T> {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "({}, {}, {})", self.x, self.y, self.z)
    }
}

impl Rectangle {
    // Associated function (constructor)
    pub fn new(width: f64, height: f64) -> Self {
        Self { width, height }
    }

    // Method with &self
    pub fn area(&self) -> f64 {
        self.width * self.height
    }

    // Method with &mut self
    pub fn scale(&mut self, factor: f64) {
        self.width *= factor;
        self.height *= factor;
    }

    // Method consuming self
    pub fn into_square(self) -> Rectangle {
        let side = (self.width + self.height) / 2.0;
        Rectangle::new(side, side)
    }

    // Associated constant
    pub const UNIT: Rectangle = Rectangle { width: 1.0, height: 1.0 };
}

// Traits
pub trait Shape {
    fn area(&self) -> f64;
    fn perimeter(&self) -> f64;

    // Default implementation
    fn describe(&self) -> String {
        format!("Area: {:.2}, Perimeter: {:.2}", self.area(), self.perimeter())
    }
}

pub trait Drawable: Shape {
    fn draw(&self);
}

// Trait with associated types
pub trait Container {
    type Item;
    type Error;

    fn get(&self, index: usize) -> Option<&Self::Item>;
    fn len(&self) -> usize;
    fn is_empty(&self) -> bool {
        self.len() == 0
    }
}

// Trait with generic parameters
pub trait Converter<T> {
    fn convert(&self) -> T;
}

// Trait with associated constants
pub trait Bounded {
    const MIN: Self;
    const MAX: Self;
}

// Implementing traits
impl Shape for Rectangle {
    fn area(&self) -> f64 {
        self.width * self.height
    }

    fn perimeter(&self) -> f64 {
        2.0 * (self.width + self.height)
    }
}

impl Drawable for Rectangle {
    fn draw(&self) {
        println!("Drawing rectangle: {:?}", self);
    }
}

// Operator overloading
impl Add for Meters {
    type Output = Self;

    fn add(self, other: Self) -> Self::Output {
        Meters(self.0 + other.0)
    }
}

impl Deref for Meters {
    type Target = f64;

    fn deref(&self) -> &Self::Target {
        &self.0
    }
}

// Generic functions
fn print_type<T: Debug>(value: &T) {
    println!("{:?}", value);
}

fn largest<T: PartialOrd>(list: &[T]) -> Option<&T> {
    list.iter().max_by(|a, b| a.partial_cmp(b).unwrap_or(Ordering::Equal))
}

// Where clause
fn process<T, U>(t: T, u: U) -> String
where
    T: Display + Clone,
    U: Debug + Default,
{
    format!("{} - {:?}", t, u)
}

// impl Trait
fn make_iterator() -> impl Iterator<Item = i32> {
    (0..10).filter(|x| x % 2 == 0)
}

fn accept_closure(f: impl Fn(i32) -> i32) -> i32 {
    f(10)
}

// Closures
fn closure_examples() {
    // Closure with type annotations
    let add: fn(i32, i32) -> i32 = |a, b| a + b;

    // Closure capturing environment
    let multiplier = 5;
    let multiply = |x| x * multiplier;

    // Closure with move
    let data = vec![1, 2, 3];
    let owns_data = move || println!("{:?}", data);

    // FnOnce, FnMut, Fn
    let consume = || {
        let s = String::from("consumed");
        s
    };

    let mut counter = 0;
    let mut increment = || {
        counter += 1;
        counter
    };

    let borrow = || println!("{}", multiplier);
}

// Error handling
#[derive(Debug)]
pub struct CustomError {
    message: String,
    code: i32,
}

impl Display for CustomError {
    fn fmt(&self, f: &mut Formatter<'_>) -> fmt::Result {
        write!(f, "Error {}: {}", self.code, self.message)
    }
}

impl Error for CustomError {}

impl From<io::Error> for CustomError {
    fn from(err: io::Error) -> Self {
        CustomError {
            message: err.to_string(),
            code: -1,
        }
    }
}

fn might_fail(succeed: bool) -> Result<i32> {
    if succeed {
        Ok(42)
    } else {
        Err(Box::new(CustomError {
            message: "Operation failed".to_string(),
            code: 1,
        }))
    }
}

fn propagate_error() -> Result<String> {
    let value = might_fail(true)?;
    Ok(format!("Success: {}", value))
}

// Pattern matching
fn pattern_matching_examples(message: Message) {
    // Match expression
    match message {
        Message::Quit => println!("Quit"),
        Message::Move { x, y } => println!("Move to ({}, {})", x, y),
        Message::Write(text) => println!("Write: {}", text),
        Message::ChangeColor(Color::Rgb(r, g, b)) => {
            println!("Change to RGB({}, {}, {})", r, g, b)
        }
        Message::ChangeColor(color) => println!("Change to {:?}", color),
    }

    // If let
    let optional = Some(42);
    if let Some(value) = optional {
        println!("Got value: {}", value);
    }

    // While let
    let mut stack = vec![1, 2, 3];
    while let Some(top) = stack.pop() {
        println!("Popped: {}", top);
    }

    // Let else
    let Some(value) = optional else {
        panic!("Expected Some");
    };

    // Match guards
    let num = Some(4);
    match num {
        Some(x) if x < 5 => println!("Less than 5: {}", x),
        Some(x) => println!("Greater or equal to 5: {}", x),
        None => println!("No value"),
    }

    // @ bindings
    match num {
        Some(n @ 1..=5) => println!("Got {} in range 1-5", n),
        Some(n) => println!("Got {}", n),
        None => println!("None"),
    }

    // Destructuring
    let point = Point::new(1, 2, 3);
    let Point { x, y, z } = point;

    let (a, b, c) = (1, 2, 3);
    let [first, second, ..] = [1, 2, 3, 4, 5];
}

// Iterators
fn iterator_examples() {
    let numbers = vec![1, 2, 3, 4, 5];

    // Map
    let doubled: Vec<_> = numbers.iter().map(|x| x * 2).collect();

    // Filter
    let evens: Vec<_> = numbers.iter().filter(|&&x| x % 2 == 0).collect();

    // Filter map
    let parsed: Vec<i32> = ["1", "two", "3"]
        .iter()
        .filter_map(|s| s.parse().ok())
        .collect();

    // Fold
    let sum: i32 = numbers.iter().fold(0, |acc, x| acc + x);

    // Chaining
    let result: i32 = numbers
        .iter()
        .filter(|&&x| x > 2)
        .map(|x| x * 2)
        .take(2)
        .sum();

    // Enumerate
    for (index, value) in numbers.iter().enumerate() {
        println!("{}: {}", index, value);
    }

    // Zip
    let names = vec!["Alice", "Bob", "Charlie"];
    let ages = vec![25, 30, 35];
    for (name, age) in names.iter().zip(ages.iter()) {
        println!("{} is {} years old", name, age);
    }

    // Flatten
    let nested = vec![vec![1, 2], vec![3, 4]];
    let flat: Vec<_> = nested.into_iter().flatten().collect();

    // Partition
    let (evens, odds): (Vec<_>, Vec<_>) = numbers.iter().partition(|&&x| x % 2 == 0);
}

// Async/Await
async fn async_function() -> i32 {
    // Simulating async operation
    42
}

async fn fetch_data(url: &str) -> Result<String> {
    // Async operations
    Ok(format!("Data from {}", url))
}

async fn concurrent_operations() {
    let future1 = async_function();
    let future2 = async_function();

    // Await multiple futures
    let (result1, result2) = (future1.await, future2.await);
}

// Macros
macro_rules! say_hello {
    () => {
        println!("Hello!");
    };
    ($name:expr) => {
        println!("Hello, {}!", $name);
    };
}

macro_rules! create_function {
    ($func_name:ident) => {
        fn $func_name() {
            println!("Function {:?} called", stringify!($func_name));
        }
    };
}

macro_rules! vec_of_strings {
    ($($x:expr),* $(,)?) => {
        vec![$($x.to_string()),*]
    };
}

// Procedural macro-like pattern (attribute simulation)
#[derive(Debug, Clone)]
#[allow(unused)]
struct MacroExample {
    #[doc = "A field"]
    field: i32,
}

// Unsafe code
fn unsafe_examples() {
    // Raw pointers
    let mut value = 42;
    let raw_ptr: *const i32 = &value;
    let raw_mut_ptr: *mut i32 = &mut value;

    unsafe {
        println!("Raw pointer value: {}", *raw_ptr);
        *raw_mut_ptr = 100;

        // Calling unsafe function
        dangerous_function();

        // Accessing mutable static
        MUTABLE_STATIC = 10;
    }

    // FFI
    extern "C" {
        fn abs(input: i32) -> i32;
    }

    unsafe {
        println!("Absolute value: {}", abs(-5));
    }
}

unsafe fn dangerous_function() {
    // Unsafe operations
}

// Union
#[repr(C)]
union FloatOrInt {
    f: f32,
    i: i32,
}

// Smart pointers
fn smart_pointer_examples() {
    // Box
    let boxed: Box<i32> = Box::new(42);
    let recursive = Box::new(Message::Write("Hello".to_string()));

    // Rc
    let shared: Rc<String> = Rc::new("shared".to_string());
    let clone1 = Rc::clone(&shared);
    let clone2 = Rc::clone(&shared);

    // Arc
    let thread_safe: Arc<Mutex<i32>> = Arc::new(Mutex::new(0));

    // RefCell
    let interior_mut: RefCell<i32> = RefCell::new(42);
    *interior_mut.borrow_mut() += 1;

    // Cell
    let cell: Cell<i32> = Cell::new(42);
    cell.set(100);

    // Weak
    let strong = Rc::new("data".to_string());
    let weak = Rc::downgrade(&strong);
}

// Concurrency
fn concurrency_examples() {
    // Spawning threads
    let handle = thread::spawn(|| {
        println!("Hello from thread!");
        42
    });

    let result = handle.join().unwrap();

    // Sharing data between threads
    let counter = Arc::new(Mutex::new(0));
    let mut handles = vec![];

    for _ in 0..10 {
        let counter = Arc::clone(&counter);
        let handle = thread::spawn(move || {
            let mut num = counter.lock().unwrap();
            *num += 1;
        });
        handles.push(handle);
    }

    for handle in handles {
        handle.join().unwrap();
    }

    // Channels
    let (tx, rx) = mpsc::channel();
    let tx2 = tx.clone();

    thread::spawn(move || {
        tx.send("Hello").unwrap();
    });

    thread::spawn(move || {
        tx2.send("World").unwrap();
    });

    for msg in rx {
        println!("Received: {}", msg);
    }

    // RwLock
    let data = Arc::new(RwLock::new(vec![1, 2, 3]));

    {
        let read = data.read().unwrap();
        println!("Read: {:?}", *read);
    }

    {
        let mut write = data.write().unwrap();
        write.push(4);
    }
}

// Lifetimes
fn lifetime_examples<'a, 'b: 'a>(x: &'a str, y: &'b str) -> &'a str {
    if x.len() > y.len() { x } else { y }
}

struct LifetimeStruct<'a> {
    data: &'a str,
}

impl<'a> LifetimeStruct<'a> {
    fn new(data: &'a str) -> Self {
        Self { data }
    }

    fn get(&self) -> &str {
        self.data
    }
}

// HRTB (Higher-Ranked Trait Bounds)
fn with_hrtb<F>(f: F)
where
    F: for<'a> Fn(&'a str) -> &'a str,
{
    let s = String::from("hello");
    let result = f(&s);
    println!("{}", result);
}

// Main function
fn main() {
    // Variable bindings
    let immutable = 42;
    let mut mutable = 0;
    mutable += 1;

    // Type annotations
    let explicit: i32 = 42;
    let inferred = 42_i64;

    // Shadowing
    let x = 5;
    let x = x + 1;
    let x = x * 2;

    // Numeric literals
    let decimal = 98_222;
    let hex = 0xff;
    let octal = 0o77;
    let binary = 0b1111_0000;
    let byte = b'A';
    let float = 3.14_f64;
    let scientific = 1.2e-4;

    // String types
    let string_literal: &str = "Hello, World!";
    let string_owned: String = String::from("Hello");
    let raw_string = r#"Raw string with "quotes""#;
    let byte_string = b"Byte string";

    // Char
    let c: char = 'A';
    let emoji = '🦀';
    let unicode = '\u{1F600}';

    // Boolean
    let t: bool = true;
    let f: bool = false;

    // Tuples
    let tuple: (i32, f64, &str) = (42, 3.14, "hello");
    let (a, b, c) = tuple;
    let first = tuple.0;

    // Arrays
    let array: [i32; 5] = [1, 2, 3, 4, 5];
    let repeated = [0; 10];
    let slice = &array[1..3];

    // Vectors
    let mut vec = vec![1, 2, 3];
    vec.push(4);
    vec.pop();

    // HashMap
    let mut map = HashMap::new();
    map.insert("key", "value");
    map.entry("key2").or_insert("value2");

    // Control flow
    if immutable > 0 {
        println!("Positive");
    } else if immutable < 0 {
        println!("Negative");
    } else {
        println!("Zero");
    }

    // If as expression
    let condition = true;
    let value = if condition { 5 } else { 6 };

    // Loop
    let mut counter = 0;
    let result = loop {
        counter += 1;
        if counter == 10 {
            break counter * 2;
        }
    };

    // Labeled loops
    'outer: for i in 0..10 {
        'inner: for j in 0..10 {
            if i * j > 50 {
                break 'outer;
            }
        }
    }

    // While
    while mutable < 10 {
        mutable += 1;
    }

    // For
    for i in 0..10 {
        println!("{}", i);
    }

    for element in array.iter() {
        println!("{}", element);
    }

    // Range expressions
    let range = 0..10;
    let range_inclusive = 0..=10;
    let range_from = 5..;
    let range_to = ..5;

    // Struct instantiation
    let rect = Rectangle::new(10.0, 20.0);
    println!("Area: {}", rect.area());

    // Using traits
    let shape: &dyn Shape = &rect;
    println!("{}", shape.describe());

    // Pattern matching
    pattern_matching_examples(Message::Write("Test".to_string()));

    // Error handling
    match might_fail(true) {
        Ok(v) => println!("Success: {}", v),
        Err(e) => eprintln!("Error: {}", e),
    }

    // Option handling
    let opt = Some(42);
    let unwrapped = opt.unwrap_or(0);
    let mapped = opt.map(|x| x * 2);
    let and_then = opt.and_then(|x| Some(x.to_string()));

    // Macros
    say_hello!();
    say_hello!("Rust");

    let strings = vec_of_strings!["hello", "world"];

    println!("Hello, Rust!");
    eprintln!("This is stderr");

    // Format macros
    let formatted = format!("Value: {}", 42);
    let debug_fmt = format!("{:?}", rect);
    let padded = format!("{:>10}", "right");
    let precision = format!("{:.2}", 3.14159);

    // Assert macros
    assert!(true);
    assert_eq!(1 + 1, 2);
    assert_ne!(1, 2);
    debug_assert!(true);

    // todo and unimplemented
    // todo!("Implement this");
    // unimplemented!("Not yet implemented");

    // Iterators
    iterator_examples();

    // Smart pointers
    smart_pointer_examples();

    // Concurrency
    // concurrency_examples();

    // Closures
    closure_examples();

    // Unsafe
    unsafe_examples();

    println!("Program completed successfully!");
}

// Tests
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_rectangle_area() {
        let rect = Rectangle::new(10.0, 20.0);
        assert_eq!(rect.area(), 200.0);
    }

    #[test]
    #[should_panic(expected = "panic message")]
    fn test_panic() {
        panic!("panic message");
    }

    #[test]
    fn test_result() -> Result<()> {
        let value = might_fail(true)?;
        assert_eq!(value, 42);
        Ok(())
    }

    #[ignore]
    #[test]
    fn expensive_test() {
        // Long-running test
    }
}

// Benchmarks (requires nightly)
#[cfg(feature = "bench")]
mod benchmarks {
    use super::*;
    use test::Bencher;

    #[bench]
    fn bench_area(b: &mut Bencher) {
        let rect = Rectangle::new(10.0, 20.0);
        b.iter(|| rect.area());
    }
}
