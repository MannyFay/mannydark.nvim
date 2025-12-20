//! Comprehensive Zig language sample demonstrating all syntax features
//! Zig is a systems programming language focused on safety and performance

const std = @import("std");
const builtin = @import("builtin");
const mem = std.mem;
const math = std.math;
const debug = std.debug;
const testing = std.testing;
const Allocator = std.mem.Allocator;
const ArrayList = std.ArrayList;
const HashMap = std.HashMap;

// ============================================================================
// Constants and Compile-time Values
// ============================================================================

/// Public constant with documentation
pub const MAX_BUFFER_SIZE: usize = 1024;
const PI: f64 = 3.14159265358979323846;
const GREETING: []const u8 = "Hello, Zig!";

// Compile-time computation
const computed_value = blk: {
    var result: u32 = 0;
    for (0..10) |i| {
        result += @intCast(i);
    }
    break :blk result;
};

// Compile-time string formatting
const formatted = std.fmt.comptimePrint("Value: {d}", .{computed_value});

// ============================================================================
// Type Declarations
// ============================================================================

/// Custom error set
const FileError = error{
    FileNotFound,
    PermissionDenied,
    IoError,
    InvalidData,
};

/// Combined error set
const AppError = FileError || std.mem.Allocator.Error;

/// Enum type
const Color = enum(u8) {
    red = 0,
    green = 1,
    blue = 2,
    alpha = 255,

    const Self = @This();

    pub fn toRgb(self: Self) [3]u8 {
        return switch (self) {
            .red => .{ 255, 0, 0 },
            .green => .{ 0, 255, 0 },
            .blue => .{ 0, 0, 255 },
            .alpha => .{ 0, 0, 0 },
        };
    }

    pub fn fromString(str: []const u8) ?Self {
        const map = std.ComptimeStringMap(Self, .{
            .{ "red", .red },
            .{ "green", .green },
            .{ "blue", .blue },
        });
        return map.get(str);
    }
};

/// Tagged union
const Value = union(enum) {
    int: i64,
    float: f64,
    boolean: bool,
    string: []const u8,
    array: []const Value,
    none,

    const Self = @This();

    pub fn format(
        self: Self,
        comptime fmt: []const u8,
        options: std.fmt.FormatOptions,
        writer: anytype,
    ) !void {
        _ = fmt;
        _ = options;
        switch (self) {
            .int => |v| try writer.print("{d}", .{v}),
            .float => |v| try writer.print("{d:.2}", .{v}),
            .boolean => |v| try writer.print("{}", .{v}),
            .string => |v| try writer.print("\"{s}\"", .{v}),
            .none => try writer.writeAll("null"),
            .array => |arr| {
                try writer.writeAll("[");
                for (arr, 0..) |item, i| {
                    if (i > 0) try writer.writeAll(", ");
                    try item.format("", .{}, writer);
                }
                try writer.writeAll("]");
            },
        }
    }
};

/// Struct with fields and methods
const Point = struct {
    x: f64,
    y: f64,
    z: f64 = 0.0, // Default value

    const Self = @This();

    /// Constructor
    pub fn init(x: f64, y: f64, z: f64) Self {
        return .{ .x = x, .y = y, .z = z };
    }

    /// Calculate distance to another point
    pub fn distance(self: Self, other: Self) f64 {
        const dx = self.x - other.x;
        const dy = self.y - other.y;
        const dz = self.z - other.z;
        return @sqrt(dx * dx + dy * dy + dz * dz);
    }

    /// Scale point by factor
    pub fn scale(self: *Self, factor: f64) void {
        self.x *= factor;
        self.y *= factor;
        self.z *= factor;
    }

    /// Add two points
    pub fn add(self: Self, other: Self) Self {
        return .{
            .x = self.x + other.x,
            .y = self.y + other.y,
            .z = self.z + other.z,
        };
    }
};

/// Packed struct for memory layout control
const PackedData = packed struct {
    flags: u4,
    id: u12,
    data: u16,
};

/// Extern struct for C interop
const CStruct = extern struct {
    value: c_int,
    pointer: ?*anyopaque,
};

// ============================================================================
// Generic Types and Functions
// ============================================================================

/// Generic stack implementation
fn Stack(comptime T: type) type {
    return struct {
        items: []T,
        count: usize,
        allocator: Allocator,

        const Self = @This();

        pub fn init(allocator: Allocator) Self {
            return .{
                .items = &[_]T{},
                .count = 0,
                .allocator = allocator,
            };
        }

        pub fn deinit(self: *Self) void {
            self.allocator.free(self.items);
        }

        pub fn push(self: *Self, item: T) !void {
            if (self.count >= self.items.len) {
                const new_capacity = if (self.items.len == 0) 8 else self.items.len * 2;
                self.items = try self.allocator.realloc(self.items, new_capacity);
            }
            self.items[self.count] = item;
            self.count += 1;
        }

        pub fn pop(self: *Self) ?T {
            if (self.count == 0) return null;
            self.count -= 1;
            return self.items[self.count];
        }

        pub fn peek(self: Self) ?T {
            if (self.count == 0) return null;
            return self.items[self.count - 1];
        }
    };
}

/// Generic linked list node
fn LinkedListNode(comptime T: type) type {
    return struct {
        data: T,
        next: ?*@This() = null,
        prev: ?*@This() = null,
    };
}

// ============================================================================
// Functions
// ============================================================================

/// Simple function with documentation
pub fn add(a: i32, b: i32) i32 {
    return a + b;
}

/// Function with error handling
fn divide(a: f64, b: f64) !f64 {
    if (b == 0) {
        return error.DivisionByZero;
    }
    return a / b;
}

/// Function with optional return
fn find(haystack: []const u8, needle: u8) ?usize {
    for (haystack, 0..) |byte, index| {
        if (byte == needle) {
            return index;
        }
    }
    return null;
}

/// Inline function
inline fn square(x: anytype) @TypeOf(x) {
    return x * x;
}

/// Function with comptime parameters
fn repeat(comptime count: usize, value: u8) [count]u8 {
    var result: [count]u8 = undefined;
    for (&result) |*byte| {
        byte.* = value;
    }
    return result;
}

/// Variadic function using tuple
fn sum(args: anytype) @TypeOf(args[0]) {
    var total: @TypeOf(args[0]) = 0;
    inline for (args) |arg| {
        total += arg;
    }
    return total;
}

/// Function pointer type
const BinaryOp = *const fn (i32, i32) i32;

fn applyOp(a: i32, b: i32, op: BinaryOp) i32 {
    return op(a, b);
}

/// Extern function declaration (C interop)
extern "c" fn printf(format: [*:0]const u8, ...) c_int;

/// Export function for C
export fn exported_function(value: c_int) c_int {
    return value * 2;
}

/// Naked function (no prologue/epilogue)
fn nakedFunction() callconv(.Naked) void {
    asm volatile ("ret");
}

// ============================================================================
// Control Flow
// ============================================================================

fn controlFlowExamples() void {
    // If expression
    const x: i32 = 10;
    const abs_x = if (x >= 0) x else -x;
    _ = abs_x;

    // If with capture
    const optional: ?i32 = 42;
    if (optional) |value| {
        std.debug.print("Value: {d}\n", .{value});
    } else {
        std.debug.print("No value\n", .{});
    }

    // If with error handling
    const result = divide(10, 2) catch |err| {
        std.debug.print("Error: {}\n", .{err});
        return;
    };
    _ = result;

    // Switch expression
    const color = Color.red;
    const rgb = switch (color) {
        .red => [3]u8{ 255, 0, 0 },
        .green => [3]u8{ 0, 255, 0 },
        .blue => [3]u8{ 0, 0, 255 },
        .alpha => [3]u8{ 0, 0, 0 },
    };
    _ = rgb;

    // Switch with ranges
    const grade: u8 = 85;
    const letter = switch (grade) {
        0...59 => 'F',
        60...69 => 'D',
        70...79 => 'C',
        80...89 => 'B',
        90...100 => 'A',
        else => '?',
    };
    _ = letter;

    // While loop
    var i: usize = 0;
    while (i < 10) : (i += 1) {
        if (i == 5) continue;
        if (i == 8) break;
    }

    // While with optional
    var maybe_value: ?i32 = 10;
    while (maybe_value) |v| {
        std.debug.print("{d}\n", .{v});
        maybe_value = if (v > 0) v - 1 else null;
    }

    // For loop
    const items = [_]i32{ 1, 2, 3, 4, 5 };
    for (items, 0..) |item, index| {
        std.debug.print("items[{d}] = {d}\n", .{ index, item });
    }

    // For with pointer
    var mutable_items = [_]i32{ 1, 2, 3, 4, 5 };
    for (&mutable_items) |*item| {
        item.* *= 2;
    }

    // Labeled blocks
    const value = outer: {
        const inner_result = inner: {
            break :inner 42;
        };
        break :outer inner_result;
    };
    _ = value;

    // Inline for (compile-time loop)
    const tuple = .{ 1, "hello", 3.14 };
    inline for (tuple) |field| {
        std.debug.print("{}\n", .{field});
    }
}

// ============================================================================
// Error Handling
// ============================================================================

fn errorHandlingExamples(allocator: Allocator) void {
    // Try operator
    const result = readFile(allocator, "test.txt") catch |err| {
        std.debug.print("Failed to read file: {}\n", .{err});
        return;
    };
    defer allocator.free(result);

    // Error union with capture
    if (divide(10, 0)) |value| {
        std.debug.print("Result: {d}\n", .{value});
    } else |err| {
        std.debug.print("Error: {}\n", .{err});
    }

    // Unreachable for impossible error
    const safe_result = divide(10, 2) catch unreachable;
    _ = safe_result;

    // Error trace
    errdefer std.debug.print("Cleaning up after error\n", .{});

    // Propagate error
    _ = propagateError() catch return;
}

fn readFile(allocator: Allocator, path: []const u8) ![]u8 {
    const file = std.fs.cwd().openFile(path, .{}) catch |err| {
        return err;
    };
    defer file.close();

    const stat = try file.stat();
    const contents = try file.readToEndAlloc(allocator, stat.size);
    return contents;
}

fn propagateError() !void {
    try mayFail();
    try mayFail();
}

fn mayFail() !void {
    return error.SomethingWentWrong;
}

// ============================================================================
// Memory Management
// ============================================================================

fn memoryExamples(allocator: Allocator) !void {
    // Single allocation
    const ptr = try allocator.create(i32);
    defer allocator.destroy(ptr);
    ptr.* = 42;

    // Array allocation
    const array = try allocator.alloc(u8, 100);
    defer allocator.free(array);
    @memset(array, 0);

    // Reallocation
    var dynamic = try allocator.alloc(u8, 10);
    dynamic = try allocator.realloc(dynamic, 20);
    allocator.free(dynamic);

    // Fixed buffer allocator
    var buffer: [1024]u8 = undefined;
    var fba = std.heap.FixedBufferAllocator.init(&buffer);
    const fba_allocator = fba.allocator();
    const temp = try fba_allocator.alloc(u8, 100);
    _ = temp;

    // Arena allocator
    var arena = std.heap.ArenaAllocator.init(allocator);
    defer arena.deinit();
    const arena_allocator = arena.allocator();
    _ = try arena_allocator.alloc(u8, 100);
    // No need to free individual allocations

    // Sentinel-terminated array
    const sentinel_array = try allocator.allocSentinel(u8, 10, 0);
    defer allocator.free(sentinel_array);
}

// ============================================================================
// Pointers and Slices
// ============================================================================

fn pointerExamples() void {
    // Single-item pointer
    var value: i32 = 42;
    const ptr: *i32 = &value;
    ptr.* = 100;

    // Const pointer
    const const_ptr: *const i32 = &value;
    _ = const_ptr.*;

    // Many-item pointer
    var array = [_]i32{ 1, 2, 3, 4, 5 };
    const many_ptr: [*]i32 = &array;
    many_ptr[0] = 10;

    // Slice
    const slice: []i32 = &array;
    for (slice) |*item| {
        item.* *= 2;
    }

    // Sentinel-terminated pointer
    const string: [*:0]const u8 = "Hello";
    const len = std.mem.len(string);
    _ = len;

    // Optional pointer
    var opt_ptr: ?*i32 = &value;
    if (opt_ptr) |p| {
        p.* = 200;
    }
    opt_ptr = null;

    // Pointer arithmetic
    const ptr2 = many_ptr + 2;
    const diff = @intFromPtr(ptr2) - @intFromPtr(many_ptr);
    _ = diff;

    // Align cast
    const aligned: *align(16) i32 = @alignCast(&value);
    _ = aligned;
}

// ============================================================================
// Comptime and Metaprogramming
// ============================================================================

fn comptimeExamples() void {
    // Compile-time evaluation
    comptime {
        var sum_val: i32 = 0;
        for (0..10) |i| {
            sum_val += @intCast(i);
        }
        if (sum_val != 45) @compileError("Sum should be 45");
    }

    // Type introspection
    const T = i32;
    const type_info = @typeInfo(T);
    switch (type_info) {
        .Int => |int_info| {
            std.debug.print("Int with {d} bits\n", .{int_info.bits});
        },
        else => {},
    }

    // Field iteration
    const MyStruct = struct {
        a: i32,
        b: []const u8,
        c: f64,
    };

    inline for (std.meta.fields(MyStruct)) |field| {
        std.debug.print("Field: {s}\n", .{field.name});
    }

    // Type coercion
    const float_val: f32 = 3.14;
    const int_val: i32 = @intFromFloat(float_val);
    _ = int_val;

    // Bit casting
    const bits: u32 = @bitCast(float_val);
    _ = bits;
}

/// Compile-time function
fn fibonacci(comptime n: usize) usize {
    if (n < 2) return n;
    return fibonacci(n - 1) + fibonacci(n - 2);
}

/// Compile-time type generation
fn Matrix(comptime T: type, comptime rows: usize, comptime cols: usize) type {
    return struct {
        data: [rows][cols]T,

        const Self = @This();

        pub fn init(value: T) Self {
            var result: Self = undefined;
            for (&result.data) |*row| {
                for (row) |*cell| {
                    cell.* = value;
                }
            }
            return result;
        }

        pub fn get(self: Self, row: usize, col: usize) T {
            return self.data[row][col];
        }

        pub fn set(self: *Self, row: usize, col: usize, value: T) void {
            self.data[row][col] = value;
        }
    };
}

// ============================================================================
// Async/Await (Suspended)
// ============================================================================

// Note: Async in Zig is currently being reworked
// This shows the syntax that was available

// fn asyncExample() void {
//     var frame: @Frame(asyncTask) = async asyncTask();
//     const result = await frame;
//     _ = result;
// }

// fn asyncTask() i32 {
//     return 42;
// }

// ============================================================================
// Testing
// ============================================================================

test "basic addition" {
    try testing.expectEqual(@as(i32, 3), add(1, 2));
}

test "point distance" {
    const p1 = Point.init(0, 0, 0);
    const p2 = Point.init(3, 4, 0);
    try testing.expectApproxEqAbs(@as(f64, 5.0), p1.distance(p2), 0.001);
}

test "stack operations" {
    var stack = Stack(i32).init(testing.allocator);
    defer stack.deinit();

    try stack.push(1);
    try stack.push(2);
    try stack.push(3);

    try testing.expectEqual(@as(?i32, 3), stack.pop());
    try testing.expectEqual(@as(?i32, 2), stack.pop());
    try testing.expectEqual(@as(?i32, 1), stack.pop());
    try testing.expectEqual(@as(?i32, null), stack.pop());
}

test "error handling" {
    try testing.expectError(error.DivisionByZero, divide(1, 0));

    const result = try divide(10, 2);
    try testing.expectApproxEqAbs(@as(f64, 5.0), result, 0.001);
}

test "memory allocation" {
    const allocator = testing.allocator;

    const data = try allocator.alloc(u8, 100);
    defer allocator.free(data);

    @memset(data, 'A');
    try testing.expect(data[0] == 'A');
    try testing.expect(data[99] == 'A');
}

// ============================================================================
// Main Entry Point
// ============================================================================

pub fn main() !void {
    // Get allocator
    var gpa = std.heap.GeneralPurposeAllocator(.{}){};
    defer _ = gpa.deinit();
    const allocator = gpa.allocator();

    // Standard output
    const stdout = std.io.getStdOut().writer();

    try stdout.print("Hello, {s}!\n", .{"Zig"});

    // Variable declarations
    var mutable: i32 = 42;
    const immutable: i32 = 100;
    _ = immutable;

    // Numeric literals
    const decimal: i32 = 42;
    const hex: u32 = 0xDEAD_BEEF;
    const octal: u32 = 0o755;
    const binary: u8 = 0b1010_1010;
    const float: f64 = 3.14159;
    const scientific: f64 = 1.23e-4;
    _ = decimal;
    _ = hex;
    _ = octal;
    _ = binary;
    _ = float;
    _ = scientific;

    // String literals
    const string = "Hello, World!";
    const multiline =
        \\This is a
        \\multiline string
        \\in Zig
    ;
    const with_escapes = "Tab:\t Newline:\n Quote:\"";
    _ = string;
    _ = multiline;
    _ = with_escapes;

    // Character literal
    const char: u8 = 'A';
    const unicode: u21 = '🦎';
    _ = char;
    _ = unicode;

    // Arrays
    const array = [_]i32{ 1, 2, 3, 4, 5 };
    const repeated_arr = [_]u8{0} ** 10;
    _ = array;
    _ = repeated_arr;

    // Struct instance
    const point = Point.init(1, 2, 3);
    try stdout.print("Point: ({d}, {d}, {d})\n", .{ point.x, point.y, point.z });

    // Enum usage
    const color = Color.red;
    const rgb = color.toRgb();
    try stdout.print("RGB: ({d}, {d}, {d})\n", .{ rgb[0], rgb[1], rgb[2] });

    // Union usage
    const value = Value{ .int = 42 };
    try stdout.print("Value: {}\n", .{value});

    // Generic stack
    var stack = Stack(i32).init(allocator);
    defer stack.deinit();

    try stack.push(1);
    try stack.push(2);
    try stack.push(3);

    while (stack.pop()) |v| {
        try stdout.print("Popped: {d}\n", .{v});
    }

    // Matrix type
    const Mat3x3 = Matrix(f32, 3, 3);
    var matrix = Mat3x3.init(0);
    matrix.set(0, 0, 1);
    matrix.set(1, 1, 1);
    matrix.set(2, 2, 1);

    // Compile-time fibonacci
    const fib10 = comptime fibonacci(10);
    try stdout.print("Fibonacci(10) = {d}\n", .{fib10});

    // ArrayList
    var list = ArrayList(i32).init(allocator);
    defer list.deinit();

    try list.append(1);
    try list.append(2);
    try list.append(3);

    for (list.items) |item| {
        try stdout.print("List item: {d}\n", .{item});
    }

    // HashMap
    var map = std.AutoHashMap([]const u8, i32).init(allocator);
    defer map.deinit();

    try map.put("one", 1);
    try map.put("two", 2);
    try map.put("three", 3);

    if (map.get("two")) |v| {
        try stdout.print("Map value: {d}\n", .{v});
    }

    // Control flow examples
    controlFlowExamples();

    // Pointer examples
    pointerExamples();

    // Comptime examples
    comptimeExamples();

    // Error handling examples
    errorHandlingExamples(allocator);

    // Memory examples
    try memoryExamples(allocator);

    // Builtin functions
    const type_name = @typeName(Point);
    const size = @sizeOf(Point);
    const align_of = @alignOf(Point);
    try stdout.print("Type: {s}, Size: {d}, Align: {d}\n", .{ type_name, size, align_of });

    // Atomics
    var atomic_value: std.atomic.Value(i32) = std.atomic.Value(i32).init(0);
    _ = atomic_value.fetchAdd(1, .seq_cst);
    const loaded = atomic_value.load(.seq_cst);
    try stdout.print("Atomic value: {d}\n", .{loaded});

    // Increment mutable
    mutable += 1;
    try stdout.print("Mutable: {d}\n", .{mutable});

    try stdout.print("Program completed successfully!\n", .{});
}
