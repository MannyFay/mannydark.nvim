/**
 * Comprehensive JavaScript Sample - Syntax Highlighting Demonstration
 *
 * This file demonstrates all major JavaScript language features
 * for syntax highlighting purposes.
 *
 * @fileoverview Complete JavaScript syntax showcase
 * @author Sample Author
 * @version 1.0.0
 */

"use strict";

// =============================================================================
// Literals and Primitive Types
// =============================================================================

// Number literals
const integerLiteral = 42;
const negativeLiteral = -17;
const floatLiteral = 3.14159;
const scientific = 6.022e23;
const hex = 0xff;
const octal = 0o755;
const binary = 0b101010;
const bigInt = 9007199254740991n;
const separators = 1_000_000;
const infinity = Infinity;
const negInfinity = -Infinity;
const notANumber = NaN;

// String literals
const singleQuoted = 'Hello, JavaScript!';
const doubleQuoted = "Hello, JavaScript!";
const escaped = "Line 1\nLine 2\tTabbed";
const unicode = "Hello, 世界! \u{1F600}";
const templateLiteral = `Hello, ${singleQuoted}!`;
const multilineTemplate = `
  This is a multi-line
  template literal with
  interpolation: ${42 * 2}
`;
const taggedTemplate = String.raw`Raw \n string`;

// Boolean and special values
const trueValue = true;
const falseValue = false;
const nullValue = null;
const undefinedValue = undefined;

// Symbol
const symbolValue = Symbol("description");
const symbolFor = Symbol.for("global.symbol");
const wellKnownSymbol = Symbol.iterator;

// Regular expressions
const regexLiteral = /pattern/gi;
const regexWithFlags = /\w+\s*=\s*\d+/gmu;
const regexGroups = /(?<name>\w+)@(?<domain>\w+\.\w+)/;

// =============================================================================
// Variables and Declarations
// =============================================================================

// Variable declarations
var oldStyleVar = "var is function-scoped";
let blockScoped = "let is block-scoped";
const constant = "const cannot be reassigned";

// Destructuring
const [first, second, ...rest] = [1, 2, 3, 4, 5];
const { name, age = 0, email: userEmail } = { name: "Alice", email: "alice@example.com" };
const { nested: { deep } } = { nested: { deep: "value" } };

// Spread operator
const arraySpread = [...[1, 2, 3], ...[4, 5, 6]];
const objectSpread = { ...{ a: 1 }, ...{ b: 2 } };

// =============================================================================
// Data Structures
// =============================================================================

// Arrays
const array = [1, 2, 3, 4, 5];
const sparseArray = [1, , , 4];
const mixedArray = [1, "two", true, null, { key: "value" }];
const nestedArray = [[1, 2], [3, 4], [5, 6]];

// Array methods
const mapped = array.map((x) => x * 2);
const filtered = array.filter((x) => x % 2 === 0);
const reduced = array.reduce((acc, x) => acc + x, 0);
const found = array.find((x) => x > 3);
const every = array.every((x) => x > 0);
const some = array.some((x) => x > 4);
const flat = nestedArray.flat();
const sorted = [...array].sort((a, b) => b - a);

// Objects
const object = {
  // Property shorthand
  name,
  age,

  // Computed property name
  ["computed" + "Key"]: "value",

  // Method shorthand
  greet() {
    return `Hello, ${this.name}!`;
  },

  // Getter and setter
  get fullName() {
    return `${this.firstName} ${this.lastName}`;
  },

  set fullName(value) {
    [this.firstName, this.lastName] = value.split(" ");
  },

  // Generator method
  *items() {
    yield 1;
    yield 2;
    yield 3;
  },

  // Async method
  async fetchData() {
    return await fetch("/api/data");
  },
};

// Map
const map = new Map([
  ["key1", "value1"],
  ["key2", "value2"],
]);
map.set("key3", "value3");
map.get("key1");
map.has("key2");
map.delete("key3");

// Set
const set = new Set([1, 2, 3, 3, 4]);
set.add(5);
set.has(3);
set.delete(4);

// WeakMap and WeakSet
const weakMap = new WeakMap();
const weakSet = new WeakSet();

// =============================================================================
// Functions
// =============================================================================

// Function declaration
function regularFunction(a, b) {
  return a + b;
}

// Function expression
const functionExpression = function (a, b) {
  return a * b;
};

// Named function expression
const namedFunction = function factorial(n) {
  return n <= 1 ? 1 : n * factorial(n - 1);
};

// Arrow functions
const arrowFunction = (a, b) => a + b;
const arrowWithBody = (a, b) => {
  const sum = a + b;
  return sum * 2;
};
const arrowSingleParam = (x) => x * 2;
const arrowNoParams = () => "Hello";
const arrowReturningObject = (x) => ({ value: x });

// Default parameters
function withDefaults(a, b = 10, c = a + b) {
  return a + b + c;
}

// Rest parameters
function withRest(first, ...others) {
  return [first, others];
}

// Spread in function call
const args = [1, 2, 3];
const result = regularFunction(...args);

// Higher-order functions
function higherOrder(fn) {
  return function (...args) {
    console.log("Calling with:", args);
    return fn(...args);
  };
}

// Immediately Invoked Function Expression (IIFE)
const iife = (function () {
  const private = "I'm private";
  return { public: "I'm public" };
})();

// Closure
function createCounter() {
  let count = 0;
  return {
    increment: () => ++count,
    decrement: () => --count,
    get: () => count,
  };
}

// Currying
const curry =
  (fn) =>
  (...args) =>
    args.length >= fn.length
      ? fn(...args)
      : curry(fn.bind(null, ...args));

// Memoization
function memoize(fn) {
  const cache = new Map();
  return function (...args) {
    const key = JSON.stringify(args);
    if (cache.has(key)) {
      return cache.get(key);
    }
    const result = fn.apply(this, args);
    cache.set(key, result);
    return result;
  };
}

// =============================================================================
// Classes
// =============================================================================

// Class declaration
class Person {
  // Private field
  #secret = "private";

  // Static field
  static count = 0;

  // Static private field
  static #privateStatic = "static private";

  // Constructor
  constructor(name, age) {
    this.name = name;
    this.age = age;
    Person.count++;
  }

  // Instance method
  greet() {
    return `Hello, I'm ${this.name}`;
  }

  // Getter
  get info() {
    return `${this.name}, ${this.age} years old`;
  }

  // Setter
  set info(value) {
    [this.name, this.age] = value.split(", ");
  }

  // Private method
  #privateMethod() {
    return this.#secret;
  }

  // Static method
  static create(name, age) {
    return new Person(name, age);
  }

  // Static private method
  static #privateStatic() {
    return Person.#privateStatic;
  }

  // Generator method
  *[Symbol.iterator]() {
    yield this.name;
    yield this.age;
  }
}

// Class inheritance
class Employee extends Person {
  constructor(name, age, department) {
    super(name, age);
    this.department = department;
  }

  greet() {
    return `${super.greet()}, I work in ${this.department}`;
  }

  // Static block (ES2022)
  static {
    console.log("Employee class initialized");
  }
}

// Class expression
const Animal = class {
  constructor(name) {
    this.name = name;
  }

  speak() {
    return `${this.name} makes a sound`;
  }
};

// Mixins
const Walkable = (Base) =>
  class extends Base {
    walk() {
      return "Walking...";
    }
  };

const Swimmable = (Base) =>
  class extends Base {
    swim() {
      return "Swimming...";
    }
  };

class Duck extends Walkable(Swimmable(Animal)) {
  quack() {
    return "Quack!";
  }
}

// =============================================================================
// Control Flow
// =============================================================================

// If/else
if (true) {
  console.log("truthy");
} else if (false) {
  console.log("never");
} else {
  console.log("also never");
}

// Ternary operator
const ternary = true ? "yes" : "no";

// Nullish coalescing
const nullish = null ?? "default";
const nullishAssign = null;
// nullishAssign ??= "assigned";

// Optional chaining
const optional = object?.nested?.deep?.value;
const optionalCall = object.method?.();
const optionalIndex = array?.[0];

// Switch
switch (name) {
  case "Alice":
    console.log("Hi Alice");
    break;
  case "Bob":
  case "Charlie":
    console.log("Hi Bob or Charlie");
    break;
  default:
    console.log("Who are you?");
}

// For loops
for (let i = 0; i < 10; i++) {
  if (i === 5) continue;
  if (i === 8) break;
  console.log(i);
}

// For...of
for (const item of array) {
  console.log(item);
}

// For...in
for (const key in object) {
  if (Object.hasOwn(object, key)) {
    console.log(key, object[key]);
  }
}

// While
let counter = 0;
while (counter < 5) {
  counter++;
}

// Do...while
do {
  counter--;
} while (counter > 0);

// Labels
outer: for (let i = 0; i < 3; i++) {
  inner: for (let j = 0; j < 3; j++) {
    if (i === 1 && j === 1) break outer;
    console.log(i, j);
  }
}

// =============================================================================
// Error Handling
// =============================================================================

// Try/catch/finally
try {
  throw new Error("Something went wrong");
} catch (error) {
  console.error(error.message);
} finally {
  console.log("Cleanup");
}

// Custom error
class CustomError extends Error {
  constructor(message, code) {
    super(message);
    this.name = "CustomError";
    this.code = code;
  }
}

// Error types
const errors = [
  new Error("Generic error"),
  new TypeError("Type error"),
  new ReferenceError("Reference error"),
  new SyntaxError("Syntax error"),
  new RangeError("Range error"),
  new URIError("URI error"),
  new EvalError("Eval error"),
  new AggregateError([new Error("1"), new Error("2")], "Multiple errors"),
];

// =============================================================================
// Async/Await and Promises
// =============================================================================

// Promise creation
const promise = new Promise((resolve, reject) => {
  setTimeout(() => resolve("Done"), 1000);
});

// Promise methods
promise
  .then((value) => console.log(value))
  .catch((error) => console.error(error))
  .finally(() => console.log("Finished"));

// Promise combinators
Promise.all([promise, promise]);
Promise.race([promise, promise]);
Promise.allSettled([promise, promise]);
Promise.any([promise, promise]);
Promise.resolve("value");
Promise.reject(new Error("error"));

// Async function
async function asyncFunction() {
  try {
    const result = await promise;
    return result;
  } catch (error) {
    throw error;
  }
}

// Async arrow function
const asyncArrow = async () => {
  const response = await fetch("/api/data");
  return response.json();
};

// Async IIFE
(async () => {
  const result = await asyncFunction();
  console.log(result);
})();

// Top-level await (in modules)
// const data = await fetch('/api/data').then(r => r.json());

// =============================================================================
// Generators and Iterators
// =============================================================================

// Generator function
function* generatorFunction() {
  yield 1;
  yield 2;
  yield 3;
  return 4;
}

// Generator with delegation
function* delegatingGenerator() {
  yield* [1, 2, 3];
  yield* generatorFunction();
}

// Async generator
async function* asyncGenerator() {
  yield await Promise.resolve(1);
  yield await Promise.resolve(2);
  yield await Promise.resolve(3);
}

// Custom iterator
const customIterable = {
  data: [1, 2, 3, 4, 5],
  [Symbol.iterator]() {
    let index = 0;
    const data = this.data;
    return {
      next() {
        if (index < data.length) {
          return { value: data[index++], done: false };
        }
        return { done: true };
      },
    };
  },
};

// Using iterators
for (const value of customIterable) {
  console.log(value);
}

// For await...of
async function consumeAsyncIterator() {
  for await (const value of asyncGenerator()) {
    console.log(value);
  }
}

// =============================================================================
// Proxies and Reflect
// =============================================================================

// Proxy
const proxy = new Proxy(object, {
  get(target, property, receiver) {
    console.log(`Getting ${String(property)}`);
    return Reflect.get(target, property, receiver);
  },
  set(target, property, value, receiver) {
    console.log(`Setting ${String(property)} to ${value}`);
    return Reflect.set(target, property, value, receiver);
  },
  has(target, property) {
    return Reflect.has(target, property);
  },
  deleteProperty(target, property) {
    return Reflect.deleteProperty(target, property);
  },
  apply(target, thisArg, args) {
    return Reflect.apply(target, thisArg, args);
  },
  construct(target, args, newTarget) {
    return Reflect.construct(target, args, newTarget);
  },
});

// Revocable proxy
const { proxy: revocable, revoke } = Proxy.revocable(object, {});

// =============================================================================
// Modules (ESM syntax)
// =============================================================================

// Import (would be at top of file)
// import defaultExport from './module.js';
// import { named, export as alias } from './module.js';
// import * as namespace from './module.js';
// import './side-effect.js';
// import defaultExport, { named } from './module.js';

// Dynamic import
async function loadModule() {
  const module = await import("./dynamic-module.js");
  return module.default;
}

// Export (would be in module)
// export default function() {}
// export const named = 'value';
// export { local as external };
// export * from './other-module.js';
// export { default } from './other-module.js';

// =============================================================================
// Decorators (Stage 3 Proposal)
// =============================================================================

// function logged(value, context) {
//   if (context.kind === 'method') {
//     return function (...args) {
//       console.log(`Calling ${context.name}`);
//       return value.apply(this, args);
//     };
//   }
// }

// class DecoratedClass {
//   @logged
//   method() {}
// }

// =============================================================================
// Utility Functions
// =============================================================================

// Object utilities
Object.keys(object);
Object.values(object);
Object.entries(object);
Object.fromEntries([["a", 1]]);
Object.assign({}, object);
Object.freeze(object);
Object.seal(object);
Object.isFrozen(object);
Object.isSealed(object);
Object.getOwnPropertyNames(object);
Object.getOwnPropertySymbols(object);
Object.getPrototypeOf(object);
Object.setPrototypeOf({}, null);
Object.hasOwn(object, "key");

// Array utilities
Array.isArray(array);
Array.from("abc");
Array.of(1, 2, 3);
array.at(-1);
array.includes(1);
array.indexOf(1);
array.lastIndexOf(1);
array.findIndex((x) => x > 3);
array.findLast((x) => x > 3);
array.findLastIndex((x) => x > 3);
array.toReversed();
array.toSorted();
array.toSpliced(1, 1);
array.with(0, 100);

// String utilities
"hello".at(-1);
"hello".includes("ell");
"hello".startsWith("he");
"hello".endsWith("lo");
"hello".padStart(10, ".");
"hello".padEnd(10, ".");
"  hello  ".trim();
"  hello  ".trimStart();
"  hello  ".trimEnd();
"hello".repeat(3);
"hello".replace("l", "L");
"hello".replaceAll("l", "L");
"a,b,c".split(",");
"hello world".match(/\w+/g);
"hello".toUpperCase();
"HELLO".toLowerCase();

// Number utilities
Number.isFinite(42);
Number.isInteger(42);
Number.isNaN(NaN);
Number.isSafeInteger(42);
Number.parseFloat("3.14");
Number.parseInt("42");
(3.14159).toFixed(2);
(3.14159).toPrecision(4);
(255).toString(16);

// Math utilities
Math.abs(-5);
Math.ceil(3.2);
Math.floor(3.8);
Math.round(3.5);
Math.trunc(3.9);
Math.max(1, 2, 3);
Math.min(1, 2, 3);
Math.pow(2, 10);
Math.sqrt(16);
Math.cbrt(27);
Math.random();
Math.sign(-5);
Math.log(Math.E);
Math.log10(100);
Math.sin(Math.PI / 2);
Math.cos(Math.PI);

// Date utilities
const now = new Date();
const specific = new Date("2024-01-15T12:00:00Z");
Date.now();
Date.parse("2024-01-15");
now.getFullYear();
now.getMonth();
now.getDate();
now.getHours();
now.getMinutes();
now.getSeconds();
now.getMilliseconds();
now.toISOString();
now.toLocaleDateString();
now.toLocaleTimeString();

// JSON utilities
JSON.stringify(object);
JSON.stringify(object, null, 2);
JSON.stringify(object, ["name", "age"]);
JSON.parse('{"key":"value"}');
JSON.parse('{"key":"value"}', (key, value) => value);

// =============================================================================
// Console API
// =============================================================================

console.log("Log message");
console.info("Info message");
console.warn("Warning message");
console.error("Error message");
console.debug("Debug message");
console.table([{ a: 1 }, { a: 2 }]);
console.dir(object);
console.count("label");
console.countReset("label");
console.time("timer");
console.timeLog("timer");
console.timeEnd("timer");
console.group("group");
console.groupCollapsed("collapsed");
console.groupEnd();
console.assert(true, "assertion");
console.trace("trace");
console.clear();

// =============================================================================
// Main Execution
// =============================================================================

function main() {
  console.log("=== JavaScript Sample ===");

  // Use examples
  const person = new Person("Alice", 30);
  console.log(person.greet());

  const counter = createCounter();
  console.log(counter.increment());
  console.log(counter.increment());
  console.log(counter.get());

  console.log("Factorial:", namedFunction(5));
  console.log("Mapped:", mapped);
  console.log("Filtered:", filtered);

  console.log("Done!");
}

main();
