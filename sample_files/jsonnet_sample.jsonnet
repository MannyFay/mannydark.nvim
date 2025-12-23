// ==============================================================================
// Comprehensive Jsonnet Sample - Syntax Highlighting Demonstration
// ==============================================================================

// This file demonstrates all major Jsonnet language features
// for syntax highlighting purposes.

// ==============================================================================
// Comments
// ==============================================================================

// Single line comment
# Hash comment (also valid)

/* Multi-line
   block comment */

/**
 * Javadoc-style
 * multi-line comment
 */

// ==============================================================================
// Local Variables and Basic Types
// ==============================================================================

// Local bindings
local x = 42;
local y = 3.14;
local name = "Jsonnet";

// Numbers
local integer = 42;
local negative = -17;
local float = 3.14159;
local scientific = 6.022e23;
local hex = 0xDEADBEEF;
local octal = 0755;

// Booleans
local trueVal = true;
local falseVal = false;

// Null
local nullVal = null;

// Strings
local simpleString = "Hello, World!";
local singleQuoted = 'Also valid';
local withEscapes = "Line 1\nLine 2\tTabbed";
local withQuotes = "She said \"Hello!\"";
local rawString = @"No escape \n processing";

// String formatting
local formatted = "Name: %s, Age: %d" % [name, 42];
local namedFormat = "%(name)s is %(age)d years old" % {name: "Alice", age: 30};

// Multi-line strings (text blocks)
local multiLine = |||
  This is a multi-line
  text block in Jsonnet.
  It preserves formatting.
|||;

// Multi-line with stripping
local stripped = |||
  Indentation will be
  preserved relative to
  the first line.
|||;

// ==============================================================================
// Objects
// ==============================================================================

// Simple object
local person = {
  name: "Alice",
  age: 30,
  email: "alice@example.com",
};

// Object with computed field names
local fieldName = "dynamic";
local dynamicObj = {
  [fieldName]: "value",
  ["computed_" + "key"]: 123,
};

// Nested objects
local config = {
  server: {
    host: "localhost",
    port: 8080,
  },
  database: {
    host: "localhost",
    port: 5432,
    name: "mydb",
  },
};

// Object comprehension
local squares = {
  [std.toString(i)]: i * i
  for i in std.range(1, 5)
};

// Object with methods
local calculator = {
  add(a, b):: a + b,
  subtract(a, b):: a - b,
  multiply(a, b):: a * b,
  divide(a, b):: if b != 0 then a / b else error "Division by zero",
};

// Self reference
local selfRef = {
  name: "Test",
  greeting: "Hello, " + self.name,
};

// Super reference (in inheritance)
local base = {
  value: 10,
  doubled: self.value * 2,
};

local derived = base + {
  value: 20,
  tripled: super.doubled + self.value,
};

// ==============================================================================
// Field Visibility
// ==============================================================================

local visibility = {
  // Visible in output (default)
  visibleField: "I appear in JSON",

  // Hidden from output (::)
  hiddenField:: "I don't appear in JSON",

  // Forced visible (:::)
  forcedVisible::: "I always appear",

  // Hidden method
  helper(x):: x * 2,

  // Visible field using hidden method
  result: self.helper(21),
};

// ==============================================================================
// Arrays
// ==============================================================================

// Simple arrays
local numbers = [1, 2, 3, 4, 5];
local strings = ["a", "b", "c"];
local mixed = [1, "two", true, null];

// Nested arrays
local nested = [[1, 2], [3, 4], [5, 6]];

// Array with trailing comma
local trailingComma = [
  "first",
  "second",
  "third",
];

// Array comprehension
local doubled = [x * 2 for x in numbers];
local filtered = [x for x in numbers if x > 2];
local combined = [x * y for x in [1, 2] for y in [10, 100]];

// Array indexing
local first = numbers[0];
local last = numbers[std.length(numbers) - 1];
local slice = numbers[1:3];
local sliceFrom = numbers[2:];
local sliceTo = numbers[:3];

// ==============================================================================
// Functions
// ==============================================================================

// Simple function
local double = function(x) x * 2;

// Shorthand syntax
local triple(x) = x * 3;

// Multiple arguments
local add(a, b) = a + b;

// Default arguments
local greet(name, greeting="Hello") = greeting + ", " + name + "!";

// Named arguments
local result1 = greet("World");
local result2 = greet(name="Alice", greeting="Hi");

// Function with local bindings
local compute(x) =
  local squared = x * x;
  local doubled = x * 2;
  squared + doubled;

// Higher-order function
local applyTwice(f, x) = f(f(x));
local quadrupled = applyTwice(double, 5);

// Anonymous function (lambda)
local nums = [1, 2, 3, 4, 5];
local mapped = std.map(function(x) x * 2, nums);

// Function returning function (currying)
local multiplier(factor) = function(x) x * factor;
local timesTen = multiplier(10);

// ==============================================================================
// Conditionals
// ==============================================================================

// If-then-else expression
local grade(score) =
  if score >= 90 then "A"
  else if score >= 80 then "B"
  else if score >= 70 then "C"
  else if score >= 60 then "D"
  else "F";

// Conditional in object
local conditionalObj = {
  [if true then "present"]: "value",
  optional: if false then "hidden",
};

// Null coalescing pattern
local maybeNull = null;
local withDefault = if maybeNull != null then maybeNull else "default";

// ==============================================================================
// Inheritance and Mixins
// ==============================================================================

// Object inheritance
local animal = {
  name: "Unknown",
  speak():: "...",
  greet: self.name + " says: " + self.speak(),
};

local dog = animal + {
  name: "Buddy",
  speak():: "Woof!",
  breed: "Labrador",
};

local cat = animal + {
  name: "Whiskers",
  speak():: "Meow!",
};

// Multiple inheritance / mixin
local mixin1 = {
  feature1: "enabled",
};

local mixin2 = {
  feature2: "enabled",
};

local combined = mixin1 + mixin2 + {
  name: "Combined",
};

// Deep merge with +:
local deepBase = {
  config: {
    server: {
      host: "localhost",
    },
  },
};

local deepDerived = deepBase + {
  config+: {
    server+: {
      port: 8080,
    },
  },
};

// ==============================================================================
// Imports
// ==============================================================================

// Import another Jsonnet file
// local otherConfig = import 'other.jsonnet';

// Import JSON file
// local data = import 'data.json';

// Import as string
// local template = importstr 'template.txt';

// Import binary as base64
// local binary = importbin 'file.bin';

// ==============================================================================
// Standard Library Functions
// ==============================================================================

local stdDemo = {
  // Type checking
  isArray: std.isArray([1, 2, 3]),
  isBoolean: std.isBoolean(true),
  isFunction: std.isFunction(function(x) x),
  isNumber: std.isNumber(42),
  isObject: std.isObject({}),
  isString: std.isString("hello"),

  // Type conversion
  toString: std.toString(42),
  parseInt: std.parseInt("42"),
  parseJson: std.parseJson('{"key": "value"}'),
  parseYaml: std.parseYaml("key: value"),

  // String functions
  length: std.length("hello"),
  substr: std.substr("hello", 1, 3),
  startsWith: std.startsWith("hello", "he"),
  endsWith: std.endsWith("hello", "lo"),
  split: std.split("a,b,c", ","),
  join: std.join("-", ["a", "b", "c"]),
  upper: std.asciiUpper("hello"),
  lower: std.asciiLower("HELLO"),
  trim: std.stripChars("  hello  ", " "),
  replace: std.strReplace("hello world", "world", "jsonnet"),
  format: std.format("Value: %d", [42]),
  escapeStringJson: std.escapeStringJson("hello\nworld"),
  escapeStringBash: std.escapeStringBash("hello world"),
  escapeStringDollars: std.escapeStringDollars("$HOME"),

  // Array functions
  arrayLength: std.length([1, 2, 3]),
  member: std.member([1, 2, 3], 2),
  count: std.count([1, 2, 1, 1, 3], 1),
  find: std.find(2, [1, 2, 3, 2]),
  map: std.map(function(x) x * 2, [1, 2, 3]),
  mapWithKey: std.mapWithKey(function(k, v) k + ": " + v, {a: "1", b: "2"}),
  mapWithIndex: std.mapWithIndex(function(i, x) {index: i, value: x}, ["a", "b"]),
  filter: std.filter(function(x) x > 2, [1, 2, 3, 4, 5]),
  filterMap: std.filterMap(
    function(x) x > 0,
    function(x) x * 2,
    [-1, 0, 1, 2]
  ),
  foldl: std.foldl(function(acc, x) acc + x, [1, 2, 3, 4], 0),
  foldr: std.foldr(function(x, acc) acc + x, [1, 2, 3, 4], 0),
  range: std.range(1, 5),
  repeat: std.repeat([1, 2], 3),
  slice: std.slice([1, 2, 3, 4, 5], 1, 4, 1),
  reverse: std.reverse([1, 2, 3]),
  sort: std.sort([3, 1, 2]),
  sortUniq: std.uniq(std.sort([1, 2, 1, 3, 2])),
  set: std.set([1, 2, 1, 3, 2]),
  setInter: std.setInter([1, 2, 3], [2, 3, 4]),
  setUnion: std.setUnion([1, 2], [2, 3]),
  setDiff: std.setDiff([1, 2, 3], [2]),
  setMember: std.setMember(2, [1, 2, 3]),
  flattenArrays: std.flattenArrays([[1, 2], [3, 4]]),
  prune: std.prune({a: 1, b: null, c: {d: null}}),

  // Object functions
  objectFields: std.objectFields({a: 1, b:: 2}),
  objectFieldsAll: std.objectFieldsAll({a: 1, b:: 2}),
  objectHas: std.objectHas({a: 1}, "a"),
  objectHasAll: std.objectHasAll({a: 1, b:: 2}, "b"),
  objectValues: std.objectValues({a: 1, b: 2}),
  objectKeysValues: std.objectKeysValues({a: 1, b: 2}),
  mapWithKey: std.mapWithKey(function(k, v) v * 2, {a: 1, b: 2}),
  get: std.get({a: 1}, "a", "default"),

  // Math functions
  abs: std.abs(-5),
  sign: std.sign(-5),
  max: std.max(1, 2),
  min: std.min(1, 2),
  pow: std.pow(2, 10),
  exp: std.exp(1),
  log: std.log(10),
  sqrt: std.sqrt(16),
  floor: std.floor(3.7),
  ceil: std.ceil(3.2),
  round: std.round(3.5),
  modulo: std.mod(17, 5),

  // Encoding/decoding
  base64: std.base64("hello"),
  base64Decode: std.base64Decode("aGVsbG8="),
  base64DecodeBytes: std.base64DecodeBytes("aGVsbG8="),
  md5: std.md5("hello"),
  sha1: std.sha1("hello"),
  sha256: std.sha256("hello"),
  sha512: std.sha512("hello"),
  sha3: std.sha3("hello"),

  // Manifest functions
  manifestJsonEx: std.manifestJsonEx({a: 1}, "  "),
  manifestYamlDoc: std.manifestYamlDoc({a: 1}),
  manifestYamlStream: std.manifestYamlStream([{a: 1}, {b: 2}]),
  manifestIni: std.manifestIni({sections: {main: {key: "value"}}}),
  manifestPython: std.manifestPython({a: 1, b: "hello"}),
  manifestPythonVars: std.manifestPythonVars({a: 1, b: "hello"}),
  manifestXmlJsonml: std.manifestXmlJsonml(["tag", {attr: "value"}, "content"]),
  manifestTomlEx: std.manifestTomlEx({section: {key: "value"}}, "  "),

  // Other functions
  thisFile: std.thisFile,
  extVar: std.extVar("foo"),  // External variable
  native: std.native("myFunc"),  // Native function
  trace: std.trace("Debug message", 42),
  assertEqual: std.assertEqual(1 + 1, 2),
};

// ==============================================================================
// Error Handling
// ==============================================================================

// Error function
local validatePositive(x) =
  if x > 0 then x
  else error "Value must be positive: " + x;

// Assert
local assertExample =
  assert std.isNumber(42) : "Expected a number";
  "passed";

// Assert in object
local withAssert = {
  assert self.value > 0 : "Value must be positive",
  value: 10,
};

// ==============================================================================
// Complex Examples
// ==============================================================================

// Kubernetes Deployment template
local kubeDeployment(name, image, port, replicas=1) = {
  apiVersion: "apps/v1",
  kind: "Deployment",
  metadata: {
    name: name,
    labels: {
      app: name,
    },
  },
  spec: {
    replicas: replicas,
    selector: {
      matchLabels: {
        app: name,
      },
    },
    template: {
      metadata: {
        labels: {
          app: name,
        },
      },
      spec: {
        containers: [
          {
            name: name,
            image: image,
            ports: [
              {
                containerPort: port,
              },
            ],
          },
        ],
      },
    },
  },
};

// Create multiple deployments
local deployments = {
  ["deployment-" + name]: kubeDeployment(name, "image:" + name, 8080)
  for name in ["app1", "app2", "app3"]
};

// CI/CD Pipeline configuration
local ciPipeline = {
  local job(name, script, image="ubuntu:latest") = {
    image: image,
    script: script,
  },

  stages: ["build", "test", "deploy"],

  build: job("build", [
    "echo Building...",
    "make build",
  ]),

  test: job("test", [
    "echo Testing...",
    "make test",
  ]),

  deploy: job("deploy", [
    "echo Deploying...",
    "make deploy",
  ]) + {
    only: ["main"],
    environment: "production",
  },
};

// Configuration with defaults
local Defaults = {
  server: {
    host: "0.0.0.0",
    port: 8080,
    timeout: 30,
  },
  database: {
    host: "localhost",
    port: 5432,
    pool_size: 10,
  },
  logging: {
    level: "info",
    format: "json",
  },
};

local Production = Defaults + {
  server+: {
    host: "0.0.0.0",
  },
  database+: {
    host: "db.production.internal",
    pool_size: 50,
  },
  logging+: {
    level: "warn",
  },
};

local Development = Defaults + {
  database+: {
    host: "localhost",
  },
  logging+: {
    level: "debug",
    format: "text",
  },
};

// ==============================================================================
// Output Formats
// ==============================================================================

// Multi-document output (for YAML stream)
local multiDoc = [
  {name: "doc1", value: 1},
  {name: "doc2", value: 2},
  {name: "doc3", value: 3},
];

// INI format
local iniConfig = {
  sections: {
    server: {
      host: "localhost",
      port: "8080",
    },
    database: {
      host: "localhost",
      port: "5432",
    },
  },
};

// ==============================================================================
// Final Output
// ==============================================================================

// The final expression is the output
{
  // Include various examples
  person: person,
  config: config,
  squares: squares,
  visibility: visibility,
  numbers: numbers,
  doubled: doubled,
  dog: dog,
  cat: cat,
  deployments: deployments,
  ciPipeline: ciPipeline,
  environments: {
    production: Production,
    development: Development,
  },
}
