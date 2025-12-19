/**
 * Comprehensive TypeScript Sample - Syntax Highlighting Demonstration
 *
 * This file demonstrates all major TypeScript language features
 * for syntax highlighting purposes.
 *
 * @fileoverview Complete TypeScript syntax showcase
 * @author Sample Author
 * @version 1.0.0
 */

// =============================================================================
// Basic Types
// =============================================================================

// Primitive types
const booleanType: boolean = true;
const numberType: number = 42;
const stringType: string = "Hello, TypeScript!";
const bigintType: bigint = 9007199254740991n;
const symbolType: symbol = Symbol("description");

// Special types
const nullType: null = null;
const undefinedType: undefined = undefined;
const voidType: void = undefined;
const neverType: never = (() => { throw new Error(); })();
const anyType: any = "anything";
const unknownType: unknown = "unknown value";

// Object type
const objectType: object = { key: "value" };

// =============================================================================
// Arrays and Tuples
// =============================================================================

// Array types
const arrayType1: number[] = [1, 2, 3];
const arrayType2: Array<number> = [1, 2, 3];
const readonlyArray: readonly number[] = [1, 2, 3];
const readonlyArray2: ReadonlyArray<number> = [1, 2, 3];

// Tuple types
const tuple: [string, number] = ["hello", 42];
const namedTuple: [name: string, age: number] = ["Alice", 30];
const optionalTuple: [string, number?] = ["hello"];
const restTuple: [string, ...number[]] = ["hello", 1, 2, 3];
const readonlyTuple: readonly [string, number] = ["hello", 42];

// =============================================================================
// Type Aliases and Interfaces
// =============================================================================

// Type alias
type UserId = number;
type UserName = string;
type Status = "active" | "inactive" | "pending";

// Complex type alias
type Point = {
  x: number;
  y: number;
};

type Nullable<T> = T | null;
type Optional<T> = T | undefined;

// Interface
interface Person {
  readonly id: number;
  name: string;
  age?: number;
  email: string;
  greet(): string;
}

// Interface extension
interface Employee extends Person {
  department: string;
  salary: number;
}

// Interface with index signature
interface StringDictionary {
  [key: string]: string;
}

// Interface with call signature
interface Calculator {
  (a: number, b: number): number;
  operator: string;
}

// Interface with construct signature
interface PersonConstructor {
  new (name: string, age: number): Person;
}

// Declaration merging
interface Config {
  host: string;
}

interface Config {
  port: number;
}

// =============================================================================
// Union and Intersection Types
// =============================================================================

// Union types
type StringOrNumber = string | number;
type Result<T> = T | Error;
type Primitive = string | number | boolean | null | undefined;

// Intersection types
type Named = { name: string };
type Aged = { age: number };
type NamedAndAged = Named & Aged;

// Discriminated unions
type Shape =
  | { kind: "circle"; radius: number }
  | { kind: "rectangle"; width: number; height: number }
  | { kind: "triangle"; base: number; height: number };

function getArea(shape: Shape): number {
  switch (shape.kind) {
    case "circle":
      return Math.PI * shape.radius ** 2;
    case "rectangle":
      return shape.width * shape.height;
    case "triangle":
      return (shape.base * shape.height) / 2;
  }
}

// =============================================================================
// Literal Types
// =============================================================================

// String literal types
type Direction = "north" | "south" | "east" | "west";
type HttpMethod = "GET" | "POST" | "PUT" | "DELETE" | "PATCH";

// Numeric literal types
type DiceRoll = 1 | 2 | 3 | 4 | 5 | 6;
type BitValue = 0 | 1;

// Boolean literal types
type True = true;
type False = false;

// Template literal types
type Greeting = `Hello, ${string}!`;
type CssValue = `${number}px` | `${number}em` | `${number}%`;
type EventName = `on${Capitalize<string>}`;

// =============================================================================
// Type Assertions and Guards
// =============================================================================

// Type assertions
const someValue: unknown = "Hello";
const strLength1 = (someValue as string).length;
const strLength2 = (<string>someValue).length;

// Const assertion
const immutableArray = [1, 2, 3] as const;
const immutableObject = { name: "Alice", age: 30 } as const;

// Non-null assertion
function getValue(): string | null {
  return "value";
}
const definitelyString = getValue()!;

// Type guards
function isString(value: unknown): value is string {
  return typeof value === "string";
}

function isError(value: unknown): value is Error {
  return value instanceof Error;
}

// Asserts keyword
function assertIsString(value: unknown): asserts value is string {
  if (typeof value !== "string") {
    throw new Error("Value is not a string");
  }
}

// Satisfies operator
const palette = {
  red: [255, 0, 0],
  green: "#00FF00",
  blue: [0, 0, 255],
} satisfies Record<string, string | [number, number, number]>;

// =============================================================================
// Generics
// =============================================================================

// Generic function
function identity<T>(value: T): T {
  return value;
}

// Generic arrow function
const genericArrow = <T>(value: T): T => value;

// Generic with constraint
function getLength<T extends { length: number }>(value: T): number {
  return value.length;
}

// Multiple type parameters
function pair<T, U>(first: T, second: U): [T, U] {
  return [first, second];
}

// Generic interface
interface Container<T> {
  value: T;
  getValue(): T;
  setValue(value: T): void;
}

// Generic class
class Box<T> {
  private content: T;

  constructor(content: T) {
    this.content = content;
  }

  get(): T {
    return this.content;
  }

  set(content: T): void {
    this.content = content;
  }
}

// Generic type alias
type Wrapper<T> = {
  value: T;
  timestamp: number;
};

// Default type parameter
type ResponseData<T = unknown> = {
  data: T;
  status: number;
};

// Generic constraints with keyof
function getProperty<T, K extends keyof T>(obj: T, key: K): T[K] {
  return obj[key];
}

// =============================================================================
// Utility Types
// =============================================================================

interface User {
  id: number;
  name: string;
  email: string;
  age?: number;
}

// Partial - makes all properties optional
type PartialUser = Partial<User>;

// Required - makes all properties required
type RequiredUser = Required<User>;

// Readonly - makes all properties readonly
type ReadonlyUser = Readonly<User>;

// Pick - selects subset of properties
type UserName2 = Pick<User, "id" | "name">;

// Omit - removes properties
type UserWithoutEmail = Omit<User, "email">;

// Record - creates object type
type StatusMap = Record<Status, User[]>;

// Exclude - excludes types from union
type NonNullable2 = Exclude<string | number | null | undefined, null | undefined>;

// Extract - extracts types from union
type OnlyStrings = Extract<string | number | boolean, string>;

// NonNullable - removes null and undefined
type DefiniteValue = NonNullable<string | null | undefined>;

// ReturnType - gets function return type
type GetUserReturn = ReturnType<typeof identity<User>>;

// Parameters - gets function parameter types
type IdentityParams = Parameters<typeof identity<string>>;

// ConstructorParameters - gets constructor parameter types
type BoxParams = ConstructorParameters<typeof Box>;

// InstanceType - gets instance type of constructor
type BoxInstance = InstanceType<typeof Box<string>>;

// Awaited - unwraps Promise type
type UnwrappedPromise = Awaited<Promise<string>>;

// =============================================================================
// Conditional Types
// =============================================================================

// Basic conditional type
type IsString<T> = T extends string ? true : false;

// Distributive conditional type
type ToArray<T> = T extends unknown ? T[] : never;

// Infer keyword
type ElementType<T> = T extends (infer E)[] ? E : T;
type ReturnTypeOf<T> = T extends (...args: unknown[]) => infer R ? R : never;
type PromiseValue<T> = T extends Promise<infer V> ? V : T;

// Recursive conditional type
type DeepReadonly<T> = T extends object
  ? { readonly [K in keyof T]: DeepReadonly<T[K]> }
  : T;

type DeepPartial<T> = T extends object
  ? { [K in keyof T]?: DeepPartial<T[K]> }
  : T;

// =============================================================================
// Mapped Types
// =============================================================================

// Basic mapped type
type OptionsFlags<T> = {
  [K in keyof T]: boolean;
};

// With modifiers
type Mutable<T> = {
  -readonly [K in keyof T]: T[K];
};

type Concrete<T> = {
  [K in keyof T]-?: T[K];
};

// Key remapping
type Getters<T> = {
  [K in keyof T as `get${Capitalize<string & K>}`]: () => T[K];
};

type Setters<T> = {
  [K in keyof T as `set${Capitalize<string & K>}`]: (value: T[K]) => void;
};

// Filtering keys
type OnlyStringsKeys<T> = {
  [K in keyof T as T[K] extends string ? K : never]: T[K];
};

// =============================================================================
// Classes
// =============================================================================

// Abstract class
abstract class Animal {
  abstract name: string;
  abstract speak(): string;

  move(): string {
    return "Moving...";
  }
}

// Class implementation
class Dog extends Animal {
  name: string;

  constructor(name: string) {
    super();
    this.name = name;
  }

  speak(): string {
    return "Woof!";
  }
}

// Class with all modifiers
class CompleteClass {
  // Public field
  public publicField: string = "public";

  // Private field
  private privateField: string = "private";

  // Protected field
  protected protectedField: string = "protected";

  // Readonly field
  readonly readonlyField: string = "readonly";

  // Static field
  static staticField: string = "static";

  // Private with # syntax
  #reallyPrivate: string = "really private";

  // Parameter properties
  constructor(
    public autoPublic: string,
    private autoPrivate: string,
    protected autoProtected: string,
    readonly autoReadonly: string
  ) {}

  // Getter
  get value(): string {
    return this.privateField;
  }

  // Setter
  set value(val: string) {
    this.privateField = val;
  }

  // Static block
  static {
    console.log("Class initialized");
  }
}

// Class implementing interface
class PersonImpl implements Person {
  constructor(
    public readonly id: number,
    public name: string,
    public email: string,
    public age?: number
  ) {}

  greet(): string {
    return `Hello, I'm ${this.name}`;
  }
}

// Generic class
class Stack<T> {
  private items: T[] = [];

  push(item: T): void {
    this.items.push(item);
  }

  pop(): T | undefined {
    return this.items.pop();
  }

  peek(): T | undefined {
    return this.items[this.items.length - 1];
  }
}

// =============================================================================
// Decorators
// =============================================================================

// Class decorator
function sealed(constructor: Function) {
  Object.seal(constructor);
  Object.seal(constructor.prototype);
}

// Method decorator
function log(
  target: any,
  propertyKey: string,
  descriptor: PropertyDescriptor
): PropertyDescriptor {
  const original = descriptor.value;
  descriptor.value = function (...args: any[]) {
    console.log(`Calling ${propertyKey} with`, args);
    return original.apply(this, args);
  };
  return descriptor;
}

// Property decorator
function validate(target: any, propertyKey: string) {
  let value: any;
  Object.defineProperty(target, propertyKey, {
    get: () => value,
    set: (newValue) => {
      if (typeof newValue !== "string") {
        throw new Error("Must be a string");
      }
      value = newValue;
    },
  });
}

// Parameter decorator
function required(
  target: any,
  propertyKey: string,
  parameterIndex: number
) {
  console.log(`Parameter ${parameterIndex} of ${propertyKey} is required`);
}

// Decorated class
@sealed
class DecoratedClass {
  @validate
  name: string = "";

  @log
  greet(@required message: string): string {
    return `${this.name}: ${message}`;
  }
}

// =============================================================================
// Enums
// =============================================================================

// Numeric enum
enum Direction {
  Up,
  Down,
  Left,
  Right,
}

// Enum with initializers
enum HttpStatus {
  OK = 200,
  Created = 201,
  BadRequest = 400,
  NotFound = 404,
  InternalServerError = 500,
}

// String enum
enum LogLevel {
  Debug = "DEBUG",
  Info = "INFO",
  Warn = "WARN",
  Error = "ERROR",
}

// Heterogeneous enum
enum Mixed {
  No = 0,
  Yes = "YES",
}

// Const enum (inlined at compile time)
const enum Inline {
  A = 1,
  B = A * 2,
  C = B * 2,
}

// Ambient enum
declare enum AmbientEnum {
  A = 1,
  B,
  C,
}

// =============================================================================
// Namespaces and Modules
// =============================================================================

// Namespace
namespace Geometry {
  export interface Point {
    x: number;
    y: number;
  }

  export function distance(p1: Point, p2: Point): number {
    return Math.sqrt((p2.x - p1.x) ** 2 + (p2.y - p1.y) ** 2);
  }

  export namespace ThreeD {
    export interface Point3D extends Point {
      z: number;
    }
  }
}

// Namespace alias
import G = Geometry;
import P3D = Geometry.ThreeD.Point3D;

// Module augmentation
declare module "./sample" {
  interface Person {
    additionalMethod(): void;
  }
}

// Global augmentation
declare global {
  interface Array<T> {
    customMethod(): void;
  }
}

// =============================================================================
// Declaration Files Syntax
// =============================================================================

// Ambient declarations
declare const VERSION: string;
declare function doSomething(value: string): void;
declare class ExternalClass {
  constructor(name: string);
  method(): void;
}

// Ambient module
declare module "external-module" {
  export function external(): void;
  export default class ExternalDefault {}
}

// Triple-slash directives
/// <reference types="node" />
/// <reference path="./types.d.ts" />
/// <reference lib="es2020" />

// =============================================================================
// Advanced Function Types
// =============================================================================

// Function overloads
function processValue(value: string): string;
function processValue(value: number): number;
function processValue(value: string | number): string | number {
  return value;
}

// Constructor type
type PersonCtor = new (name: string, age: number) => Person;

// This parameter
function handleClick(this: HTMLElement, event: Event): void {
  console.log(this.tagName);
}

// Variadic tuple types
type Concat<T extends unknown[], U extends unknown[]> = [...T, ...U];

// Labeled tuple elements
type Range = [start: number, end: number];

// =============================================================================
// Type Assertions and Narrowing
// =============================================================================

function example(value: string | number | null) {
  // typeof narrowing
  if (typeof value === "string") {
    console.log(value.toUpperCase());
  }

  // Truthiness narrowing
  if (value) {
    console.log(value);
  }

  // Equality narrowing
  if (value === null) {
    console.log("null");
  }

  // in narrowing
  const obj: { name: string } | { age: number } = { name: "test" };
  if ("name" in obj) {
    console.log(obj.name);
  }

  // instanceof narrowing
  if (value instanceof String) {
    console.log(value.charAt(0));
  }
}

// Control flow analysis
function controlFlow(value: string | null): string {
  if (value === null) {
    return "default";
  }
  // value is narrowed to string here
  return value.toUpperCase();
}

// =============================================================================
// Export and Import
// =============================================================================

// Named exports
export { Person, Employee };
export type { UserId, Status };

// Export declaration
export interface ExportedInterface {
  value: string;
}

export class ExportedClass {}

export const exportedConst = "value";

export function exportedFunction(): void {}

// Default export
export default class MainClass {
  main(): void {
    console.log("Main");
  }
}

// Re-exports
// export { something } from './other-module';
// export * from './other-module';
// export * as namespace from './other-module';

// =============================================================================
// Main Execution
// =============================================================================

function main(): void {
  console.log("=== TypeScript Sample ===");

  // Generic usage
  const boxNumber = new Box<number>(42);
  const boxString = new Box<string>("Hello");
  console.log(boxNumber.get(), boxString.get());

  // Interface implementation
  const person = new PersonImpl(1, "Alice", "alice@example.com", 30);
  console.log(person.greet());

  // Discriminated union
  const circle: Shape = { kind: "circle", radius: 5 };
  console.log("Circle area:", getArea(circle));

  // Stack usage
  const stack = new Stack<number>();
  stack.push(1);
  stack.push(2);
  stack.push(3);
  console.log("Stack peek:", stack.peek());

  console.log("Done!");
}

main();
