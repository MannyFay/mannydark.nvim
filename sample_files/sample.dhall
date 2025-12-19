{- ==============================================================================
   Comprehensive Dhall Sample - Syntax Highlighting Demonstration
   ==============================================================================

   This file demonstrates all major Dhall language features
   for syntax highlighting purposes.
-}

-- ==============================================================================
-- Comments
-- ==============================================================================

-- Single line comment

{- Multi-line
   block comment -}

{- Nested {- block -} comments are supported -}

-- ==============================================================================
-- Basic Types and Values
-- ==============================================================================

-- Natural numbers (non-negative integers)
let naturalExample : Natural = 42

let naturalZero : Natural = 0

-- Natural arithmetic
let naturalSum = 10 + 5
let naturalProduct = 10 * 5

-- Integers (positive, negative, or zero)
let integerPositive : Integer = +42
let integerNegative : Integer = -17
let integerZero : Integer = +0

-- Double (floating-point)
let doubleExample : Double = 3.14159
let doubleScientific : Double = 6.022e23
let doubleNegative : Double = -2.71828
let doubleInfinity : Double = Infinity
let doubleNegInfinity : Double = -Infinity
let doubleNaN : Double = NaN

-- Text (strings)
let textSimple : Text = "Hello, Dhall!"
let textWithEscapes : Text = "Line 1\nLine 2\tTabbed"
let textWithQuotes : Text = "She said \"Hello!\""
let textWithBackslash : Text = "C:\\Users\\Name"
let textWithDollar : Text = "\${escaped}"  -- Escaped interpolation

-- Text interpolation
let name = "World"
let greeting : Text = "Hello, ${name}!"

-- Multi-line text
let multiLineText : Text =
  ''
  This is a multi-line
  text literal in Dhall.
  Indentation is preserved.
  ''

-- Multi-line with interpolation
let multiLineInterpolation : Text =
  ''
  Hello, ${name}!
  Welcome to Dhall.
  ''

-- Bool
let boolTrue : Bool = True
let boolFalse : Bool = False

-- Optional values
let someValue : Optional Natural = Some 42
let noneValue : Optional Natural = None Natural

-- ==============================================================================
-- Records (Objects)
-- ==============================================================================

-- Record type
let PersonType = { name : Text, age : Natural, email : Text }

-- Record value
let person : PersonType =
  { name = "Alice"
  , age = 30
  , email = "alice@example.com"
  }

-- Nested records
let Config =
  { server :
      { host : Text
      , port : Natural
      }
  , database :
      { host : Text
      , port : Natural
      , name : Text
      }
  }

let config : Config =
  { server =
      { host = "localhost"
      , port = 8080
      }
  , database =
      { host = "localhost"
      , port = 5432
      , name = "mydb"
      }
  }

-- Record field access
let serverHost : Text = config.server.host
let serverPort : Natural = config.server.port

-- Record update (non-destructive)
let updatedPerson : PersonType = person // { age = 31 }

-- Record type with optional fields
let PersonWithOptional =
  { name : Text
  , age : Natural
  , nickname : Optional Text
  }

let personWithNickname : PersonWithOptional =
  { name = "Bob"
  , age = 25
  , nickname = Some "Bobby"
  }

-- Empty record
let emptyRecord : {} = {=}

-- Record completion
let defaultPerson : PersonType =
  { name = "Unknown"
  , age = 0
  , email = "unknown@example.com"
  }

-- Record type operator
let CombinedType = { a : Natural } //\\ { b : Text }
let combinedValue : CombinedType = { a = 1, b = "hello" }

-- Recursive record type operator
let RecursiveCombined = { a : Natural } /\ { b : Text }

-- ==============================================================================
-- Unions (Sum Types / Enums)
-- ==============================================================================

-- Simple union (enum-like)
let Status = < Active | Inactive | Pending >

let activeStatus : Status = Status.Active
let pendingStatus : Status = Status.Pending

-- Union with associated data
let Result = < Ok : Text | Error : Text >

let successResult : Result = Result.Ok "Success!"
let errorResult : Result = Result.Error "Something went wrong"

-- Pattern matching with merge
let statusToText : Status -> Text =
  \(s : Status) ->
    merge
      { Active = "The status is active"
      , Inactive = "The status is inactive"
      , Pending = "The status is pending"
      }
      s

let resultToText : Result -> Text =
  \(r : Result) ->
    merge
      { Ok = \(msg : Text) -> "Success: ${msg}"
      , Error = \(err : Text) -> "Error: ${err}"
      }
      r

-- Union type combination
let ExtendedStatus = Status | < Archived >

-- ==============================================================================
-- Lists
-- ==============================================================================

-- List of naturals
let numbers : List Natural = [1, 2, 3, 4, 5]

-- Empty list
let emptyList : List Natural = [] : List Natural

-- List of records
let people : List PersonType =
  [ { name = "Alice", age = 30, email = "alice@example.com" }
  , { name = "Bob", age = 25, email = "bob@example.com" }
  , { name = "Charlie", age = 35, email = "charlie@example.com" }
  ]

-- List concatenation
let list1 = [1, 2, 3]
let list2 = [4, 5, 6]
let concatenated = list1 # list2

-- List of lists
let nestedList : List (List Natural) = [[1, 2], [3, 4], [5, 6]]

-- ==============================================================================
-- Functions
-- ==============================================================================

-- Simple function
let double : Natural -> Natural =
  \(n : Natural) -> n * 2

-- Multiple arguments (curried)
let add : Natural -> Natural -> Natural =
  \(a : Natural) -> \(b : Natural) -> a + b

-- Function application
let doubled = double 21
let sum = add 10 20

-- Partial application
let addFive : Natural -> Natural = add 5
let result = addFive 10

-- Lambda with pattern matching on records
let getName : PersonType -> Text =
  \(p : PersonType) -> p.name

-- Higher-order function
let applyTwice : (Natural -> Natural) -> Natural -> Natural =
  \(f : Natural -> Natural) -> \(x : Natural) -> f (f x)

let quadrupled = applyTwice double 5

-- Polymorphic function
let identity : forall (a : Type) -> a -> a =
  \(a : Type) -> \(x : a) -> x

let identityNatural = identity Natural 42
let identityText = identity Text "hello"

-- Function composition
let compose
  : forall (a : Type) -> forall (b : Type) -> forall (c : Type)
  -> (b -> c) -> (a -> b) -> a -> c
  = \(a : Type) -> \(b : Type) -> \(c : Type)
  -> \(f : b -> c) -> \(g : a -> b) -> \(x : a)
  -> f (g x)

-- ==============================================================================
-- Let Bindings
-- ==============================================================================

-- Simple let binding
let x = 42

-- Let with type annotation
let y : Natural = 100

-- Multiple let bindings
let result1 =
  let a = 10
  let b = 20
  let c = a + b
  in c * 2

-- Nested let bindings
let result2 =
  let outer = 100
  in let inner = outer * 2
     in inner + outer

-- Let binding with function
let calculate =
  let square = \(n : Natural) -> n * n
  let double = \(n : Natural) -> n * 2
  in double (square 5)

-- ==============================================================================
-- If-Then-Else
-- ==============================================================================

let condition = True

let ifResult : Text =
  if condition
  then "Yes"
  else "No"

-- Nested if-then-else
let categorize : Natural -> Text =
  \(n : Natural) ->
    if Natural/isZero n
    then "Zero"
    else if Natural/lessThan n 10
    then "Small"
    else if Natural/lessThan n 100
    then "Medium"
    else "Large"

-- ==============================================================================
-- Built-in Functions
-- ==============================================================================

-- Natural functions
let natIsZero = Natural/isZero 0
let natFold = Natural/fold 5 Text (\(t : Text) -> t ++ "!") "Hi"
let natBuild = Natural/build (\(natural : Type) -> \(succ : natural -> natural) -> \(zero : natural) -> succ (succ (succ zero)))
let natSubtract = Natural/subtract 3 10
let natShow = Natural/show 42
let natToInteger = Natural/toInteger 42

-- Integer functions
let intClamp = Integer/clamp -5
let intNegate = Integer/negate +42
let intShow = Integer/show -17
let intToDouble = Integer/toDouble +42
let intAbs = Integer/abs -17

-- Double functions
let doubleShow = Double/show 3.14

-- Text functions
let textShow = Text/show "hello"
let textReplace = Text/replace { search = "world", replace = "Dhall" } "Hello, world!"

-- List functions
let listBuild =
  List/build
    Natural
    ( \(list : Type)
    -> \(cons : Natural -> list -> list)
    -> \(nil : list)
    -> cons 1 (cons 2 (cons 3 nil))
    )

let listFold =
  List/fold
    Natural
    [1, 2, 3, 4, 5]
    Natural
    (\(x : Natural) -> \(acc : Natural) -> x + acc)
    0

let listLength = List/length Natural [1, 2, 3]
let listHead = List/head Natural [1, 2, 3]
let listLast = List/last Natural [1, 2, 3]
let listIndexed = List/indexed Natural [10, 20, 30]
let listReverse = List/reverse Natural [1, 2, 3]

-- Optional functions
let optionalFold =
  Optional/fold
    Natural
    (Some 42)
    Text
    (\(n : Natural) -> "Got: ${Natural/show n}")
    "Nothing"

let optionalBuild =
  Optional/build
    Natural
    ( \(optional : Type)
    -> \(some : Natural -> optional)
    -> \(none : optional)
    -> some 42
    )

-- Bool functions
let boolBuild =
  Bool/build
    (\(bool : Type) -> \(true : bool) -> \(false : bool) -> true)

let boolFold =
  Bool/fold
    True
    Text
    "It's true!"
    "It's false!"

let boolShow = Bool/show True
let boolAnd = True && False
let boolOr = True || False
let boolNot = True == False

-- ==============================================================================
-- Imports
-- ==============================================================================

-- Import from file (commented for demonstration)
-- let importedConfig = ./config.dhall

-- Import from URL
-- let remoteImport = https://example.com/config.dhall

-- Import with hash (integrity check)
-- let hashedImport = ./config.dhall sha256:abc123...

-- Import alternative (fallback)
-- let withFallback = ./config.dhall ? ./default.dhall

-- Import as Text
-- let asText = ./config.dhall as Text

-- Import as Location
-- let asLocation = ./config.dhall as Location

-- Import as Bytes
-- let asBytes = ./config.dhall as Bytes

-- Environment variable
-- let envVar = env:HOME as Text

-- Environment variable with default
-- let envVarDefault = env:UNDEFINED ? "default"

-- ==============================================================================
-- Type Annotations and Assertions
-- ==============================================================================

-- Type annotation
let annotated : Natural = 42

-- Type assertion (assert)
let assertion = assert : 1 + 1 === 2

-- Type with kind annotation
let ListNatural : Type = List Natural

-- Kind annotation
let typeConstructor : Type -> Type = List

-- ==============================================================================
-- Prelude Usage
-- ==============================================================================

-- The Prelude provides many useful functions
-- let Prelude = https://prelude.dhall-lang.org/v21.1.0/package.dhall

-- Using Prelude functions (assuming import)
-- let mapped = Prelude.List.map Natural Text Natural/show [1, 2, 3]
-- let filtered = Prelude.List.filter Natural (\(n : Natural) -> Natural/lessThan 2 n) [1, 2, 3, 4, 5]
-- let concatenated = Prelude.List.concat Natural [[1, 2], [3, 4]]
-- let jsonOutput = Prelude.JSON.render (Prelude.JSON.object [{ mapKey = "key", mapValue = Prelude.JSON.string "value" }])

-- ==============================================================================
-- Complex Examples
-- ==============================================================================

-- Kubernetes Deployment type (simplified)
let Deployment =
  { apiVersion : Text
  , kind : Text
  , metadata :
      { name : Text
      , labels : List { mapKey : Text, mapValue : Text }
      }
  , spec :
      { replicas : Natural
      , selector :
          { matchLabels : List { mapKey : Text, mapValue : Text }
          }
      , template :
          { metadata :
              { labels : List { mapKey : Text, mapValue : Text }
              }
          , spec :
              { containers :
                  List
                    { name : Text
                    , image : Text
                    , ports : List { containerPort : Natural }
                    }
              }
          }
      }
  }

-- Create a deployment
let makeDeployment
  : { name : Text, image : Text, port : Natural, replicas : Natural }
  -> Deployment
  = \(config : { name : Text, image : Text, port : Natural, replicas : Natural })
  -> let labels = [{ mapKey = "app", mapValue = config.name }]
     in { apiVersion = "apps/v1"
        , kind = "Deployment"
        , metadata =
            { name = config.name
            , labels = labels
            }
        , spec =
            { replicas = config.replicas
            , selector =
                { matchLabels = labels
                }
            , template =
                { metadata =
                    { labels = labels
                    }
                , spec =
                    { containers =
                        [ { name = config.name
                          , image = config.image
                          , ports = [{ containerPort = config.port }]
                          }
                        ]
                    }
                }
            }
        }

let myDeployment =
  makeDeployment
    { name = "my-app"
    , image = "my-app:latest"
    , port = 8080
    , replicas = 3
    }

-- ==============================================================================
-- JSON/YAML Output
-- ==============================================================================

-- For JSON output, use dhall-to-json
-- For YAML output, use dhall-to-yaml

-- Example structure that converts well to JSON
let jsonExample =
  { string = "value"
  , number = 42
  , boolean = True
  , null = None Natural
  , array = [1, 2, 3]
  , object =
      { nested = "value"
      , another = 123
      }
  }

-- ==============================================================================
-- Type-Level Programming
-- ==============================================================================

-- Type-level list
let TypeList = Type -> Type -> Type

-- Type alias
let StringList = List Text

-- Parameterized type alias
let Pair = \(a : Type) -> \(b : Type) -> { first : a, second : b }
let IntStringPair = Pair Natural Text
let pairExample : IntStringPair = { first = 42, second = "hello" }

-- ==============================================================================
-- Record Schemas
-- ==============================================================================

-- Define a schema (type)
let PersonSchema =
  { Type =
      { name : Text
      , age : Natural
      , active : Bool
      }
  , default =
      { name = "Unknown"
      , age = 0
      , active = True
      }
  }

-- Use schema with completion operator
let personFromSchema : PersonSchema.Type =
  PersonSchema::{ name = "Alice", age = 30 }

-- ==============================================================================
-- Final Expression
-- ==============================================================================

-- The final expression in the file is the result
in { greeting
   , person
   , config
   , myDeployment
   , doubled
   , result1
   , result2
   }
