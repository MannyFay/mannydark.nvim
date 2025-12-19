# ==============================================================================
# Comprehensive Nix Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This file demonstrates all major Nix language features
# for syntax highlighting purposes.

# ==============================================================================
# Comments
# ==============================================================================

# Single line comment

/* Multi-line
   block comment */

/*
  Nested /* comments */ are NOT supported in Nix
*/

# ==============================================================================
# Basic Values
# ==============================================================================

{
  # Integers
  integer = 42;
  negative = -17;
  zero = 0;

  # Floating-point numbers
  float = 3.14159;
  scientific = 6.022e23;
  negativeFloat = -2.71828;

  # Booleans
  boolTrue = true;
  boolFalse = false;

  # Null
  nullValue = null;

  # Strings (double-quoted)
  simpleString = "Hello, Nix!";
  withEscapes = "Line 1\nLine 2\tTabbed";
  withQuotes = "She said \"Hello!\"";
  withBackslash = "C:\\Users\\Name";
  withDollar = "Price: \${10}";  # Escaped interpolation

  # String interpolation
  name = "World";
  greeting = "Hello, ${name}!";
  expression = "2 + 2 = ${toString (2 + 2)}";

  # Multi-line strings (indented strings)
  multiLine = ''
    This is a multi-line string.
    Leading whitespace is stripped.
    ${name} can be interpolated.
  '';

  multiLineWithEscapes = ''
    Literal ''${} - escaped interpolation
    Two single quotes: '''
    Dollar sign: ''$
  '';

  # Paths (unquoted, must contain /)
  absolutePath = /etc/nixos/configuration.nix;
  relativePath = ./flake.nix;
  homeRelative = ~/Documents;
  parentPath = ../parent/file.nix;

  # Path interpolation
  configDir = ./config;
  configFile = "${configDir}/settings.json";

  # URIs
  httpUrl = https://nixos.org;
  gitUrl = git://github.com/NixOS/nixpkgs;
  fileUrl = file:///home/user/file.txt;

  # Antiquotation (string interpolation)
  pkg = pkgs.hello;
  derivationPath = "${pkg}";
  withPath = "${./script.sh}";
}

# ==============================================================================
# Lists
# ==============================================================================

{
  # Simple lists
  emptyList = [];
  numberList = [1 2 3 4 5];
  stringList = ["a" "b" "c"];
  mixedList = [1 "two" true null];

  # Nested lists
  nestedList = [[1 2] [3 4] [5 6]];

  # List with expressions
  computedList = [
    (1 + 1)
    (if true then "yes" else "no")
    (builtins.toString 42)
  ];

  # List concatenation
  list1 = [1 2 3];
  list2 = [4 5 6];
  concatenated = list1 ++ list2;

  # List operations (builtins)
  listLength = builtins.length [1 2 3];
  listHead = builtins.head [1 2 3];
  listTail = builtins.tail [1 2 3];
  listElem = builtins.elemAt [1 2 3] 1;
  listConcat = builtins.concatLists [[1 2] [3 4]];
  listMap = builtins.map (x: x * 2) [1 2 3];
  listFilter = builtins.filter (x: x > 2) [1 2 3 4 5];
  listFold = builtins.foldl' (acc: x: acc + x) 0 [1 2 3];
  listSort = builtins.sort (a: b: a < b) [3 1 2];
  listGenerate = builtins.genList (x: x * x) 5;
}

# ==============================================================================
# Attribute Sets (Records)
# ==============================================================================

{
  # Simple attribute set
  person = {
    name = "Alice";
    age = 30;
    email = "alice@example.com";
  };

  # Nested attribute sets
  config = {
    server = {
      host = "localhost";
      port = 8080;
    };
    database = {
      host = "localhost";
      port = 5432;
      name = "mydb";
    };
  };

  # Dotted attribute paths
  dotted = {
    a.b.c = 1;
    a.b.d = 2;
    a.e = 3;
  };

  # Attribute access
  serverHost = config.server.host;
  serverPort = config.server.port;

  # Dynamic attribute names
  attrName = "dynamicKey";
  dynamic = {
    ${attrName} = "value";
    ${"another" + "Key"} = "anotherValue";
  };

  # Attribute set operations
  set1 = { a = 1; b = 2; };
  set2 = { b = 3; c = 4; };
  merged = set1 // set2;  # set2 overwrites

  # Recursive attribute sets
  recursive = rec {
    x = 1;
    y = 2;
    sum = x + y;  # Can reference other attributes
  };

  # Inheriting attributes
  inherited = {
    inherit name;  # Same as: name = name;
    inherit (person) age email;  # From another set
  };

  # Optional attribute access
  maybeValue = config.nonexistent or "default";
  safeAccess = config.server.host or "127.0.0.1";

  # Checking attribute existence
  hasAttr = config ? server;
  hasNestedAttr = config ? server.host;

  # Attribute set functions
  attrNames = builtins.attrNames { a = 1; b = 2; };
  attrValues = builtins.attrValues { a = 1; b = 2; };
  hasAttrFn = builtins.hasAttr "a" { a = 1; };
  getAttr = builtins.getAttr "a" { a = 1; };
  removeAttrs = builtins.removeAttrs { a = 1; b = 2; } ["a"];
  intersectAttrs = builtins.intersectAttrs { a = 1; b = 2; } { a = 3; c = 4; };
  listToAttrs = builtins.listToAttrs [
    { name = "a"; value = 1; }
    { name = "b"; value = 2; }
  ];
  mapAttrs = builtins.mapAttrs (name: value: value * 2) { a = 1; b = 2; };
  filterAttrs = lib.filterAttrs (name: value: value > 1) { a = 1; b = 2; c = 3; };
}

# ==============================================================================
# Functions
# ==============================================================================

{
  # Single argument function
  double = x: x * 2;

  # Multiple arguments (curried)
  add = a: b: a + b;
  multiply = a: b: c: a * b * c;

  # Attribute set argument (destructuring)
  greet = { name, greeting ? "Hello" }: "${greeting}, ${name}!";

  # With @-pattern (bind whole set)
  greetWithSet = args@{ name, ... }: "Hello, ${name}! (${builtins.toJSON args})";

  # With ellipsis (allow extra attributes)
  flexible = { required, optional ? "default", ... }: required + optional;

  # Function application
  result1 = double 21;
  result2 = add 1 2;
  result3 = greet { name = "World"; };
  result4 = greet { name = "Nix"; greeting = "Hi"; };

  # Partial application
  addFive = add 5;
  result5 = addFive 10;

  # Function composition
  compose = f: g: x: f (g x);
  doubleAndAdd5 = compose (x: x + 5) (x: x * 2);

  # Higher-order functions
  applyTwice = f: x: f (f x);
  result6 = applyTwice double 3;

  # Lambda with let
  complex = x:
    let
      doubled = x * 2;
      squared = x * x;
    in
      doubled + squared;

  # Recursive functions
  factorial = n:
    if n <= 1
    then 1
    else n * factorial (n - 1);

  fibonacci = n:
    if n <= 1
    then n
    else fibonacci (n - 1) + fibonacci (n - 2);

  # Using fix for recursion
  factorialFix = let
    fix = f: let x = f x; in x;
  in fix (self: n: if n <= 1 then 1 else n * self (n - 1));
}

# ==============================================================================
# Control Flow
# ==============================================================================

{
  # If-then-else (expression, not statement)
  x = 10;
  sign = if x > 0 then "positive" else if x < 0 then "negative" else "zero";

  # Nested conditionals
  grade = score:
    if score >= 90 then "A"
    else if score >= 80 then "B"
    else if score >= 70 then "C"
    else if score >= 60 then "D"
    else "F";

  # Boolean operations
  a = true;
  b = false;
  andResult = a && b;
  orResult = a || b;
  notResult = !a;
  implies = !a || b;  # a -> b

  # Comparison
  eq = 1 == 1;
  neq = 1 != 2;
  lt = 1 < 2;
  lte = 1 <= 2;
  gt = 2 > 1;
  gte = 2 >= 1;

  # Assertion
  assertExample = assert 1 + 1 == 2; "Math works!";

  # With assert in function
  safeDivide = a: b:
    assert b != 0;
    a / b;

  # Throw error
  validatePositive = x:
    if x > 0
    then x
    else throw "Value must be positive, got: ${toString x}";

  # Abort (like throw but more severe)
  mustExist = path:
    if builtins.pathExists path
    then path
    else abort "Required path does not exist: ${toString path}";
}

# ==============================================================================
# Let Expressions
# ==============================================================================

{
  # Basic let
  simpleResult = let
    x = 1;
    y = 2;
  in x + y;

  # Let with multiple bindings
  complexResult = let
    base = 10;
    multiplier = 2;
    offset = 5;
    intermediate = base * multiplier;
  in intermediate + offset;

  # Let with function definitions
  withFunctions = let
    square = x: x * x;
    double = x: x * 2;
    compose = f: g: x: f (g x);
    squareThenDouble = compose double square;
  in squareThenDouble 3;

  # Nested let
  nestedLet = let
    outer = 10;
  in let
    inner = outer * 2;
  in inner + outer;

  # Let with inherit
  letInherit = let
    x = 1;
    y = 2;
    result = { inherit x y; };
  in result;

  # Recursive let (using rec)
  recursiveLet = let
    # Note: regular let is not recursive
    # Use rec { } for recursive attribute sets
    fib = n:
      if n <= 1 then n
      else fib (n - 1) + fib (n - 2);
  in fib 10;
}

# ==============================================================================
# With Expression
# ==============================================================================

{
  # Basic with
  withExample = let
    attrs = { a = 1; b = 2; c = 3; };
  in with attrs; a + b + c;

  # With for packages
  # with pkgs; [ gcc cmake ninja ]

  # Nested with
  nested = let
    outer = { x = 1; };
    inner = { y = 2; };
  in with outer; with inner; x + y;

  # With and explicit scope
  # Inner scope takes precedence
  scopeExample = let
    attrs = { x = 1; };
    x = 100;
  in with attrs; x;  # Returns 1 (from attrs), not 100
}

# ==============================================================================
# Imports
# ==============================================================================

{
  # Import another Nix file
  importedConfig = import ./config.nix;

  # Import with arguments
  importedWithArgs = import ./module.nix { inherit pkgs; };

  # Import from path
  importedFromPath = import /etc/nixos/configuration.nix;

  # Import from URL (not recommended for reproducibility)
  # importedFromUrl = import (builtins.fetchurl "https://example.com/file.nix");

  # Import from fetchGit
  importedFromGit = import (builtins.fetchGit {
    url = "https://github.com/user/repo";
    ref = "main";
    rev = "abc123...";
  });

  # Import from fetchTarball
  importedFromTarball = import (builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/nixos-23.11.tar.gz";
    sha256 = "sha256:0...";
  });

  # Import pkgs
  pkgs = import <nixpkgs> {};
  pkgsWithConfig = import <nixpkgs> {
    config = { allowUnfree = true; };
    overlays = [];
  };
}

# ==============================================================================
# Derivations
# ==============================================================================

{
  # Basic derivation
  simpleDerivation = derivation {
    name = "hello";
    system = "x86_64-linux";
    builder = "/bin/sh";
    args = ["-c" "echo Hello > $out"];
  };

  # Using stdenv.mkDerivation
  packageExample = pkgs.stdenv.mkDerivation {
    pname = "my-package";
    version = "1.0.0";

    src = ./src;

    # Or fetch from URL
    # src = pkgs.fetchurl {
    #   url = "https://example.com/source.tar.gz";
    #   sha256 = "sha256-...";
    # };

    # Build inputs
    buildInputs = with pkgs; [
      zlib
      openssl
    ];

    nativeBuildInputs = with pkgs; [
      cmake
      pkg-config
    ];

    propagatedBuildInputs = with pkgs; [
      libffi
    ];

    # Phases
    configurePhase = ''
      cmake -B build -S .
    '';

    buildPhase = ''
      cmake --build build
    '';

    installPhase = ''
      mkdir -p $out/bin
      cp build/myapp $out/bin/
    '';

    checkPhase = ''
      ctest --test-dir build
    '';

    # Environment variables
    NIX_CFLAGS_COMPILE = "-O2";

    # Meta information
    meta = with pkgs.lib; {
      description = "My awesome package";
      homepage = "https://example.com";
      license = licenses.mit;
      maintainers = with maintainers; [ ];
      platforms = platforms.linux;
    };
  };

  # Using buildPythonPackage
  pythonPackage = pkgs.python3Packages.buildPythonPackage {
    pname = "my-python-package";
    version = "1.0.0";

    src = ./python-src;

    propagatedBuildInputs = with pkgs.python3Packages; [
      requests
      numpy
    ];

    checkInputs = with pkgs.python3Packages; [
      pytest
    ];
  };

  # Shell derivation
  shellDerivation = pkgs.mkShell {
    buildInputs = with pkgs; [
      nodejs
      yarn
      python3
    ];

    shellHook = ''
      echo "Development environment loaded!"
      export PATH="$PWD/node_modules/.bin:$PATH"
    '';
  };
}

# ==============================================================================
# Flakes
# ==============================================================================

# flake.nix structure:
/*
{
  description = "A sample flake";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixos-23.11";
    flake-utils.url = "github:numtide/flake-utils";

    # Input with specific revision
    home-manager = {
      url = "github:nix-community/home-manager";
      inputs.nixpkgs.follows = "nixpkgs";
    };
  };

  outputs = { self, nixpkgs, flake-utils, ... }@inputs:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = nixpkgs.legacyPackages.${system};
      in {
        packages = {
          default = pkgs.hello;
          myPackage = pkgs.callPackage ./package.nix { };
        };

        devShells.default = pkgs.mkShell {
          buildInputs = with pkgs; [ gcc gnumake ];
        };

        apps.default = {
          type = "app";
          program = "${pkgs.hello}/bin/hello";
        };

        checks = {
          build = self.packages.${system}.default;
        };
      }
    ) // {
      nixosConfigurations.myHost = nixpkgs.lib.nixosSystem {
        system = "x86_64-linux";
        modules = [ ./configuration.nix ];
      };

      homeConfigurations.myUser = home-manager.lib.homeManagerConfiguration {
        pkgs = nixpkgs.legacyPackages.x86_64-linux;
        modules = [ ./home.nix ];
      };

      overlays.default = final: prev: {
        myPackage = final.callPackage ./package.nix { };
      };
    };
}
*/

# ==============================================================================
# NixOS Module
# ==============================================================================

# module.nix structure:
{ config, lib, pkgs, ... }:

with lib;

let
  cfg = config.services.myService;
in {
  options.services.myService = {
    enable = mkEnableOption "My custom service";

    port = mkOption {
      type = types.port;
      default = 8080;
      description = "Port to listen on";
    };

    host = mkOption {
      type = types.str;
      default = "localhost";
      description = "Host to bind to";
    };

    user = mkOption {
      type = types.str;
      default = "myservice";
      description = "User to run service as";
    };

    package = mkOption {
      type = types.package;
      default = pkgs.myPackage;
      description = "Package to use";
    };

    settings = mkOption {
      type = types.submodule {
        options = {
          debug = mkOption {
            type = types.bool;
            default = false;
          };
          logLevel = mkOption {
            type = types.enum [ "debug" "info" "warn" "error" ];
            default = "info";
          };
        };
      };
      default = { };
      description = "Service settings";
    };

    extraConfig = mkOption {
      type = types.lines;
      default = "";
      description = "Extra configuration";
    };
  };

  config = mkIf cfg.enable {
    users.users.${cfg.user} = {
      isSystemUser = true;
      group = cfg.user;
    };

    users.groups.${cfg.user} = { };

    systemd.services.myService = {
      description = "My Custom Service";
      wantedBy = [ "multi-user.target" ];
      after = [ "network.target" ];

      serviceConfig = {
        Type = "simple";
        User = cfg.user;
        ExecStart = "${cfg.package}/bin/myservice --port ${toString cfg.port}";
        Restart = "on-failure";
        RestartSec = 5;
      };

      environment = {
        HOST = cfg.host;
        PORT = toString cfg.port;
        LOG_LEVEL = cfg.settings.logLevel;
      };
    };

    networking.firewall.allowedTCPPorts = mkIf cfg.enable [ cfg.port ];
  };
}

# ==============================================================================
# Builtin Functions
# ==============================================================================

{
  # Type checking
  isInt = builtins.isInt 42;
  isFloat = builtins.isFloat 3.14;
  isBool = builtins.isBool true;
  isString = builtins.isString "hello";
  isPath = builtins.isPath ./file.nix;
  isList = builtins.isList [1 2 3];
  isAttrs = builtins.isAttrs { a = 1; };
  isFunction = builtins.isFunction (x: x);
  isNull = builtins.isNull null;

  # Type conversion
  toString = builtins.toString 42;
  toJSON = builtins.toJSON { a = 1; };
  fromJSON = builtins.fromJSON ''{"a": 1}'';
  toXML = builtins.toXML { a = 1; };
  toFile = builtins.toFile "hello.txt" "Hello, World!";

  # String operations
  stringLength = builtins.stringLength "hello";
  substring = builtins.substring 0 3 "hello";
  match = builtins.match "([a-z]+)" "hello";
  split = builtins.split "," "a,b,c";
  concatStringsSep = builtins.concatStringsSep ", " ["a" "b" "c"];
  replaceStrings = builtins.replaceStrings ["a" "b"] ["x" "y"] "aabb";
  hashString = builtins.hashString "sha256" "hello";

  # List operations
  elem = builtins.elem 2 [1 2 3];
  elemAt = builtins.elemAt [1 2 3] 1;
  head = builtins.head [1 2 3];
  tail = builtins.tail [1 2 3];
  length = builtins.length [1 2 3];
  map = builtins.map (x: x * 2) [1 2 3];
  filter = builtins.filter (x: x > 1) [1 2 3];
  foldl = builtins.foldl' (acc: x: acc + x) 0 [1 2 3];
  concatLists = builtins.concatLists [[1 2] [3 4]];
  sort = builtins.sort (a: b: a < b) [3 1 2];
  genList = builtins.genList (x: x * x) 5;
  all = builtins.all (x: x > 0) [1 2 3];
  any = builtins.any (x: x > 2) [1 2 3];

  # Attribute set operations
  attrNames = builtins.attrNames { b = 2; a = 1; };
  attrValues = builtins.attrValues { a = 1; b = 2; };
  getAttr = builtins.getAttr "a" { a = 1; };
  hasAttr = builtins.hasAttr "a" { a = 1; };
  intersectAttrs = builtins.intersectAttrs { a = 1; b = 2; } { b = 3; c = 4; };
  removeAttrs = builtins.removeAttrs { a = 1; b = 2; } ["a"];
  listToAttrs = builtins.listToAttrs [{ name = "a"; value = 1; }];
  catAttrs = builtins.catAttrs "x" [{ x = 1; } { x = 2; } { y = 3; }];
  mapAttrs = builtins.mapAttrs (n: v: v * 2) { a = 1; b = 2; };
  functionArgs = builtins.functionArgs ({ a, b ? 2 }: a + b);

  # Path operations
  pathExists = builtins.pathExists /etc/passwd;
  readFile = builtins.readFile /etc/hostname;
  readDir = builtins.readDir ./.;
  baseNameOf = builtins.baseNameOf /path/to/file.txt;
  dirOf = builtins.dirOf /path/to/file.txt;
  toPath = builtins.toPath "/etc/passwd";

  # Derivation operations
  derivationStrict = builtins.derivation {
    name = "test";
    system = "x86_64-linux";
    builder = "/bin/sh";
  };
  placeholder = builtins.placeholder "out";

  # Math operations
  add = builtins.add 1 2;
  sub = builtins.sub 5 3;
  mul = builtins.mul 3 4;
  div = builtins.div 10 3;
  bitAnd = builtins.bitAnd 12 10;
  bitOr = builtins.bitOr 12 10;
  bitXor = builtins.bitXor 12 10;
  lessThan = builtins.lessThan 1 2;
  floor = builtins.floor 3.7;
  ceil = builtins.ceil 3.2;

  # Misc
  currentSystem = builtins.currentSystem;
  currentTime = builtins.currentTime;
  nixVersion = builtins.nixVersion;
  langVersion = builtins.langVersion;
  storeDir = builtins.storeDir;
  nixPath = builtins.nixPath;

  # Fetchers
  fetchurl = builtins.fetchurl {
    url = "https://example.com/file.tar.gz";
    sha256 = "sha256-...";
  };
  fetchGit = builtins.fetchGit {
    url = "https://github.com/user/repo";
    ref = "main";
  };
  fetchTarball = builtins.fetchTarball {
    url = "https://example.com/archive.tar.gz";
    sha256 = "sha256-...";
  };

  # Tracing (for debugging)
  traceVal = builtins.trace "Debug message" 42;
  traceSeq = builtins.traceSeq { evaluated = true; } 42;

  # Import/parsing
  import = builtins.import ./file.nix;
  scopedImport = builtins.scopedImport { x = 1; } ./file.nix;
  parseDrvName = builtins.parseDrvName "hello-1.2.3";
}

# ==============================================================================
# lib Functions (nixpkgs library)
# ==============================================================================

{ lib }:

with lib;

{
  # Attribute set functions
  attrByPath = attrByPath ["a" "b"] "default" { a.b = 1; };
  setAttrByPath = setAttrByPath ["a" "b"] 42;
  getAttrFromPath = getAttrFromPath ["a" "b"] { a.b = 1; };
  hasAttrByPath = hasAttrByPath ["a" "b"] { a.b = 1; };
  attrVals = attrVals ["a" "b"] { a = 1; b = 2; c = 3; };
  filterAttrs = filterAttrs (n: v: v > 1) { a = 1; b = 2; };
  mapAttrsToList = mapAttrsToList (n: v: "${n}=${toString v}") { a = 1; b = 2; };
  optionalAttrs = optionalAttrs true { enabled = true; };
  recursiveUpdate = recursiveUpdate { a.b = 1; } { a.c = 2; };
  foldAttrs = foldAttrs (n: a: a + n) 0 [{ a = 1; } { a = 2; }];
  zipAttrs = zipAttrs [{ a = 1; } { a = 2; b = 3; }];

  # List functions
  singleton = singleton 42;
  optional = optional true "value";
  optionals = optionals true [ "a" "b" ];
  unique = unique [1 1 2 2 3];
  flatten = flatten [[1 2] [3 [4 5]]];
  reverseList = reverseList [1 2 3];
  range = range 1 5;
  imap0 = imap0 (i: v: "${toString i}: ${v}") ["a" "b" "c"];
  partition = partition (x: x > 2) [1 2 3 4 5];
  zipLists = zipLists [1 2 3] ["a" "b" "c"];
  sublist = sublist 1 2 [1 2 3 4 5];
  last = last [1 2 3];
  init = init [1 2 3];

  # String functions
  concatStrings = concatStrings ["a" "b" "c"];
  concatStringsSep = concatStringsSep ", " ["a" "b" "c"];
  concatMapStrings = concatMapStrings (x: x + ",") ["a" "b" "c"];
  concatMapStringsSep = concatMapStringsSep "-" toString [1 2 3];
  splitString = splitString "," "a,b,c";
  hasPrefix = hasPrefix "pre" "prefix";
  hasSuffix = hasSuffix "fix" "suffix";
  removePrefix = removePrefix "pre" "prefix";
  removeSuffix = removeSuffix "fix" "suffix";
  optionalString = optionalString true "value";
  fixedWidthString = fixedWidthString 10 " " "hello";
  toLower = toLower "HELLO";
  toUpper = toUpper "hello";
  escapeShellArg = escapeShellArg "hello world";
  escapeShellArgs = escapeShellArgs ["a" "b c"];
  stringToCharacters = stringToCharacters "abc";
  stringAsChars = stringAsChars toUpper "hello";

  # Trivial functions
  id = id 42;
  const = const 1 2;
  flip = flip (a: b: a - b) 1 2;
  pipe = pipe 1 [(x: x + 1) (x: x * 2)];
  # compose = compose [(x: x * 2) (x: x + 1)] 1;

  # Type checking
  isString = isString "hello";
  isInt = isInt 42;
  isList = isList [1 2 3];
  isAttrs = isAttrs { };
  isFunction = isFunction (x: x);
  isBool = isBool true;
  isPath = isPath ./file;
  isDerivation = isDerivation pkgs.hello;

  # Assertions
  assertMsg = assertMsg true "Error message";
  assertOneOf = assertOneOf "value" "name" ["value" "other"];

  # Generators (for config files)
  toINI = generators.toINI { } { section = { key = "value"; }; };
  toJSON = generators.toJSON { } { a = 1; };
  toYAML = generators.toYAML { } { a = 1; };
  toTOML = generators.toTOML { } { a = 1; };
  toPretty = generators.toPretty { } { a = 1; };

  # Version comparison
  versionOlder = versionOlder "1.0" "2.0";
  versionAtLeast = versionAtLeast "2.0" "1.0";
  compareVersions = compareVersions "1.0" "2.0";
}

# ==============================================================================
# End of Nix Sample
# ==============================================================================
