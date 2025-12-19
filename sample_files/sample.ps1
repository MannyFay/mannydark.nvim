<#
.SYNOPSIS
    Comprehensive PowerShell Sample - Syntax Highlighting Demonstration

.DESCRIPTION
    This script demonstrates all major PowerShell language features
    for syntax highlighting purposes.

.NOTES
    Author: Sample Author
    Version: 1.0.0
#>

#Requires -Version 7.0
#Requires -Modules @{ ModuleName="Microsoft.PowerShell.Utility"; ModuleVersion="7.0.0" }

using namespace System.Collections.Generic
using namespace System.IO
using namespace System.Text

# ==============================================================================
# Strict Mode and Preferences
# ==============================================================================

Set-StrictMode -Version Latest
$ErrorActionPreference = 'Stop'
$VerbosePreference = 'Continue'

# ==============================================================================
# Comments
# ==============================================================================

# This is a single-line comment

<#
This is a multi-line
block comment that can
span multiple lines.
#>

# ==============================================================================
# Variable Declarations
# ==============================================================================

# Simple variables
$Name = "Alice"
$Age = 30
$IsActive = $true
$Nothing = $null

# Typed variables
[string]$TypedString = "Hello"
[int]$TypedInt = 42
[double]$TypedDouble = 3.14159
[bool]$TypedBool = $true
[datetime]$TypedDate = Get-Date

# Constant and readonly
Set-Variable -Name CONSTANT -Value "Immutable" -Option Constant
Set-Variable -Name READONLY -Value "ReadOnly" -Option ReadOnly

# Scope modifiers
$script:ScriptVar = "Script scope"
$global:GlobalVar = "Global scope"
$local:LocalVar = "Local scope"
$private:PrivateVar = "Private scope"

# Environment variables
$env:MY_VAR = "Environment variable"
$env:PATH += ";C:\MyApp\bin"

# ==============================================================================
# Data Types and Literals
# ==============================================================================

# Numeric literals
$Integer = 42
$Negative = -17
$Hex = 0xFF
$Binary = 0b101010
$Octal = 0o755
$Long = 42L
$Decimal = 42D
$BigInteger = 42N
$Float = 3.14
$Scientific = 6.022e23

# With type suffix and underscores
$KB = 1KB      # 1024
$MB = 1MB      # 1048576
$GB = 1GB
$TB = 1TB
$PB = 1PB

# String literals
$SingleQuoted = 'No $expansion here'
$DoubleQuoted = "Hello, $Name! You are $Age years old."
$Escaped = "Line 1`nLine 2`tTabbed"
$Subexpression = "Result: $($Age * 2)"

# Here-strings
$HereStringSingle = @'
This is a here-string.
No $expansion occurs.
'@

$HereStringDouble = @"
This is a here-string.
With $Name expansion.
"@

# Character
$Char = [char]'A'
$Unicode = [char]0x03BB  # λ

# ==============================================================================
# Arrays and Collections
# ==============================================================================

# Arrays
$Array = @(1, 2, 3, 4, 5)
$EmptyArray = @()
$StringArray = @("apple", "banana", "cherry")
$TypedArray = [int[]]@(1, 2, 3)

# Array operations
$Array += 6                      # Append
$First = $Array[0]               # First element
$Last = $Array[-1]               # Last element
$Slice = $Array[1..3]            # Slice
$Length = $Array.Count           # Length

# Hashtable (dictionary)
$HashTable = @{
    Name   = "Alice"
    Age    = 30
    Email  = "alice@example.com"
}

# Ordered hashtable
$OrderedHash = [ordered]@{
    First  = 1
    Second = 2
    Third  = 3
}

# Generic collections
$List = [List[string]]::new()
$List.Add("item1")
$Dictionary = [Dictionary[string, int]]::new()
$Dictionary["key"] = 42

# ==============================================================================
# Operators
# ==============================================================================

# Arithmetic
$Sum = 5 + 3
$Diff = 10 - 3
$Product = 4 * 5
$Quotient = 20 / 4
$Modulo = 17 % 5
$Power = 2 ** 10

# Comparison
$Equal = 5 -eq 5
$NotEqual = 5 -ne 3
$GreaterThan = 5 -gt 3
$LessThan = 3 -lt 5
$GreaterOrEqual = 5 -ge 5
$LessOrEqual = 3 -le 5

# Case-sensitive comparison
$CaseSensitive = "Hello" -ceq "Hello"
$CaseInsensitive = "Hello" -ieq "hello"

# String operators
$Like = "Hello" -like "H*"
$NotLike = "Hello" -notlike "X*"
$Match = "Hello123" -match '\d+'
$Replace = "Hello" -replace 'l', 'L'
$Split = "a,b,c" -split ','
$Join = @("a", "b", "c") -join ','

# Logical operators
$And = $true -and $false
$Or = $true -or $false
$Xor = $true -xor $false
$Not = -not $false

# Containment
$Contains = @(1, 2, 3) -contains 2
$In = 2 -in @(1, 2, 3)
$NotIn = 4 -notin @(1, 2, 3)

# Type operators
$Is = "Hello" -is [string]
$IsNot = "Hello" -isnot [int]
$As = "42" -as [int]

# Range operator
$Range = 1..10
$ReverseRange = 10..1

# Null-coalescing (PowerShell 7+)
$NullCoalesce = $null ?? "default"
$NullCoalesceAssign ??= "assigned if null"

# Ternary operator (PowerShell 7+)
$Ternary = $Age -ge 18 ? "Adult" : "Minor"

# Pipeline chain operators (PowerShell 7+)
# command1 && command2  # Run command2 if command1 succeeds
# command1 || command2  # Run command2 if command1 fails

# ==============================================================================
# Control Flow
# ==============================================================================

# If/elseif/else
if ($Age -ge 18) {
    Write-Host "Adult"
}
elseif ($Age -ge 13) {
    Write-Host "Teenager"
}
else {
    Write-Host "Child"
}

# Switch statement
switch ($Name) {
    "Alice" { Write-Host "Hello, Alice!" }
    "Bob" { Write-Host "Hello, Bob!" }
    { $_ -match "^Admin" } { Write-Host "Hello, Admin!" }
    default { Write-Host "Hello, stranger!" }
}

# Switch with options
switch -Regex -CaseSensitive ($input) {
    "^\d+$" { Write-Host "Number" }
    "^[a-z]+$" { Write-Host "Lowercase" }
    "^[A-Z]+$" { Write-Host "Uppercase" }
}

# For loop
for ($i = 0; $i -lt 10; $i++) {
    Write-Host "Number: $i"
}

# ForEach loop
foreach ($item in $Array) {
    Write-Host "Item: $item"
}

# ForEach-Object (pipeline)
$Array | ForEach-Object {
    Write-Host "Pipeline item: $_"
}

# While loop
$counter = 0
while ($counter -lt 5) {
    Write-Host "Counter: $counter"
    $counter++
}

# Do-While loop
do {
    Write-Host "Do-while"
} while ($false)

# Do-Until loop
do {
    Write-Host "Do-until"
} until ($true)

# Break and Continue
foreach ($i in 1..10) {
    if ($i -eq 5) { continue }
    if ($i -eq 8) { break }
    Write-Host $i
}

# ==============================================================================
# Functions
# ==============================================================================

# Simple function
function Get-Greeting {
    "Hello, World!"
}

# Function with parameters
function Get-PersonalGreeting {
    param (
        [string]$Name = "World"
    )
    "Hello, $Name!"
}

# Advanced function with full parameter attributes
function Add-Numbers {
    [CmdletBinding()]
    [OutputType([int])]
    param (
        [Parameter(Mandatory, Position = 0, ValueFromPipeline)]
        [ValidateRange(0, 100)]
        [int]$First,

        [Parameter(Mandatory, Position = 1)]
        [ValidateNotNullOrEmpty()]
        [int]$Second,

        [Parameter()]
        [switch]$Double
    )

    begin {
        Write-Verbose "Starting Add-Numbers"
    }

    process {
        $result = $First + $Second
        if ($Double) {
            $result *= 2
        }
        $result
    }

    end {
        Write-Verbose "Finished Add-Numbers"
    }
}

# Function with multiple parameter sets
function Get-Data {
    [CmdletBinding(DefaultParameterSetName = 'ById')]
    param (
        [Parameter(ParameterSetName = 'ById', Mandatory)]
        [int]$Id,

        [Parameter(ParameterSetName = 'ByName', Mandatory)]
        [string]$Name,

        [Parameter(ParameterSetName = 'All')]
        [switch]$All
    )

    switch ($PSCmdlet.ParameterSetName) {
        'ById' { "Getting by ID: $Id" }
        'ByName' { "Getting by Name: $Name" }
        'All' { "Getting all" }
    }
}

# Recursive function
function Get-Factorial {
    param ([int]$N)

    if ($N -le 1) { return 1 }
    return $N * (Get-Factorial -N ($N - 1))
}

# ==============================================================================
# Classes
# ==============================================================================

class Person {
    # Properties
    [string]$Name
    [int]$Age
    hidden [string]$_secret

    # Static property
    static [int]$Count = 0

    # Constructor
    Person([string]$name, [int]$age) {
        $this.Name = $name
        $this.Age = $age
        [Person]::Count++
    }

    # Method
    [string] Greet() {
        return "Hello, I'm $($this.Name)"
    }

    # Static method
    static [Person] Create([string]$name) {
        return [Person]::new($name, 0)
    }

    # Override ToString
    [string] ToString() {
        return "$($this.Name) (Age: $($this.Age))"
    }
}

# Inheritance
class Employee : Person {
    [string]$Department
    [decimal]$Salary

    Employee([string]$name, [int]$age, [string]$dept) : base($name, $age) {
        $this.Department = $dept
    }

    [string] Greet() {
        return "$([Person]$this.Greet()), I work in $($this.Department)"
    }
}

# Enum
enum Status {
    Active = 1
    Inactive = 2
    Pending = 3
}

# Flags enum
[Flags()]
enum Permissions {
    None = 0
    Read = 1
    Write = 2
    Execute = 4
    All = 7
}

# ==============================================================================
# Error Handling
# ==============================================================================

# Try/Catch/Finally
try {
    $result = 1 / 0
}
catch [DivideByZeroException] {
    Write-Error "Cannot divide by zero: $_"
}
catch {
    Write-Error "Unknown error: $_"
}
finally {
    Write-Verbose "Cleanup"
}

# Trap statement
trap {
    Write-Error "Trapped: $_"
    continue
}

# Throw exception
function Test-Throw {
    throw "Custom error message"
}

# Custom exception
function Test-CustomException {
    $exception = [System.InvalidOperationException]::new("Custom message")
    throw $exception
}

# ErrorAction
Get-ChildItem -Path "nonexistent" -ErrorAction SilentlyContinue
Get-ChildItem -Path "nonexistent" -ErrorAction Stop
Get-ChildItem -Path "nonexistent" -ErrorAction Continue

# ==============================================================================
# Pipeline
# ==============================================================================

# Pipeline basics
Get-Process | Where-Object { $_.CPU -gt 10 } | Sort-Object -Property CPU -Descending | Select-Object -First 5

# Pipeline with ForEach-Object
1..10 | ForEach-Object { $_ * 2 }

# Pipeline with Where-Object
1..100 | Where-Object { $_ % 2 -eq 0 }

# Splatting
$params = @{
    Path        = "C:\Temp"
    Filter      = "*.txt"
    Recurse     = $true
    ErrorAction = "SilentlyContinue"
}
Get-ChildItem @params

# ==============================================================================
# Regular Expressions
# ==============================================================================

# Match operator
if ("Hello123" -match '(\w+)(\d+)') {
    $Matches[0]  # Full match
    $Matches[1]  # First group
    $Matches[2]  # Second group
}

# Named groups
if ("John Smith" -match '(?<first>\w+)\s+(?<last>\w+)') {
    $Matches.first
    $Matches.last
}

# Replace with regex
"Hello World" -replace 'World', 'PowerShell'
"abc123" -replace '\d', 'X'

# Select-String
Get-Content file.txt | Select-String -Pattern 'error' -CaseSensitive

# [regex] type accelerator
$regex = [regex]'^\d{3}-\d{4}$'
$regex.IsMatch("123-4567")

# ==============================================================================
# File Operations
# ==============================================================================

# Read file
$content = Get-Content -Path "file.txt"
$rawContent = Get-Content -Path "file.txt" -Raw
$bytes = Get-Content -Path "file.bin" -AsByteStream

# Write file
Set-Content -Path "output.txt" -Value "Hello, World!"
Add-Content -Path "output.txt" -Value "Appended line"
$content | Out-File -FilePath "output.txt" -Encoding UTF8

# File system operations
New-Item -Path "newfile.txt" -ItemType File
Copy-Item -Path "source.txt" -Destination "dest.txt"
Move-Item -Path "old.txt" -Destination "new.txt"
Remove-Item -Path "delete.txt"
Rename-Item -Path "old.txt" -NewName "new.txt"

# Test path
if (Test-Path -Path "file.txt") {
    Write-Host "File exists"
}

# ==============================================================================
# Modules and Dot Sourcing
# ==============================================================================

# Import module
Import-Module -Name Microsoft.PowerShell.Utility

# Dot sourcing
. .\script.ps1

# Call operator
& ".\script.ps1"

# Module manifest
# @{
#     RootModule = 'MyModule.psm1'
#     ModuleVersion = '1.0.0'
#     GUID = 'xxxxxxxx-xxxx-xxxx-xxxx-xxxxxxxxxxxx'
#     Author = 'Author Name'
#     Description = 'Module description'
#     FunctionsToExport = @('Get-Something', 'Set-Something')
# }

# ==============================================================================
# Jobs and Parallel Execution
# ==============================================================================

# Background job
$job = Start-Job -ScriptBlock {
    Start-Sleep -Seconds 5
    "Job completed"
}

# Wait for job
Wait-Job -Job $job
$result = Receive-Job -Job $job
Remove-Job -Job $job

# ForEach-Object -Parallel (PowerShell 7+)
1..10 | ForEach-Object -Parallel {
    Write-Output "Processing $_"
} -ThrottleLimit 5

# ThreadJob (PowerShell 7+)
$threadJob = Start-ThreadJob -ScriptBlock {
    "Thread job result"
}

# ==============================================================================
# DSC (Desired State Configuration)
# ==============================================================================

<#
Configuration MyConfiguration {
    param (
        [string[]]$ComputerName = 'localhost'
    )

    Import-DscResource -ModuleName PSDesiredStateConfiguration

    Node $ComputerName {
        File ExampleFile {
            Ensure          = 'Present'
            DestinationPath = 'C:\Temp\example.txt'
            Contents        = 'Hello, DSC!'
        }

        Service ExampleService {
            Name        = 'Spooler'
            StartupType = 'Automatic'
            State       = 'Running'
        }
    }
}
#>

# ==============================================================================
# Main Script
# ==============================================================================

function Main {
    Write-Host "=== PowerShell Sample Script ==="

    # Variable examples
    Write-Host "Name: $Name, Age: $Age"

    # Array examples
    Write-Host "Array: $($Array -join ', ')"

    # Function calls
    Write-Host (Get-PersonalGreeting -Name "World")
    Write-Host "5 + 3 = $(Add-Numbers -First 5 -Second 3)"

    # Class usage
    $person = [Person]::new("Alice", 30)
    Write-Host $person.Greet()

    # Factorial
    Write-Host "Factorial of 5: $(Get-Factorial -N 5)"

    Write-Host "Done!"
}

# Run main
Main
