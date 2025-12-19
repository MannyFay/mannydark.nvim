#!/usr/bin/env bash
# ==============================================================================
# Comprehensive Bash Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This script demonstrates all major Bash language features
# for syntax highlighting purposes.

# Strict mode
set -euo pipefail
IFS=$'\n\t'

# ==============================================================================
# Shebang variations (commented for reference)
# ==============================================================================
# #!/bin/bash
# #!/usr/bin/bash
# #!/usr/bin/env bash

# ==============================================================================
# Comments
# ==============================================================================

# This is a single-line comment

: '
This is a multi-line comment
using a null command with a here-string.
It can span multiple lines.
'

# ==============================================================================
# Variable Declarations
# ==============================================================================

# Simple variables
NAME="Alice"
AGE=30
GREETING='Hello, World!'

# Readonly variables
readonly CONSTANT="This cannot be changed"
declare -r ANOTHER_CONSTANT=42

# Integer variables
declare -i INTEGER_VAR=100
INTEGER_VAR+=50  # Now 150

# Array variables
declare -a INDEXED_ARRAY=("one" "two" "three" "four" "five")
declare -A ASSOCIATIVE_ARRAY=(
    [name]="Alice"
    [age]="30"
    [city]="Wonderland"
)

# Export variables
export PATH="$HOME/bin:$PATH"
export EDITOR="vim"

# Local variables (inside functions)
function demo_local() {
    local LOCAL_VAR="I'm local"
    echo "$LOCAL_VAR"
}

# Nameref (reference) variables
function demo_nameref() {
    local -n ref=$1
    ref="Modified through nameref"
}

# ==============================================================================
# String Literals
# ==============================================================================

# Single-quoted strings (literal)
LITERAL='No $expansion or `backticks` here'
LITERAL_WITH_QUOTES='It'\''s a nice day'

# Double-quoted strings (with expansion)
EXPANDED="Hello, $NAME! You are $AGE years old."
EXPANDED_BRACES="Hello, ${NAME}! You are ${AGE} years old."
COMMAND_EXPANSION="Today is $(date +%Y-%m-%d)"
ARITHMETIC="The sum is $((5 + 3))"

# ANSI-C quoting
ANSI_C=$'Line 1\nLine 2\tTabbed'
UNICODE=$'\u03BB'  # λ

# Here documents
cat << 'EOF'
This is a heredoc with no variable expansion.
$NAME will not be expanded.
EOF

cat << EOF
This is a heredoc with variable expansion.
Hello, $NAME!
Today is $(date).
EOF

cat <<- EOF
	This heredoc strips leading tabs.
	Useful for indentation.
EOF

# Here strings
grep "pattern" <<< "This is a here string"

# ==============================================================================
# Numeric Operations
# ==============================================================================

# Arithmetic expansion
RESULT=$((5 + 3 * 2))
REMAINDER=$((17 % 5))
POWER=$((2 ** 10))
INCREMENT=$((AGE++))
DECREMENT=$((AGE--))

# Let command
let "A = 5 + 3"
let "B = A * 2"
let "A++"

# Declare with arithmetic
declare -i NUM=10
NUM="NUM + 5"  # Automatic arithmetic evaluation

# Hexadecimal, octal, binary
HEX=$((16#FF))        # 255
OCTAL=$((8#755))      # 493
BINARY=$((2#101010))  # 42
BASE=$((36#HELLO))    # Base-36

# Floating point with bc
FLOAT=$(echo "scale=4; 22 / 7" | bc)

# ==============================================================================
# Arrays
# ==============================================================================

# Indexed arrays
FRUITS=("apple" "banana" "cherry")
FRUITS+=("date")  # Append
FRUITS[10]="elderberry"  # Sparse array

# Array operations
echo "First element: ${FRUITS[0]}"
echo "All elements: ${FRUITS[@]}"
echo "All elements quoted: ${FRUITS[*]}"
echo "Array length: ${#FRUITS[@]}"
echo "Element length: ${#FRUITS[0]}"
echo "Array indices: ${!FRUITS[@]}"
echo "Slice: ${FRUITS[@]:1:2}"

# Associative arrays
declare -A PERSON=(
    [name]="Bob"
    [age]=25
    [email]="bob@example.com"
)

echo "Name: ${PERSON[name]}"
echo "Keys: ${!PERSON[@]}"

# Array iteration
for fruit in "${FRUITS[@]}"; do
    echo "Fruit: $fruit"
done

for key in "${!PERSON[@]}"; do
    echo "$key: ${PERSON[$key]}"
done

# ==============================================================================
# String Operations
# ==============================================================================

STRING="Hello, World!"

# Length
echo "Length: ${#STRING}"

# Substring
echo "Substring: ${STRING:0:5}"
echo "From position: ${STRING:7}"

# Substitution
echo "Replace first: ${STRING/o/0}"
echo "Replace all: ${STRING//o/0}"
echo "Replace prefix: ${STRING/#Hello/Hi}"
echo "Replace suffix: ${STRING/%World!/Universe!}"

# Case modification
echo "Uppercase: ${STRING^^}"
echo "Lowercase: ${STRING,,}"
echo "Capitalize: ${STRING^}"

# Trimming
FILE="/path/to/file.txt"
echo "Remove prefix: ${FILE#*/}"
echo "Remove longest prefix: ${FILE##*/}"
echo "Remove suffix: ${FILE%.*}"
echo "Remove longest suffix: ${FILE%%/*}"

# Default values
echo "Default if unset: ${UNSET_VAR:-default}"
echo "Default if empty: ${EMPTY_VAR:=default}"
echo "Error if unset: ${NAME:?Variable is not set}"
echo "Value if set: ${NAME:+Name is set}"

# Indirect expansion
VAR_NAME="NAME"
echo "Indirect: ${!VAR_NAME}"

# ==============================================================================
# Control Flow
# ==============================================================================

# If statement
if [[ "$NAME" == "Alice" ]]; then
    echo "Hello, Alice!"
elif [[ "$NAME" == "Bob" ]]; then
    echo "Hello, Bob!"
else
    echo "Hello, stranger!"
fi

# Test operators
if [[ -f "/etc/passwd" ]]; then
    echo "File exists"
fi

if [[ -d "/tmp" && -w "/tmp" ]]; then
    echo "Directory exists and is writable"
fi

if [[ "$STRING" =~ ^Hello ]]; then
    echo "Starts with Hello"
fi

if [[ "$AGE" -ge 18 ]]; then
    echo "Adult"
fi

# Case statement
case "$NAME" in
    Alice|Bob)
        echo "Known user"
        ;;
    Admin*)
        echo "Administrator"
        ;;
    [0-9]*)
        echo "Starts with digit"
        ;;
    *)
        echo "Unknown"
        ;;
esac

# For loops
for i in 1 2 3 4 5; do
    echo "Number: $i"
done

for i in {1..10}; do
    echo "Count: $i"
done

for i in {0..100..10}; do
    echo "Step: $i"
done

for file in *.txt; do
    echo "File: $file"
done

for ((i = 0; i < 10; i++)); do
    echo "C-style: $i"
done

# While loop
COUNTER=0
while [[ $COUNTER -lt 5 ]]; do
    echo "Counter: $COUNTER"
    ((COUNTER++))
done

# Until loop
COUNTER=5
until [[ $COUNTER -eq 0 ]]; do
    echo "Countdown: $COUNTER"
    ((COUNTER--))
done

# Infinite loop with break
while true; do
    read -r -p "Continue? (y/n) " choice
    case "$choice" in
        n|N) break ;;
        y|Y) continue ;;
    esac
done

# Select statement
PS3="Choose a fruit: "
select fruit in "apple" "banana" "cherry" "quit"; do
    case "$fruit" in
        quit) break ;;
        *) echo "You chose: $fruit" ;;
    esac
done

# ==============================================================================
# Functions
# ==============================================================================

# Simple function
function greet() {
    echo "Hello, $1!"
}

# Alternative syntax
say_goodbye() {
    echo "Goodbye, $1!"
}

# Function with return value
add() {
    local a=$1
    local b=$2
    echo $((a + b))
}

# Function with return status
is_even() {
    local num=$1
    if (( num % 2 == 0 )); then
        return 0
    else
        return 1
    fi
}

# Function with local variables
calculate() {
    local -i x=$1
    local -i y=$2
    local -i result=$((x * y))
    echo "$result"
}

# Function with default parameters
greet_with_default() {
    local name=${1:-World}
    echo "Hello, $name!"
}

# Recursive function
factorial() {
    local n=$1
    if (( n <= 1 )); then
        echo 1
    else
        local prev=$(factorial $((n - 1)))
        echo $((n * prev))
    fi
}

# Function with array parameter
print_array() {
    local -n arr=$1  # nameref
    for item in "${arr[@]}"; do
        echo "  - $item"
    done
}

# ==============================================================================
# Command Execution
# ==============================================================================

# Simple command
ls -la

# Command with options
grep -r --include="*.txt" "pattern" /path/to/search

# Command substitution
CURRENT_DIR=$(pwd)
FILE_COUNT=$(ls | wc -l)
TODAY=$(date +%Y-%m-%d)

# Old-style command substitution (backticks)
OLD_STYLE=`date`

# Process substitution
diff <(sort file1.txt) <(sort file2.txt)
tee >(grep "error" > errors.txt) >(grep "warning" > warnings.txt)

# Pipelines
cat file.txt | grep "pattern" | sort | uniq -c | sort -rn

# Command chaining
command1 && command2  # AND
command1 || command2  # OR
command1; command2    # Sequential
command1 & command2   # Background

# Grouping
{ command1; command2; command3; }  # Current shell
(command1; command2; command3)     # Subshell

# Coprocess
coproc MYPROC { while read -r line; do echo "Received: $line"; done; }

# ==============================================================================
# Redirection
# ==============================================================================

# Output redirection
echo "Hello" > output.txt       # Overwrite
echo "World" >> output.txt      # Append

# Input redirection
while read -r line; do
    echo "Line: $line"
done < input.txt

# Stderr redirection
command 2> errors.txt
command 2>> errors.txt
command &> all.txt              # Both stdout and stderr
command > output.txt 2>&1       # Redirect stderr to stdout

# Descriptor manipulation
exec 3>&1                       # Save stdout
exec 3>&-                       # Close descriptor
exec 4<> file.txt               # Open for read/write

# Null device
command > /dev/null 2>&1

# ==============================================================================
# Pattern Matching
# ==============================================================================

# Glob patterns
shopt -s extglob nullglob globstar

# Basic globs
echo *.txt           # Match .txt files
echo file?.txt       # Single character
echo file[0-9].txt   # Character class
echo **/*.txt        # Recursive (globstar)

# Extended globs
echo ?(pattern)      # Zero or one
echo *(pattern)      # Zero or more
echo +(pattern)      # One or more
echo @(pat1|pat2)    # Exactly one
echo !(pattern)      # Anything except

# Pattern matching in case
case "$filename" in
    *.tar.gz|*.tgz) echo "Gzipped tarball" ;;
    *.tar.bz2|*.tbz2) echo "Bzipped tarball" ;;
    *.zip) echo "ZIP archive" ;;
esac

# ==============================================================================
# Regular Expressions
# ==============================================================================

# Bash regex matching
if [[ "$email" =~ ^[A-Za-z0-9._%+-]+@[A-Za-z0-9.-]+\.[A-Za-z]{2,}$ ]]; then
    echo "Valid email"
fi

# Capturing groups
if [[ "John Smith" =~ ^([A-Za-z]+)\ ([A-Za-z]+)$ ]]; then
    echo "First name: ${BASH_REMATCH[1]}"
    echo "Last name: ${BASH_REMATCH[2]}"
fi

# ==============================================================================
# Error Handling
# ==============================================================================

# Exit on error
set -e

# Custom error handler
trap 'echo "Error on line $LINENO"' ERR

# Cleanup on exit
cleanup() {
    rm -f /tmp/tempfile.$$
    echo "Cleanup complete"
}
trap cleanup EXIT

# Signal handling
trap 'echo "Caught SIGINT"; exit 1' SIGINT
trap 'echo "Caught SIGTERM"; exit 1' SIGTERM

# Disable errexit temporarily
set +e
risky_command
status=$?
set -e

# Or pattern
risky_command || true
risky_command || { echo "Failed"; exit 1; }

# ==============================================================================
# Input/Output
# ==============================================================================

# Read input
read -r -p "Enter your name: " name
read -r -s -p "Enter password: " password
read -r -t 10 -p "Quick response: " response  # Timeout
read -r -n 1 -p "Press any key..." key        # Single char

# Read into array
read -r -a words <<< "one two three"

# Read file line by line
while IFS= read -r line; do
    echo "Line: $line"
done < file.txt

# printf formatting
printf "Name: %s, Age: %d\n" "$NAME" "$AGE"
printf "Hex: %x, Octal: %o\n" 255 255
printf "Float: %.2f\n" 3.14159
printf "Padded: %10s\n" "hello"
printf "Left: %-10s|\n" "hello"

# ==============================================================================
# Debugging
# ==============================================================================

# Trace mode
set -x  # Enable
set +x  # Disable

# Verbose mode
set -v

# Debug a specific section
{
    set -x
    echo "Debugging this section"
    command1
    command2
    set +x
}

# Debug trap
trap 'echo "DEBUG: $BASH_COMMAND"' DEBUG

# PS4 for trace output
export PS4='+ ${BASH_SOURCE}:${LINENO}:${FUNCNAME[0]}: '

# ==============================================================================
# Special Variables
# ==============================================================================

echo "Script name: $0"
echo "First argument: $1"
echo "All arguments: $@"
echo "All arguments as string: $*"
echo "Argument count: $#"
echo "Exit status: $?"
echo "Process ID: $$"
echo "Parent PID: $PPID"
echo "Last background PID: $!"
echo "Options: $-"
echo "Random: $RANDOM"
echo "Line number: $LINENO"
echo "Seconds: $SECONDS"
echo "Bash version: $BASH_VERSION"

# ==============================================================================
# Conditional Expressions
# ==============================================================================

# File tests
[[ -e file ]]     # Exists
[[ -f file ]]     # Regular file
[[ -d dir ]]      # Directory
[[ -L link ]]     # Symbolic link
[[ -r file ]]     # Readable
[[ -w file ]]     # Writable
[[ -x file ]]     # Executable
[[ -s file ]]     # Not empty
[[ file1 -nt file2 ]]  # Newer than
[[ file1 -ot file2 ]]  # Older than

# String tests
[[ -z "$str" ]]   # Empty
[[ -n "$str" ]]   # Not empty
[[ "$a" == "$b" ]]
[[ "$a" != "$b" ]]
[[ "$a" < "$b" ]]
[[ "$a" > "$b" ]]
[[ "$a" =~ regex ]]

# Integer tests
[[ $a -eq $b ]]   # Equal
[[ $a -ne $b ]]   # Not equal
[[ $a -lt $b ]]   # Less than
[[ $a -le $b ]]   # Less or equal
[[ $a -gt $b ]]   # Greater than
[[ $a -ge $b ]]   # Greater or equal

# ==============================================================================
# Main Script
# ==============================================================================

main() {
    echo "=== Bash Sample Script ==="

    # Function calls
    greet "World"
    RESULT=$(add 5 3)
    echo "5 + 3 = $RESULT"

    # Arrays
    echo "Fruits: ${FRUITS[*]}"

    # Factorial
    echo "Factorial of 5: $(factorial 5)"

    echo "Done!"
}

# Run main if script is executed directly
if [[ "${BASH_SOURCE[0]}" == "${0}" ]]; then
    main "$@"
fi
