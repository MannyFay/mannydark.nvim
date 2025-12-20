#!/usr/bin/env fish
# ==============================================================================
# Comprehensive Fish Shell Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This script demonstrates all major Fish shell language features
# for syntax highlighting purposes.

# ==============================================================================
# Comments
# ==============================================================================

# This is a single-line comment

# ==============================================================================
# Variable Declarations
# ==============================================================================

# Set variables
set NAME "Alice"
set AGE 30
set -l LOCAL_VAR "I'm local"
set -g GLOBAL_VAR "I'm global"
set -U UNIVERSAL_VAR "I'm universal (persistent)"
set -x EXPORTED_VAR "I'm exported"

# Erase variables
set -e TEMP_VAR

# List variable (array)
set FRUITS apple banana cherry date elderberry
set COLORS red green blue

# Append to list
set -a FRUITS fig
set -p FRUITS apricot  # Prepend

# Path-like variables
set -gx PATH $HOME/bin $PATH

# ==============================================================================
# String Literals
# ==============================================================================

# Single-quoted strings (almost literal)
set LITERAL 'No $expansion here'
set WITH_ESCAPE 'It\'s a nice day'

# Double-quoted strings (with expansion)
set EXPANDED "Hello, $NAME! You are $AGE years old."
set WITH_COMMAND "Today is (date +%Y-%m-%d)"

# Unquoted strings
set SIMPLE hello
set WITH_TILDE ~/documents

# Escape sequences
set NEWLINE "Line 1\nLine 2"
set TAB "Column1\tColumn2"
set UNICODE \U1F600  # 😀

# ==============================================================================
# Arrays and Indexing
# ==============================================================================

# Array indexing (1-based)
echo $FRUITS[1]       # First element
echo $FRUITS[-1]      # Last element
echo $FRUITS[2..4]    # Slice
echo $FRUITS[1 3 5]   # Specific indices

# Array length
echo (count $FRUITS)

# Iterate over array
for fruit in $FRUITS
    echo "Fruit: $fruit"
end

# Check if array contains element
if contains banana $FRUITS
    echo "We have bananas!"
end

# ==============================================================================
# String Operations
# ==============================================================================

# String length
echo (string length $NAME)

# String manipulation
string upper $NAME
string lower $NAME
string sub -s 1 -l 3 $NAME  # Substring
string trim "  hello  "
string replace o 0 "hello"
string replace -a o 0 "hello"  # Replace all

# String splitting
string split ',' "a,b,c,d"
string split0 < /etc/passwd  # Split on null

# String joining
string join ', ' $FRUITS

# String matching
string match -r '\d+' "abc123def"
string match '*.txt' file.txt

# ==============================================================================
# Control Flow
# ==============================================================================

# If statement
if test $AGE -ge 18
    echo "Adult"
else if test $AGE -ge 13
    echo "Teenager"
else
    echo "Child"
end

# Alternative test syntax
if [ $NAME = "Alice" ]
    echo "Hello, Alice!"
end

# Test with string matching
if string match -q '*.txt' $filename
    echo "Text file"
end

# And/Or
if test -f file.txt; and test -r file.txt
    echo "File exists and is readable"
end

if test ! -f file.txt; or test ! -r file.txt
    echo "File doesn't exist or isn't readable"
end

# Not
if not test -d /nonexistent
    echo "Directory doesn't exist"
end

# Switch/case
switch $NAME
    case Alice
        echo "Hello, Alice!"
    case Bob
        echo "Hello, Bob!"
    case 'Admin*'
        echo "Hello, Admin!"
    case '*'
        echo "Hello, stranger!"
end

# For loop
for i in (seq 1 10)
    echo "Number: $i"
end

for file in *.txt
    echo "File: $file"
end

# While loop
set counter 0
while test $counter -lt 5
    echo "Counter: $counter"
    set counter (math $counter + 1)
end

# Break and continue
for i in (seq 1 10)
    if test $i -eq 5
        continue
    end
    if test $i -eq 8
        break
    end
    echo $i
end

# ==============================================================================
# Functions
# ==============================================================================

# Simple function
function greet
    echo "Hello, $argv[1]!"
end

# Function with description
function say_goodbye -d "Say goodbye to someone"
    echo "Goodbye, $argv[1]!"
end

# Function with argument handling
function add -d "Add two numbers"
    set -l a $argv[1]
    set -l b $argv[2]
    math $a + $b
end

# Function with options
function myfunction -a arg1 arg2 -d "Function with named arguments"
    echo "Arg1: $arg1"
    echo "Arg2: $arg2"
    echo "All: $argv"
end

# Function with variable number of args
function print_all
    for arg in $argv
        echo "Arg: $arg"
    end
end

# Event handler function
function on_variable_change --on-variable MY_VAR
    echo "MY_VAR changed to: $MY_VAR"
end

# Function on signal
function on_sigint --on-signal INT
    echo "Caught SIGINT!"
end

# Function on event
function on_fish_exit --on-event fish_exit
    echo "Fish is exiting..."
end

# Recursive function
function factorial
    set -l n $argv[1]
    if test $n -le 1
        echo 1
    else
        set -l prev (factorial (math $n - 1))
        math $n \* $prev
    end
end

# Function scopes
function outer_function
    set -l outer_var "I'm outer"

    function inner_function
        echo $outer_var  # Can access outer_var
    end

    inner_function
end

# ==============================================================================
# Command Substitution
# ==============================================================================

# Parentheses syntax
set CURRENT_DIR (pwd)
set FILE_COUNT (ls | count)
set TODAY (date +%Y-%m-%d)

# Nested substitution
set RESULT (string upper (string trim "  hello  "))

# ==============================================================================
# Process and Job Control
# ==============================================================================

# Background jobs
sleep 10 &
set PID (jobs -lp)

# Job control
jobs        # List jobs
fg          # Bring to foreground
bg          # Send to background
disown $PID # Detach from shell

# Process substitution
diff (sort file1.txt | psub) (sort file2.txt | psub)

# ==============================================================================
# Pipelines and Redirection
# ==============================================================================

# Pipes
cat file.txt | grep pattern | sort | uniq -c

# Status pipe (preserves exit status)
false | true
echo $pipestatus  # (1 0)

# Redirection
echo "Hello" > output.txt       # Overwrite
echo "World" >> output.txt      # Append
cat < input.txt                 # Input

# Stderr redirection
command 2> errors.txt
command 2>> errors.txt
command &> all.txt              # Both
command 2>&1                    # Stderr to stdout

# Null device
command > /dev/null 2>&1

# Combining
begin
    echo "stdout"
    echo "stderr" >&2
end 2>&1 | cat

# ==============================================================================
# Arithmetic
# ==============================================================================

# Math command
math 5 + 3
math "5 * 3"
math "2 ^ 10"
math "sqrt(16)"
math "sin(3.14159 / 2)"
math "floor(3.7)"
math "ceil(3.2)"

# In expressions
set result (math $AGE + 1)
set pi (math "4 * atan(1)")

# Comparison
math "$AGE >= 18"  # Returns 1 (true) or 0 (false)

# ==============================================================================
# Conditionals and Tests
# ==============================================================================

# Test command
test -f file.txt    # File exists
test -d directory   # Directory exists
test -r file        # Readable
test -w file        # Writable
test -x file        # Executable
test -s file        # Not empty
test -L link        # Symbolic link

test $a -eq $b      # Numeric equal
test $a -ne $b      # Numeric not equal
test $a -lt $b      # Less than
test $a -le $b      # Less or equal
test $a -gt $b      # Greater than
test $a -ge $b      # Greater or equal

test "$a" = "$b"    # String equal
test "$a" != "$b"   # String not equal
test -n "$a"        # Not empty string
test -z "$a"        # Empty string

# Combining tests
test -f file -a -r file  # AND
test -f file -o -d dir   # OR
test ! -f file           # NOT

# ==============================================================================
# Completions
# ==============================================================================

# Define completion
complete -c mycommand -s h -l help -d "Show help"
complete -c mycommand -s v -l verbose -d "Verbose output"
complete -c mycommand -s f -l file -r -d "Specify file"
complete -c mycommand -a "start stop restart" -d "Action"

# Completion with function
function __fish_mycommand_complete
    echo "option1"
    echo "option2"
    echo "option3"
end
complete -c mycommand -a "(__fish_mycommand_complete)"

# Conditional completion
complete -c mycommand -n "__fish_seen_subcommand_from start" -a "fast slow"

# ==============================================================================
# Abbreviations
# ==============================================================================

# Define abbreviations
abbr -a gco git checkout
abbr -a gst git status
abbr -a ll 'ls -la'

# Position-dependent abbreviation
abbr -a -p anywhere L '| less'

# ==============================================================================
# Prompt
# ==============================================================================

function fish_prompt
    set -l last_status $status

    # Show status if non-zero
    if test $last_status -ne 0
        set_color red
        echo -n "[$last_status] "
    end

    # User and host
    set_color green
    echo -n (whoami)
    set_color normal
    echo -n "@"
    set_color blue
    echo -n (hostname -s)
    set_color normal
    echo -n ":"

    # Current directory
    set_color yellow
    echo -n (prompt_pwd)
    set_color normal

    # Git info
    if git rev-parse --is-inside-work-tree > /dev/null 2>&1
        set_color magenta
        echo -n " ("(git branch --show-current)")"
    end

    set_color normal
    echo -n "> "
end

function fish_right_prompt
    set_color brblack
    date "+%H:%M:%S"
end

# ==============================================================================
# Color and Formatting
# ==============================================================================

# Set colors
set_color red
echo "This is red"
set_color normal

set_color -b blue white
echo "White on blue"
set_color normal

set_color --bold green
echo "Bold green"
set_color normal

set_color --italics cyan
echo "Italic cyan"
set_color normal

set_color --underline
echo "Underlined"
set_color normal

# Using printf
printf '%s%s%s\n' (set_color red) "Red text" (set_color normal)

# ==============================================================================
# Error Handling
# ==============================================================================

# Check exit status
if command_that_might_fail
    echo "Success"
else
    echo "Failed with status: $status"
end

# Or pattern
command_that_might_fail; or echo "Failed"
command_that_might_fail; or return 1

# And pattern
command1; and command2; and command3

# Status variable
command
if test $status -ne 0
    echo "Command failed"
end

# ==============================================================================
# Configuration
# ==============================================================================

# Check if interactive
if status is-interactive
    # Interactive shell configuration
    set -g fish_greeting "Welcome to Fish!"
end

# Check if login shell
if status is-login
    # Login shell configuration
end

# Path configuration
fish_add_path ~/bin
fish_add_path -p /opt/homebrew/bin  # Prepend

# ==============================================================================
# Key Bindings
# ==============================================================================

# Bind key
bind \cg 'git status'
bind \cx\ce edit_command_buffer

# Vi mode
fish_vi_key_bindings

# Hybrid mode
fish_hybrid_key_bindings

# Default mode
fish_default_key_bindings

# ==============================================================================
# Useful Builtins
# ==============================================================================

# Type information
type -t echo      # builtin, function, file
type -a grep      # All definitions

# Functions
functions         # List all functions
functions greet   # Show function definition
functions -e greet  # Erase function

# Variables
set               # List all variables
set -S NAME       # Show variable info

# Source other files
source ~/.config/fish/aliases.fish

# Emit events
emit my_custom_event

# Status functions
status current-command
status current-filename
status current-function
status current-line-number
status fish-path
status is-block
status is-breakpoint
status is-command-substitution
status is-interactive
status is-login
status job-control
status stack-trace

# ==============================================================================
# Main Script
# ==============================================================================

function main
    echo "=== Fish Shell Sample Script ==="

    # Variables and arrays
    echo "Name: $NAME"
    echo "Fruits: $FRUITS"

    # Function call
    greet "World"
    echo "5 + 3 = "(add 5 3)

    # Factorial
    echo "Factorial of 5: "(factorial 5)

    # String operations
    echo "Uppercase: "(string upper $NAME)

    # Math
    echo "Pi ≈ "(math "4 * atan(1)")

    echo "Done!"
end

# Run main
main $argv
