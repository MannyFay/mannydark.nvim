#!/usr/bin/env zsh
# ==============================================================================
# Comprehensive Zsh Sample - Syntax Highlighting Demonstration
# ==============================================================================

# This script demonstrates all major Zsh language features
# for syntax highlighting purposes.

# Strict mode
setopt ERR_EXIT NO_UNSET PIPE_FAIL

# ==============================================================================
# Zsh-Specific Options
# ==============================================================================

# Enable extended globbing
setopt EXTENDED_GLOB
setopt NULL_GLOB
setopt GLOB_DOTS
setopt NUMERIC_GLOB_SORT

# Array options
setopt KSH_ARRAYS  # 0-indexed arrays (optional)
unsetopt KSH_ARRAYS  # Back to 1-indexed (zsh default)

# Other options
setopt AUTO_CD
setopt CORRECT
setopt HIST_IGNORE_DUPS
setopt SHARE_HISTORY

# ==============================================================================
# Variable Declarations
# ==============================================================================

# Simple variables
NAME="Alice"
AGE=30
typeset GREETING="Hello, World!"

# Integer and float
typeset -i INTEGER_VAR=100
typeset -F FLOAT_VAR=3.14159
typeset -E SCIENTIFIC=6.022e23

# Readonly
typeset -r CONSTANT="Immutable"
readonly ANOTHER_CONSTANT=42

# Export
typeset -x EXPORTED_VAR="I'm exported"
export PATH="$HOME/bin:$PATH"

# Local (in functions)
function demo_local() {
    local LOCAL_VAR="I'm local"
    typeset -l LOWERCASE="WILL BE LOWERCASE"
    typeset -u UPPERCASE="will be uppercase"
    echo "$LOCAL_VAR $LOWERCASE $UPPERCASE"
}

# Tied variables (like PATH/path)
typeset -T MYPATH mypath ':'

# ==============================================================================
# Arrays
# ==============================================================================

# Indexed arrays (1-indexed by default in zsh)
FRUITS=(apple banana cherry date elderberry)
FRUITS+=(fig)  # Append

# Array slicing
echo "First: $FRUITS[1]"
echo "Last: $FRUITS[-1]"
echo "Slice: $FRUITS[2,4]"
echo "All: $FRUITS[@]"
echo "Length: $#FRUITS"
echo "Indices: ${(k)FRUITS}"

# Array flags
echo "Join: ${(j:, :)FRUITS}"      # Join with separator
echo "Quote: ${(q)FRUITS[1]}"      # Quote
echo "Unique: ${(u)FRUITS}"        # Unique elements
echo "Sort: ${(o)FRUITS}"          # Sort ascending
echo "Reverse sort: ${(O)FRUITS}"  # Sort descending
echo "Uppercase: ${(U)FRUITS[1]}"  # Uppercase
echo "Lowercase: ${(L)FRUITS[1]}"  # Lowercase

# Associative arrays
typeset -A PERSON=(
    name "Bob"
    age 25
    email "bob@example.com"
)

echo "Name: $PERSON[name]"
echo "Keys: ${(k)PERSON}"
echo "Values: ${(v)PERSON}"

# Nested arrays simulation
typeset -A NESTED
NESTED[user.name]="Alice"
NESTED[user.email]="alice@example.com"

# ==============================================================================
# String Operations
# ==============================================================================

STRING="Hello, World!"

# Length
echo "Length: $#STRING"

# Case modification
echo "Uppercase: ${(U)STRING}"
echo "Lowercase: ${(L)STRING}"
echo "Capitalize: ${(C)STRING}"

# Substitution
echo "Replace: ${STRING/World/Zsh}"
echo "Replace all: ${STRING//o/0}"
echo "Replace pattern: ${STRING/(#m)World/Universe}"

# Splitting
WORDS=(${(s: :)STRING})  # Split on space
LINES=(${(f)"$(cat file.txt)"})  # Split on newline

# Trimming
echo "Trim left: ${STRING##*,}"
echo "Trim right: ${STRING%%,*}"

# Padding
echo "Pad left: ${(l:20:)STRING}"
echo "Pad right: ${(r:20:)STRING}"
echo "Pad with char: ${(l:20:-:)STRING}"

# ==============================================================================
# Glob Qualifiers
# ==============================================================================

# Basic qualifiers
echo *(.):       # Regular files only
echo *(/)        # Directories only
echo *(@)        # Symbolic links only
echo *(*)        # Executable files
echo *(.r)       # Readable files
echo *(.w)       # Writable files

# Size qualifiers
echo *(.Lk+100)  # Files larger than 100KB
echo *(.Lm-1)    # Files smaller than 1MB

# Time qualifiers
echo *(.mh-1)    # Modified in last hour
echo *(.mw+1)    # Modified more than a week ago

# Combining qualifiers
echo *(.Lk+10mh-24)  # Large files modified today

# Sorting
echo *(om)       # Sort by modification time
echo *(oL)       # Sort by size
echo *(On)       # Sort by name, reverse

# Limiting
echo *(om[1])    # Most recently modified
echo *(om[1,5])  # 5 most recently modified

# Recursive
echo **/*.txt    # All .txt files recursively
echo ***/*.txt   # Including symlinks

# ==============================================================================
# Extended Globbing
# ==============================================================================

# Negation
echo ^*.txt          # Everything except .txt files
echo *~*.bak         # Exclude .bak files

# Approximate matching
echo (#a1)flie       # Match "file" with 1 error

# Grouping
echo (foo|bar)*.txt  # Files starting with foo or bar

# Numeric ranges
echo file<1-10>.txt  # file1.txt to file10.txt

# Character classes
echo file[[:digit:]].txt
echo file[[:alpha:]].txt

# Patterns
echo (#i)*.TXT       # Case insensitive
echo (#l)*.txt       # Force lowercase
echo (#B)pattern     # Disable backslash escaping

# ==============================================================================
# Control Flow
# ==============================================================================

# If statement
if [[ $NAME == "Alice" ]]; then
    echo "Hello, Alice!"
elif [[ $NAME == "Bob" ]]; then
    echo "Hello, Bob!"
else
    echo "Hello, stranger!"
fi

# Short form
[[ $AGE -ge 18 ]] && echo "Adult" || echo "Minor"

# Case statement
case $NAME in
    Alice|Bob)
        echo "Known user"
        ;|  # Continue testing (zsh-specific)
    A*)
        echo "Starts with A"
        ;;
    *)
        echo "Unknown"
        ;;
esac

# For loops
for fruit in $FRUITS; do
    echo "Fruit: $fruit"
done

# With index
for i in {1..10}; do
    echo "Number: $i"
done

# C-style
for ((i = 1; i <= 10; i++)); do
    echo "Count: $i"
done

# Over associative array
for key val in ${(kv)PERSON}; do
    echo "$key: $val"
done

# While and until
while (( COUNTER < 10 )); do
    ((COUNTER++))
done

until [[ $DONE == "true" ]]; do
    DONE="true"
done

# Repeat
repeat 5 echo "Hello"

# Select
select fruit in $FRUITS "quit"; do
    [[ $fruit == "quit" ]] && break
    echo "You selected: $fruit"
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

# With local variables
calculate() {
    local -i x=$1 y=$2
    local -i result=$((x + y))
    echo $result
}

# Anonymous function
() {
    local temp="I'm in an anonymous function"
    echo $temp
}

# Autoloaded function
autoload -Uz add-zsh-hook

# Function with special locals
demo_special() {
    local -a reply
    local REPLY
    vared -p "Enter value: " REPLY
    echo "You entered: $REPLY"
}

# Function hooks
precmd() {
    # Runs before each prompt
    :
}

preexec() {
    # Runs before each command
    :
}

# ==============================================================================
# Parameter Expansion Flags
# ==============================================================================

VAR="hello world"

# Type conversion
echo ${(t)VAR}           # Type of variable
echo ${(P)VAR}           # Indirect expansion

# String manipulation
echo ${(S)VAR/world/zsh} # Shortest match
echo ${(I:1:)VAR//?/X}   # Replace by index

# Word splitting
echo ${(w)#VAR}          # Word count
echo ${(ws:,:)VAR}       # Split on comma

# Quoting
echo ${(q)VAR}           # Single quote
echo ${(qq)VAR}          # Double quote
echo ${(qqq)VAR}         # Dollar quote
echo ${(qqqq)VAR}        # Backslash quote

# Visibility
echo ${(V)VAR}           # Visible representation

# ==============================================================================
# Zsh Modules
# ==============================================================================

# Load modules
zmodload zsh/datetime
zmodload zsh/stat
zmodload zsh/mathfunc
zmodload zsh/pcre
zmodload zsh/regex

# Using datetime
echo "Epoch: $EPOCHSECONDS"
strftime '%Y-%m-%d %H:%M:%S' $EPOCHSECONDS

# Using mathfunc
(( PI = 4.0 * atan(1.0) ))
(( SQRT2 = sqrt(2) ))
echo "Pi: $PI, Sqrt(2): $SQRT2"

# Using stat
zstat -A statinfo file.txt
echo "Size: $statinfo[size]"

# Using PCRE
pcre_compile -m 'pattern'
pcre_match -b -- string

# ==============================================================================
# Completion System
# ==============================================================================

# Initialize completion
autoload -Uz compinit && compinit

# Custom completion
_my_command() {
    local -a options
    options=(
        '--help[Show help]'
        '--version[Show version]'
        '-v[Verbose output]'
        '-q[Quiet mode]'
    )
    _arguments $options
}
compdef _my_command my_command

# Completion styles
zstyle ':completion:*' menu select
zstyle ':completion:*' matcher-list 'm:{a-z}={A-Z}'
zstyle ':completion:*' list-colors ${(s.:.)LS_COLORS}

# ==============================================================================
# ZLE (Zsh Line Editor)
# ==============================================================================

# Define widget
my-widget() {
    BUFFER="echo Hello, World!"
    CURSOR=$#BUFFER
}
zle -N my-widget

# Bind key
bindkey '^X^H' my-widget
bindkey -v  # Vi mode
bindkey -e  # Emacs mode

# Custom key bindings
bindkey '^[[A' history-search-backward
bindkey '^[[B' history-search-forward

# ==============================================================================
# Process Substitution and Multios
# ==============================================================================

# Process substitution
diff <(sort file1.txt) <(sort file2.txt)
cat >(grep error > errors.txt) >(grep warning > warnings.txt)

# Multios (multiple redirections)
setopt MULTIOS
echo "Hello" > file1.txt > file2.txt  # Write to both
echo "Hello" > file.txt | cat         # Write and pipe

# ==============================================================================
# Coprocess
# ==============================================================================

coproc {
    while read line; do
        print "Processed: $line"
    done
}

print -p "Hello"
read -p reply
echo $reply

# ==============================================================================
# Anonymous Functions and Closures
# ==============================================================================

# Immediate execution
(){ echo "Arguments: $@" } arg1 arg2 arg3

# Closure-like behavior
make_counter() {
    local count=0
    print -r -- '
        ((count++))
        echo $count
    '
}

# ==============================================================================
# Traps and Signals
# ==============================================================================

# Trap signals
TRAPINT() {
    echo "Caught SIGINT"
    return $(( 128 + $1 ))
}

TRAPEXIT() {
    echo "Exiting..."
}

TRAPZERR() {
    echo "Error occurred"
}

# Traditional trap syntax
trap 'echo "Cleanup"; rm -f /tmp/$$.*' EXIT

# ==============================================================================
# Arithmetic
# ==============================================================================

# Integer arithmetic
(( result = 5 + 3 * 2 ))
(( result++ ))
(( result += 10 ))

# Floating point (requires zsh/mathfunc)
(( floatresult = 22.0 / 7.0 ))
(( pi = 4.0 * atan(1.0) ))

# Ternary
(( max = a > b ? a : b ))

# Bitwise
(( bits = 0xFF & 0x0F ))
(( shifted = 1 << 4 ))

# ==============================================================================
# Prompt Customization
# ==============================================================================

# Prompt escapes
PROMPT='%n@%m:%~%# '
RPROMPT='%D{%H:%M:%S}'

# Prompt with colors
PROMPT='%F{green}%n%f@%F{blue}%m%f:%F{yellow}%~%f%# '

# Conditional prompts
PROMPT='%(?.%F{green}✓.%F{red}✗)%f %~ %# '

# Prompt with vcs_info
autoload -Uz vcs_info
precmd() { vcs_info }
PROMPT='${vcs_info_msg_0_}%# '

# ==============================================================================
# History
# ==============================================================================

HISTFILE=~/.zsh_history
HISTSIZE=10000
SAVEHIST=10000

setopt HIST_IGNORE_DUPS
setopt HIST_IGNORE_SPACE
setopt HIST_REDUCE_BLANKS
setopt HIST_VERIFY
setopt SHARE_HISTORY
setopt EXTENDED_HISTORY

# History expansion
echo !!        # Last command
echo !$        # Last argument
echo !^        # First argument
echo !*        # All arguments
echo !-2       # Second to last command
echo !grep     # Last grep command

# ==============================================================================
# Main Script
# ==============================================================================

main() {
    print "=== Zsh Sample Script ==="

    # Array operations
    print "Fruits: ${(j:, :)FRUITS}"
    print "Sorted: ${(o)FRUITS}"

    # Glob qualifiers
    print "Text files: "$(echo *.txt(.N))

    # Function calls
    greet "World"
    print "5 + 3 = $(calculate 5 3)"

    # Floating point
    (( pi = 4.0 * atan(1.0) ))
    print "Pi ≈ $pi"

    print "Done!"
}

# Run main
main "$@"
