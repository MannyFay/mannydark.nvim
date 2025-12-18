-------------------------------------------------------------------------------
-- Bash Files
-- Highlighting for .bash, .bashrc, .bash_profile, .bash_aliases files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local bash    = {}


-------------------------------------------------------------------------------
-- Settings

bash.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - Bash-specific extensions to sh.vim

  -- Bash-specific Statements/Builtins
  highlight(0, 'bashStatement',        { fg = colors.orange,     bg = 'NONE'            })  -- bind, builtin, caller, compgen, complete, compopt, declare, dirs, disown, enable, fg, help, history, let, logout, mapfile, popd, pushd, readarray, shopt, source, suspend, time, typeset
  highlight(0, 'bashAdminStatement',   { fg = colors.orange,     bg = 'NONE'            })  -- reload, restart, start, status, stop (init script commands)

  -- Bash Special Variables
  highlight(0, 'bashSpecialVariables', { fg = colors.pink,       bg = 'NONE'            })  -- BASH, BASH_ALIASES, BASH_ARGC, BASH_ARGV, BASH_COMMAND, BASH_ENV, etc.

  -- Coproc
  highlight(0, 'bashCoproc',           { fg = colors.blue,       bg = 'NONE'            })  -- coproc keyword

  -- Shell Common Groups (sh* prefix - shared with shell.lua but comprehensive here)

  -- Keywords - Control Flow
  highlight(0, 'shConditional',        { fg = colors.blue,       bg = 'NONE'            })  -- if, then, else, elif, fi
  highlight(0, 'shCase',               { fg = colors.blue,       bg = 'NONE'            })  -- case, esac, in
  highlight(0, 'shCaseIn',             { fg = colors.blue,       bg = 'NONE'            })  -- in (case)
  highlight(0, 'shLoop',               { fg = colors.blue,       bg = 'NONE'            })  -- for, while, until, do, done
  highlight(0, 'shRepeat',             { fg = colors.blue,       bg = 'NONE'            })  -- for, while, until, select
  highlight(0, 'shFor',                { fg = colors.blue,       bg = 'NONE'            })  -- for
  highlight(0, 'shDo',                 { fg = colors.blue,       bg = 'NONE'            })  -- do, done

  -- Keywords - Function Definition
  highlight(0, 'shFunctionKey',        { fg = colors.blue,       bg = 'NONE'            })  -- function keyword
  highlight(0, 'shFunction',           { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'shFunctionName',       { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'shFunctionOne',        { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'shFunctionTwo',        { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions (alternate)

  -- Built-in Commands
  highlight(0, 'shStatement',          { fg = colors.orange,     bg = 'NONE'            })  -- echo, read, printf, test, etc.
  highlight(0, 'shEcho',               { fg = colors.orange,     bg = 'NONE'            })  -- echo
  highlight(0, 'shSet',                { fg = colors.orange,     bg = 'NONE'            })  -- set
  highlight(0, 'shSource',             { fg = colors.orange,     bg = 'NONE'            })  -- source, .
  highlight(0, 'shAlias',              { fg = colors.orange,     bg = 'NONE'            })  -- alias, unalias
  highlight(0, 'shTouch',              { fg = colors.orange,     bg = 'NONE'            })  -- touch
  highlight(0, 'shTouchCmd',           { fg = colors.orange,     bg = 'NONE'            })  -- touch command

  -- Variables - Declaration/Assignment
  highlight(0, 'shVariable',           { fg = colors.purple,     bg = 'NONE'            })  -- Variable names in assignments
  highlight(0, 'shVar',                { fg = colors.purple,     bg = 'NONE'            })  -- Variable names
  highlight(0, 'shVarAssign',          { fg = colors.purple,     bg = 'NONE'            })  -- Variable assignments

  -- Variables - Dereferencing
  highlight(0, 'shDeref',              { fg = colors.white,      bg = 'NONE'            })  -- ${} braces
  highlight(0, 'shDerefVar',           { fg = colors.purple,     bg = 'NONE'            })  -- $variable
  highlight(0, 'shDerefSimple',        { fg = colors.purple,     bg = 'NONE'            })  -- $var (simple dereference)
  highlight(0, 'shDerefVarArray',      { fg = colors.purple,     bg = 'NONE'            })  -- ${array[index]}
  highlight(0, 'shDerefSpecial',       { fg = colors.pink,       bg = 'NONE'            })  -- $?, $$, $!, $#, $*, $@, $-, $_
  highlight(0, 'shDerefPattern',       { fg = colors.pink,       bg = 'NONE'            })  -- ${var#pattern}, ${var%pattern}
  highlight(0, 'shDerefString',        { fg = colors.redLight,   bg = 'NONE'            })  -- String in parameter expansion
  highlight(0, 'shDerefLen',           { fg = colors.pink,       bg = 'NONE'            })  -- ${#var}
  highlight(0, 'shDerefOp',            { fg = colors.white,      bg = 'NONE'            })  -- Operators in ${var:-default}
  highlight(0, 'shDerefOffset',        { fg = colors.greenLight, bg = 'NONE'            })  -- ${var:offset:length}
  highlight(0, 'shDerefEscape',        { fg = colors.pink,       bg = 'NONE'            })  -- Escapes in dereferences
  highlight(0, 'shDerefDelim',         { fg = colors.white,      bg = 'NONE'            })  -- Delimiters in dereferences

  -- Variables - Shell Built-in Variables
  highlight(0, 'shShellVariables',     { fg = colors.pink,       bg = 'NONE'            })  -- HOME, PATH, PS1, PS2, etc.
  highlight(0, 'shSpecialVar',         { fg = colors.pink,       bg = 'NONE'            })  -- $0, $1, $2, ... $9
  highlight(0, 'shPosnParm',           { fg = colors.pink,       bg = 'NONE'            })  -- Positional parameters

  -- Strings - Double Quoted
  highlight(0, 'shDoubleQuote',        { fg = colors.redLight,   bg = 'NONE'            })  -- "string"
  highlight(0, 'shSpecialDQ',          { fg = colors.pink,       bg = 'NONE'            })  -- Special chars in double quotes

  -- Strings - Single Quoted
  highlight(0, 'shSingleQuote',        { fg = colors.redLight,   bg = 'NONE'            })  -- 'string'
  highlight(0, 'shQuote',              { fg = colors.redLight,   bg = 'NONE'            })  -- Quotes

  -- Strings - Special Quoting
  highlight(0, 'shExSingleQuote',      { fg = colors.redLight,   bg = 'NONE'            })  -- $'...' ANSI-C quoting
  highlight(0, 'shExDoubleQuote',      { fg = colors.redLight,   bg = 'NONE'            })  -- $"..." locale translation
  highlight(0, 'shString',             { fg = colors.redLight,   bg = 'NONE'            })  -- General strings
  highlight(0, 'shStringSpecial',      { fg = colors.pink,       bg = 'NONE'            })  -- Special in strings

  -- Heredoc
  highlight(0, 'shHereDoc',            { fg = colors.redLight,   bg = 'NONE'            })  -- <<EOF heredoc content
  highlight(0, 'shHereString',         { fg = colors.redLight,   bg = 'NONE'            })  -- <<< here-string
  highlight(0, 'shBeginHere',          { fg = colors.pink,       bg = 'NONE'            })  -- Heredoc start marker
  highlight(0, 'shHerePayload',        { fg = colors.redLight,   bg = 'NONE'            })  -- Heredoc content

  -- Numbers
  highlight(0, 'shNumber',             { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers

  -- Operators
  highlight(0, 'shOperator',           { fg = colors.white,      bg = 'NONE'            })  -- = == != < > etc.
  highlight(0, 'shTestOpr',            { fg = colors.blue,       bg = 'NONE'            })  -- -eq, -ne, -lt, -gt, -le, -ge, -z, -n, -f, -d, -e, etc.
  highlight(0, 'shArithOpr',           { fg = colors.white,      bg = 'NONE'            })  -- Arithmetic operators
  highlight(0, 'shBoolLogic',          { fg = colors.white,      bg = 'NONE'            })  -- && ||

  -- Redirection
  highlight(0, 'shRedir',              { fg = colors.pink,       bg = 'NONE'            })  -- > >> < << &> 2>&1 etc.

  -- Pipes
  highlight(0, 'shPipe',               { fg = colors.white,      bg = 'NONE'            })  -- |

  -- Command Substitution
  highlight(0, 'shCommandSub',         { fg = colors.purple,     bg = 'NONE'            })  -- $(command)
  highlight(0, 'shCommandSubBQ',       { fg = colors.purple,     bg = 'NONE'            })  -- `command` (backticks)
  highlight(0, 'shCmdParenRegion',     { fg = colors.purple,     bg = 'NONE'            })  -- Command substitution region
  highlight(0, 'shCmdSubRegion',       { fg = colors.purple,     bg = 'NONE'            })  -- $(...)

  -- Arithmetic Expansion
  highlight(0, 'shArithmetic',         { fg = colors.greenLight, bg = 'NONE'            })  -- $((expr)) or ((expr))
  highlight(0, 'shArithRegion',        { fg = colors.greenLight, bg = 'NONE'            })  -- Arithmetic region
  highlight(0, 'shArithParen',         { fg = colors.white,      bg = 'NONE'            })  -- (( ))
  highlight(0, 'shDblParen',           { fg = colors.white,      bg = 'NONE'            })  -- (( ))

  -- Test/Conditional Expressions
  highlight(0, 'shTest',               { fg = colors.blue,       bg = 'NONE'            })  -- test, [ ]
  highlight(0, 'shDblBrace',           { fg = colors.blue,       bg = 'NONE'            })  -- [[ ]]
  highlight(0, 'shTestPattern',        { fg = colors.redLight,   bg = 'NONE'            })  -- Pattern in [[ ]]
  highlight(0, 'shTestDoubleQuote',    { fg = colors.redLight,   bg = 'NONE'            })  -- "string" in test
  highlight(0, 'shTestSingleQuote',    { fg = colors.redLight,   bg = 'NONE'            })  -- 'string' in test

  -- Process Substitution
  highlight(0, 'shSubSh',              { fg = colors.purple,     bg = 'NONE'            })  -- <(cmd) or >(cmd)
  highlight(0, 'shSubShRegion',        { fg = colors.purple,     bg = 'NONE'            })  -- Process substitution region

  -- Brace Expansion
  highlight(0, 'shCurlyIn',            { fg = colors.white,      bg = 'NONE'            })  -- {a,b,c}

  -- Glob/Pattern
  highlight(0, 'shPattern',            { fg = colors.redLight,   bg = 'NONE'            })  -- Glob patterns
  highlight(0, 'shCharClass',          { fg = colors.pink,       bg = 'NONE'            })  -- [[:alpha:]], [[:digit:]], etc.

  -- Range
  highlight(0, 'shRange',              { fg = colors.white,      bg = 'NONE'            })  -- [ ]

  -- Array
  highlight(0, 'shExpr',               { fg = colors.blue,       bg = 'NONE'            })  -- Array index @
  highlight(0, 'shAtExpr',             { fg = colors.blue,       bg = 'NONE'            })  -- @ expression

  -- Options/Flags
  highlight(0, 'shOption',             { fg = colors.blue,       bg = 'NONE'            })  -- -option flags
  highlight(0, 'shSetOption',          { fg = colors.blue,       bg = 'NONE'            })  -- set options
  highlight(0, 'shSetList',            { fg = colors.purple,     bg = 'NONE'            })  -- set list

  -- Escape Sequences
  highlight(0, 'shEscape',             { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \r, \\, etc.
  highlight(0, 'shCtrlSeq',            { fg = colors.pink,       bg = 'NONE'            })  -- Control sequences
  highlight(0, 'shSpecial',            { fg = colors.pink,       bg = 'NONE'            })  -- Special characters

  -- Shebang
  highlight(0, 'shShebang',            { fg = colors.pink,       bg = 'NONE'            })  -- #!/bin/bash

  -- Case Patterns
  highlight(0, 'shCaseLabel',          { fg = colors.redLight,   bg = 'NONE'            })  -- Case pattern labels
  highlight(0, 'shCaseBar',            { fg = colors.white,      bg = 'NONE'            })  -- | in case patterns
  highlight(0, 'shCaseRange',          { fg = colors.redLight,   bg = 'NONE'            })  -- Range in case
  highlight(0, 'shSnglCase',           { fg = colors.redLight,   bg = 'NONE'            })  -- Single case pattern
  highlight(0, 'shCaseStart',          { fg = colors.blue,       bg = 'NONE'            })  -- Case start

  -- Punctuation
  highlight(0, 'shSemicolon',          { fg = colors.white,      bg = 'NONE'            })  -- ;
  highlight(0, 'shComma',              { fg = colors.white,      bg = 'NONE'            })  -- ,
  highlight(0, 'shColon',              { fg = colors.white,      bg = 'NONE'            })  -- :
  highlight(0, 'shParen',              { fg = colors.white,      bg = 'NONE'            })  -- ( )
  highlight(0, 'shBrackets',           { fg = colors.white,      bg = 'NONE'            })  -- [ ]

  -- Line Continuation
  highlight(0, 'shWrapLineOperator',   { fg = colors.pink,       bg = 'NONE'            })  -- \ at end of line

  -- Comments
  highlight(0, 'shComment',            { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'shQuickComment',       { fg = colors.red,        bg = 'NONE'            })  -- Quick comments
  highlight(0, 'shTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'shError',              { fg = colors.red,        bg = 'NONE'            })  -- Errors
  highlight(0, 'shCondError',          { fg = colors.red,        bg = 'NONE'            })  -- Condition errors
  highlight(0, 'shDoError',            { fg = colors.red,        bg = 'NONE'            })  -- do/done errors
  highlight(0, 'shIfError',            { fg = colors.red,        bg = 'NONE'            })  -- if/fi errors
  highlight(0, 'shInError',            { fg = colors.red,        bg = 'NONE'            })  -- in errors
  highlight(0, 'shCaseError',          { fg = colors.red,        bg = 'NONE'            })  -- case errors
  highlight(0, 'shEsacError',          { fg = colors.red,        bg = 'NONE'            })  -- esac errors
  highlight(0, 'shCurlyError',         { fg = colors.red,        bg = 'NONE'            })  -- Brace errors
  highlight(0, 'shParenError',         { fg = colors.red,        bg = 'NONE'            })  -- Parenthesis errors
  highlight(0, 'shTestError',          { fg = colors.red,        bg = 'NONE'            })  -- Test errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.bash)

  -- Variables
  highlight(0, '@variable.bash',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.bash',      { fg = colors.pink,      bg = 'NONE' })  -- BASH, BASH_SOURCE, FUNCNAME, LINENO, etc.
  highlight(0, '@variable.parameter.bash',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters

  -- Constants
  highlight(0, '@constant.bash',              { fg = colors.purple,    bg = 'NONE' })  -- UPPERCASE variables (convention)
  highlight(0, '@constant.builtin.bash',      { fg = colors.pink,      bg = 'NONE' })  -- Signal names (SIGHUP, SIGTERM, etc.)

  -- Functions
  highlight(0, '@function.bash',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.bash',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.bash',      { fg = colors.orange,    bg = 'NONE' })  -- echo, read, printf, test, trap, etc.

  -- Keywords
  highlight(0, '@keyword.bash',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.bash',      { fg = colors.blue,      bg = 'NONE' })  -- function
  highlight(0, '@keyword.operator.bash',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@keyword.return.bash',        { fg = colors.blue,      bg = 'NONE' })  -- return, exit
  highlight(0, '@keyword.repeat.bash',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, until, do, done, select
  highlight(0, '@keyword.conditional.bash',   { fg = colors.blue,      bg = 'NONE' })  -- if, then, else, elif, fi, case, esac, in
  highlight(0, '@keyword.conditional.ternary.bash', { fg = colors.white, bg = 'NONE' })  -- ? : in arithmetic
  highlight(0, '@keyword.import.bash',        { fg = colors.pink,      bg = 'NONE' })  -- export, source, .
  highlight(0, '@keyword.directive.bash',     { fg = colors.pink,      bg = 'NONE' })  -- Shebang

  -- Labels
  highlight(0, '@label.bash',                 { fg = colors.pink,      bg = 'NONE' })  -- Heredoc markers

  -- Strings
  highlight(0, '@string.bash',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.regexp.bash',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex patterns, globs
  highlight(0, '@string.escape.bash',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.bash',        { fg = colors.pink,      bg = 'NONE' })  -- Special strings
  highlight(0, '@character.bash',             { fg = colors.redLight,  bg = 'NONE' })  -- Characters
  highlight(0, '@character.special.bash',     { fg = colors.pink,      bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number.bash',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- Booleans
  highlight(0, '@boolean.bash',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.bash',               { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.bash',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.bash',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, [[]], (())
  highlight(0, '@punctuation.delimiter.bash', { fg = colors.white,     bg = 'NONE' })  -- ; ;; ;& ;;& &
  highlight(0, '@punctuation.special.bash',   { fg = colors.pink,      bg = 'NONE' })  -- $ in expansions, # in comments


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.bash / @lsp.type.xxx.shellscript)

  -- Note: bash-language-server uses 'shellscript' as the language identifier
  highlight(0, '@lsp.type.variable.bash',         { fg = colors.purple,    bg = 'NONE' })
  highlight(0, '@lsp.type.variable.shellscript',  { fg = colors.purple,    bg = 'NONE' })
  highlight(0, '@lsp.type.parameter.bash',        { fg = colors.purple,    bg = 'NONE' })
  highlight(0, '@lsp.type.parameter.shellscript', { fg = colors.purple,    bg = 'NONE' })
  highlight(0, '@lsp.type.function.bash',         { fg = colors.orange,    bg = 'NONE' })
  highlight(0, '@lsp.type.function.shellscript',  { fg = colors.orange,    bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.bash',          { fg = colors.blue,      bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.shellscript',   { fg = colors.blue,      bg = 'NONE' })
  highlight(0, '@lsp.type.operator.bash',         { fg = colors.white,     bg = 'NONE' })
  highlight(0, '@lsp.type.operator.shellscript',  { fg = colors.white,     bg = 'NONE' })
  highlight(0, '@lsp.type.string.bash',           { fg = colors.redLight,  bg = 'NONE' })
  highlight(0, '@lsp.type.string.shellscript',    { fg = colors.redLight,  bg = 'NONE' })
  highlight(0, '@lsp.type.number.bash',           { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.number.shellscript',    { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.comment.bash',          { fg = colors.red,       bg = 'NONE' })
  highlight(0, '@lsp.type.comment.shellscript',   { fg = colors.red,       bg = 'NONE' })

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.bash',        { fg = colors.pink,   bg = 'NONE' })
  highlight(0, '@lsp.typemod.variable.readonly.shellscript', { fg = colors.pink,   bg = 'NONE' })
  highlight(0, '@lsp.typemod.function.declaration.bash',     { fg = colors.orange, bg = 'NONE' })
  highlight(0, '@lsp.typemod.function.declaration.shellscript', { fg = colors.orange, bg = 'NONE' })
  highlight(0, '@lsp.typemod.function.defaultLibrary.bash',  { fg = colors.orange, bg = 'NONE' })
  highlight(0, '@lsp.typemod.function.defaultLibrary.shellscript', { fg = colors.orange, bg = 'NONE' })
end

return bash

