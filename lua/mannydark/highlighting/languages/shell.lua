-------------------------------------------------------------------------------
-- Shell (POSIX sh)
-- Base highlighting for POSIX shell scripts (.sh files).
-- These sh* groups are shared with bash, ksh, dash, and other sh-compatible shells.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local shell     = {}


-------------------------------------------------------------------------------
-- Settings

shell.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - from Vim's sh.vim

  -- Keywords - Control Flow
  highlight(0, 'shConditional',       { link = "Conditional" })  -- if, then, else, elif, fi
  highlight(0, 'shCase',              { fg = colors.blue,       bg = 'NONE'                 })  -- case, esac
  highlight(0, 'shCaseIn',            { fg = colors.blue,       bg = 'NONE'                 })  -- in
  highlight(0, 'shCaseEsac',          { fg = colors.blue,       bg = 'NONE'                 })  -- case/esac region
  highlight(0, 'shLoop',              { fg = colors.blue,       bg = 'NONE'                 })  -- for, while, until, do, done
  highlight(0, 'shRepeat',            { fg = colors.blue,       bg = 'NONE'                 })  -- for, while, until, select
  highlight(0, 'shFor',               { fg = colors.blue,       bg = 'NONE'                 })  -- for
  highlight(0, 'shDo',                { fg = colors.blue,       bg = 'NONE'                 })  -- do, done
  highlight(0, 'shIf',                { fg = colors.blue,       bg = 'NONE'                 })  -- if region

  -- Keywords - Function Definition
  highlight(0, 'shFunctionKey',       { link = "Function" })  -- function keyword
  highlight(0, 'shFunction',          { link = "Function" })  -- Function names
  highlight(0, 'shFunctionName',      { link = "Function" })  -- Function names
  highlight(0, 'shFunctionOne',       { link = "Function" })  -- Function definitions (style 1)
  highlight(0, 'shFunctionTwo',       { link = "Function" })  -- Function definitions (style 2)
  highlight(0, 'shFunctionThree',     { link = "Function" })  -- Function definitions (style 3)
  highlight(0, 'shFunctionFour',      { link = "Function" })  -- Function definitions (style 4)
  highlight(0, 'shFunctionStart',     { link = "Function" })  -- Function start

  -- Built-in Commands/Statements
  highlight(0, 'shStatement',         { fg = colors.orange,     bg = 'NONE'                 })  -- echo, read, printf, test, etc.
  highlight(0, 'shEcho',              { fg = colors.orange,     bg = 'NONE'                 })  -- echo
  highlight(0, 'shEmbeddedEcho',      { fg = colors.orange,     bg = 'NONE'                 })  -- Embedded echo
  highlight(0, 'shSet',               { fg = colors.orange,     bg = 'NONE'                 })  -- set
  highlight(0, 'shSource',            { fg = colors.orange,     bg = 'NONE'                 })  -- source, .
  highlight(0, 'shAlias',             { fg = colors.orange,     bg = 'NONE'                 })  -- alias, unalias
  highlight(0, 'shTouch',             { fg = colors.orange,     bg = 'NONE'                 })  -- touch
  highlight(0, 'shTouchCmd',          { fg = colors.orange,     bg = 'NONE'                 })  -- touch command

  -- Variables - Declaration/Assignment
  highlight(0, 'shVariable',          { link = "Variable" })  -- Variable names in assignments
  highlight(0, 'shVar',               { link = "Variable" })  -- Variable names
  highlight(0, 'shVarAssign',         { link = "Variable" })  -- Variable assignments

  -- Variables - Dereferencing
  highlight(0, 'shDeref',             { link = "Variable" })  -- ${} braces
  highlight(0, 'shDerefVar',          { link = "Variable" })  -- $variable
  highlight(0, 'shDerefSimple',       { link = "Variable" })  -- $var (simple dereference)
  highlight(0, 'shDerefVarArray',     { link = "Variable" })  -- ${array[index]}
  highlight(0, 'shDerefSpecial',      { link = "Variable" })  -- $?, $$, $!, $#, $*, $@, $-, $_
  highlight(0, 'shDerefPattern',      { link = "Variable" })  -- ${var#pattern}, ${var%pattern}
  highlight(0, 'shDerefString',       { link = "String" })  -- String in parameter expansion
  highlight(0, 'shDerefLen',          { link = "Variable" })  -- ${#var}
  highlight(0, 'shDerefOp',           { link = "Variable" })  -- Operators in ${var:-default}
  highlight(0, 'shDerefOffset',       { link = "Variable" })  -- ${var:offset:length}
  highlight(0, 'shDerefEscape',       { link = "Variable" })  -- Escapes in dereferences
  highlight(0, 'shDerefDelim',        { link = "Delimiter" })  -- Delimiters in dereferences
  highlight(0, 'shDerefPSR',          { link = "Variable" })  -- ${var/pattern/string}
  highlight(0, 'shDerefPPS',          { link = "Variable" })  -- ${var%%pattern}
  highlight(0, 'shDerefPOL',          { link = "Variable" })  -- ${var:?error}
  highlight(0, 'shDerefPPSleft',      { link = "Variable" })  -- Pattern removal left
  highlight(0, 'shDerefPPSright',     { link = "Variable" })  -- Pattern removal right
  highlight(0, 'shDerefPSRleft',      { link = "Variable" })  -- Pattern substitution left
  highlight(0, 'shDerefPSRright',     { link = "Variable" })  -- Pattern substitution right

  -- Variables - Shell Built-in Variables
  highlight(0, 'shShellVariables',    { link = "Variable" })  -- HOME, PATH, PS1, PS2, etc.
  highlight(0, 'shSpecialVar',        { link = "Variable" })  -- $0, $1, $2, ... $9
  highlight(0, 'shPosnParm',          { link = "Variable" })  -- Positional parameters

  -- Strings - Double Quoted
  highlight(0, 'shDoubleQuote',       { fg = colors.redLight,   bg = 'NONE'                 })  -- "string"
  highlight(0, 'shSpecialDQ',         { fg = colors.pink,       bg = 'NONE'                 })  -- Special chars in double quotes

  -- Strings - Single Quoted
  highlight(0, 'shSingleQuote',       { fg = colors.redLight,   bg = 'NONE'                 })  -- 'string'
  highlight(0, 'shQuote',             { fg = colors.redLight,   bg = 'NONE'                 })  -- Quotes

  -- Strings - Special Quoting
  highlight(0, 'shExSingleQuote',     { fg = colors.redLight,   bg = 'NONE'                 })  -- $'...' ANSI-C quoting
  highlight(0, 'shExDoubleQuote',     { fg = colors.redLight,   bg = 'NONE'                 })  -- $"..." locale translation
  highlight(0, 'shString',            { link = "String" })  -- General strings
  highlight(0, 'shStringSpecial',     { link = "String" })  -- Special in strings
  highlight(0, 'shNoQuote',           { fg = colors.redLight,   bg = 'NONE'                 })  -- Unquoted strings
  highlight(0, 'shAstQuote',          { fg = colors.redLight,   bg = 'NONE'                 })  -- * in quotes

  -- Heredoc
  highlight(0, 'shHereDoc',           { fg = colors.redLight,   bg = 'NONE'                 })  -- <<EOF heredoc content
  highlight(0, 'shHereString',        { link = "String" })  -- <<< here-string
  highlight(0, 'shBeginHere',         { fg = colors.pink,       bg = 'NONE'                 })  -- Heredoc start marker
  highlight(0, 'shHerePayload',       { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc content
  highlight(0, 'shHereDoc01',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 01
  highlight(0, 'shHereDoc02',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 02
  highlight(0, 'shHereDoc03',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 03
  highlight(0, 'shHereDoc04',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 04
  highlight(0, 'shHereDoc05',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 05
  highlight(0, 'shHereDoc06',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 06
  highlight(0, 'shHereDoc07',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 07
  highlight(0, 'shHereDoc08',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 08
  highlight(0, 'shHereDoc09',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 09
  highlight(0, 'shHereDoc10',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 10
  highlight(0, 'shHereDoc11',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 11
  highlight(0, 'shHereDoc12',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 12
  highlight(0, 'shHereDoc13',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 13
  highlight(0, 'shHereDoc14',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 14
  highlight(0, 'shHereDoc15',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 15
  highlight(0, 'shHereDoc16',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Heredoc variant 16

  -- Numbers
  highlight(0, 'shNumber',            { link = "Number" })  -- Numbers

  -- Operators
  highlight(0, 'shOperator',          { link = "Operator" })  -- = == != < > etc.
  highlight(0, 'shTestOpr',           { fg = colors.blue,       bg = 'NONE'                 })  -- -eq, -ne, -lt, -gt, -le, -ge, -z, -n, -f, -d, etc.
  highlight(0, 'shArithOpr',          { fg = colors.white,      bg = 'NONE'                 })  -- Arithmetic operators

  -- Redirection
  highlight(0, 'shRedir',             { fg = colors.pink,       bg = 'NONE'                 })  -- > >> < << &> 2>&1 etc.

  -- Command Substitution
  highlight(0, 'shCommandSub',        { fg = colors.purple,     bg = 'NONE'                 })  -- $(command)
  highlight(0, 'shCommandSubBQ',      { fg = colors.purple,     bg = 'NONE'                 })  -- `command` (backticks)
  highlight(0, 'shCmdParenRegion',    { fg = colors.purple,     bg = 'NONE'                 })  -- Command substitution region
  highlight(0, 'shCmdSubRegion',      { fg = colors.purple,     bg = 'NONE'                 })  -- $(...)

  -- Arithmetic Expansion
  highlight(0, 'shArithmetic',        { fg = colors.greenLight, bg = 'NONE'                 })  -- $((expr)) or ((expr))
  highlight(0, 'shArithRegion',       { fg = colors.greenLight, bg = 'NONE'                 })  -- Arithmetic region
  highlight(0, 'shArithParen',        { fg = colors.white,      bg = 'NONE'                 })  -- (( ))
  highlight(0, 'shDblParen',          { fg = colors.white,      bg = 'NONE'                 })  -- (( ))

  -- Test/Conditional Expressions
  highlight(0, 'shTest',              { fg = colors.blue,       bg = 'NONE'                 })  -- test, [ ]
  highlight(0, 'shDblBrace',          { fg = colors.blue,       bg = 'NONE'                 })  -- [[ ]]
  highlight(0, 'shTestPattern',       { fg = colors.redLight,   bg = 'NONE'                 })  -- Pattern in [[ ]]
  highlight(0, 'shTestDoubleQuote',   { fg = colors.redLight,   bg = 'NONE'                 })  -- "string" in test
  highlight(0, 'shTestSingleQuote',   { fg = colors.redLight,   bg = 'NONE'                 })  -- 'string' in test

  -- Subshell/Process Substitution
  highlight(0, 'shSubSh',             { fg = colors.purple,     bg = 'NONE'                 })  -- (...) subshell
  highlight(0, 'shSubShRegion',       { fg = colors.purple,     bg = 'NONE'                 })  -- Subshell region

  -- Brace Expansion
  highlight(0, 'shCurlyIn',           { fg = colors.white,      bg = 'NONE'                 })  -- {a,b,c}

  -- Glob/Pattern
  highlight(0, 'shPattern',           { fg = colors.redLight,   bg = 'NONE'                 })  -- Glob patterns
  highlight(0, 'shCharClass',         { fg = colors.pink,       bg = 'NONE'                 })  -- [[:alpha:]], [[:digit:]], etc.

  -- Range
  highlight(0, 'shRange',             { fg = colors.white,      bg = 'NONE'                 })  -- [ ]

  -- Array
  highlight(0, 'shExpr',              { fg = colors.blue,       bg = 'NONE'                 })  -- Array index @
  highlight(0, 'shAtExpr',            { fg = colors.blue,       bg = 'NONE'                 })  -- @ expression
  highlight(0, 'shExprRegion',        { fg = colors.purple,     bg = 'NONE'                 })  -- Expression region

  -- Options/Flags
  highlight(0, 'shOption',            { fg = colors.blue,       bg = 'NONE'                 })  -- -option flags
  highlight(0, 'shSetOption',         { fg = colors.blue,       bg = 'NONE'                 })  -- set options
  highlight(0, 'shSetList',           { fg = colors.purple,     bg = 'NONE'                 })  -- set list
  highlight(0, 'shSetListDelim',      { link = "Delimiter" })  -- set list delimiters

  -- Escape Sequences
  highlight(0, 'shEscape',            { fg = colors.pink,       bg = 'NONE'                 })  -- \n, \t, \r, \\, etc.
  highlight(0, 'shCtrlSeq',           { fg = colors.pink,       bg = 'NONE'                 })  -- Control sequences
  highlight(0, 'shSpecial',           { fg = colors.pink,       bg = 'NONE'                 })  -- Special characters
  highlight(0, 'shSpecialNxt',        { fg = colors.pink,       bg = 'NONE'                 })  -- Next special
  highlight(0, 'shSpecialNoZS',       { fg = colors.pink,       bg = 'NONE'                 })  -- Special no ZS
  highlight(0, 'shSpecialSQ',         { fg = colors.pink,       bg = 'NONE'                 })  -- Special in single quote
  highlight(0, 'shSpecialStart',      { fg = colors.pink,       bg = 'NONE'                 })  -- Special start
  highlight(0, 'shBkslshSnglQuote',   { fg = colors.pink,       bg = 'NONE'                 })  -- Backslash single quote
  highlight(0, 'shBkslshDblQuote',    { fg = colors.pink,       bg = 'NONE'                 })  -- Backslash double quote

  -- Shebang
  highlight(0, 'shShebang',           { fg = colors.green,      bg = 'NONE'                 })  -- #!/bin/sh

  -- Case Patterns
  highlight(0, 'shCaseLabel',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Case pattern labels
  highlight(0, 'shCaseBar',           { fg = colors.white,      bg = 'NONE'                 })  -- | in case patterns
  highlight(0, 'shCaseRange',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Range in case
  highlight(0, 'shSnglCase',          { fg = colors.redLight,   bg = 'NONE'                 })  -- Single case pattern
  highlight(0, 'shCaseStart',         { fg = colors.blue,       bg = 'NONE'                 })  -- Case start
  highlight(0, 'shCaseCommandSub',    { fg = colors.purple,     bg = 'NONE'                 })  -- Command substitution in case
  highlight(0, 'shCaseExSingleQuote', { fg = colors.redLight,   bg = 'NONE'                 })  -- $'...' in case
  highlight(0, 'shCaseSingleQuote',   { fg = colors.redLight,   bg = 'NONE'                 })  -- Single quote in case
  highlight(0, 'shCaseDoubleQuote',   { fg = colors.redLight,   bg = 'NONE'                 })  -- Double quote in case

  -- Punctuation
  highlight(0, 'shSemicolon',         { fg = colors.white,      bg = 'NONE'                 })  -- ;
  highlight(0, 'shComma',             { fg = colors.white,      bg = 'NONE'                 })  -- ,
  highlight(0, 'shColon',             { fg = colors.white,      bg = 'NONE'                 })  -- :
  highlight(0, 'shParen',             { fg = colors.white,      bg = 'NONE'                 })  -- ( )

  -- Line Continuation
  highlight(0, 'shWrapLineOperator',  { link = "Operator" })  -- \ at end of line

  -- Echo
  highlight(0, 'shEchoQuote',         { fg = colors.redLight,   bg = 'NONE'                 })  -- Quoted in echo
  highlight(0, 'shEchoDelim',         { link = "Delimiter" })  -- Delimiter in echo

  -- For Loop
  highlight(0, 'shForPP',             { fg = colors.blue,       bg = 'NONE'                 })  -- For loop ++

  -- Comments
  highlight(0, 'shComment',           { link = "Comment" })  -- # comments
  highlight(0, 'shQuickComment',      { link = "Comment" })  -- Quick comments
  highlight(0, 'shBQComment',         { link = "Comment" })  -- Backtick comments
  highlight(0, 'shTodo',              { fg = colors.red,        bg = 'NONE', bold = true    })  -- TODO, FIXME, XXX

  -- OK/Verification
  highlight(0, 'shOK',                { fg = colors.green,      bg = 'NONE'                 })  -- OK markers

  -- Errors
  highlight(0, 'shError',             { fg = colors.red,        bg = 'NONE'                 })  -- Errors
  highlight(0, 'shCondError',         { fg = colors.red,        bg = 'NONE'                 })  -- Condition errors
  highlight(0, 'shDoError',           { fg = colors.red,        bg = 'NONE'                 })  -- do/done errors
  highlight(0, 'shIfError',           { fg = colors.red,        bg = 'NONE'                 })  -- if/fi errors
  highlight(0, 'shInError',           { fg = colors.red,        bg = 'NONE'                 })  -- in errors
  highlight(0, 'shCaseError',         { fg = colors.red,        bg = 'NONE'                 })  -- case errors
  highlight(0, 'shEsacError',         { fg = colors.red,        bg = 'NONE'                 })  -- esac errors
  highlight(0, 'shCurlyError',        { fg = colors.red,        bg = 'NONE'                 })  -- Brace errors
  highlight(0, 'shParenError',        { fg = colors.red,        bg = 'NONE'                 })  -- Parenthesis errors
  highlight(0, 'shTestError',         { fg = colors.red,        bg = 'NONE'                 })  -- Test errors
  highlight(0, 'shDerefWordError',    { link = "Variable" })  -- Dereference word error
  highlight(0, 'shDerefOpError',      { link = "Variable" })  -- Dereference operator error

  -- Sync groups (for syntax synchronization)
  highlight(0, 'shCaseEsacSync',      { fg = colors.blue,       bg = 'NONE'                 })
  highlight(0, 'shDoSync',            { fg = colors.blue,       bg = 'NONE'                 })
  highlight(0, 'shForSync',           { fg = colors.blue,       bg = 'NONE'                 })
  highlight(0, 'shIfSync',            { fg = colors.blue,       bg = 'NONE'                 })
  highlight(0, 'shUntilSync',         { fg = colors.blue,       bg = 'NONE'                 })
  highlight(0, 'shWhileSync',         { fg = colors.blue,       bg = 'NONE'                 })


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.sh)
  -- Note: Most Treesitter parsers use 'bash' as the language, but some may use 'sh'

  -- Variables
  highlight(0, '@variable.sh',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.sh',      { link = "Variable" })  -- Special variables ($?, $$, etc.)
  highlight(0, '@variable.parameter.sh',    { link = "Variable" })  -- Function parameters

  -- Constants
  highlight(0, '@constant.sh',              { link = "Constant" })  -- UPPERCASE variables
  highlight(0, '@constant.builtin.sh',      { link = "Constant" })  -- Built-in constants

  -- Functions
  highlight(0, '@function.sh',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.sh',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.sh',      { link = "Function" })  -- Built-in commands

  -- Keywords
  highlight(0, '@keyword.sh',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.sh',      { link = "Keyword" })  -- function
  highlight(0, '@keyword.operator.sh',      { link = "Operator" })  -- Operators
  highlight(0, '@keyword.return.sh',        { link = "Keyword" })  -- return, exit
  highlight(0, '@keyword.repeat.sh',        { link = "Keyword" })  -- for, while, until
  highlight(0, '@keyword.conditional.sh',   { link = "Conditional" })  -- if, then, else, elif, fi, case, esac
  highlight(0, '@keyword.import.sh',        { link = "Keyword" })  -- source, .
  highlight(0, '@keyword.directive.sh',     { link = "Keyword" })  -- Shebang

  -- Labels
  highlight(0, '@label.sh',                 { fg = colors.pink,      bg = 'NONE' })  -- Heredoc markers

  -- Strings
  highlight(0, '@string.sh',                { link = "String" })  -- Strings
  highlight(0, '@string.regexp.sh',         { link = "String" })  -- Regex patterns
  highlight(0, '@string.escape.sh',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.sh',        { link = "String" })  -- Special strings
  highlight(0, '@character.sh',             { fg = colors.redLight,  bg = 'NONE' })  -- Characters
  highlight(0, '@character.special.sh',     { fg = colors.pink,      bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number.sh',                { link = "Number" })  -- Numbers

  -- Booleans
  highlight(0, '@boolean.sh',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.sh',               { link = "Comment" })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.sh',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.sh',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.sh', { link = "Delimiter" })  -- ; ;;
  highlight(0, '@punctuation.special.sh',   { fg = colors.pink,      bg = 'NONE' })  -- $ in expansions


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.sh)

  highlight(0, '@lsp.type.variable.sh',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.sh',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.function.sh',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.keyword.sh',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.sh',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.sh',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.sh',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.sh',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.sh',    { link = "Variable" })  -- Built-in variables
  highlight(0, '@lsp.typemod.function.declaration.sh', { fg = colors.orange, bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.sh', { fg = colors.orange, bg = 'NONE' })  -- Built-in commands
end

return shell
