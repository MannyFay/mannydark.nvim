-------------------------------------------------------------------------------
-- AWK Files
-- Highlighting for .awk, .gawk files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local awk     = {}


-------------------------------------------------------------------------------
-- Settings

awk.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'awkConditional',       { link = "Conditional" })  -- if, else
  highlight(0, 'awkRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- while, for, do

  -- Keywords - Statements
  highlight(0, 'awkStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- break, continue, delete, exit, next, nextfile, return, switch, case, default

  -- Special Patterns
  highlight(0, 'awkPatterns',          { fg = colors.pink,       bg = 'NONE'            })  -- BEGIN, END, BEGINFILE, ENDFILE

  -- Function Definition
  highlight(0, 'awkFuncDef',           { link = "Function" })  -- function, func
  highlight(0, 'awkFuncName',          { link = "Function" })  -- User-defined function names

  -- Built-in Functions - Numeric
  highlight(0, 'awkFunction',          { link = "Function" })  -- All built-in functions
  highlight(0, 'awkNumericFunc',       { link = "Function" })  -- atan2, cos, exp, int, log, rand, sin, sqrt, srand

  -- Built-in Functions - String
  highlight(0, 'awkStringFunc',        { link = "String" })  -- gsub, index, length, match, split, sprintf, sub, substr, tolower, toupper, gensub, patsplit, strtonum, asort, asorti

  -- Built-in Functions - I/O
  highlight(0, 'awkIOFunc',            { link = "Function" })  -- close, fflush, getline, print, printf, system

  -- Built-in Functions - Time (GAWK)
  highlight(0, 'awkTimeFunc',          { link = "Function" })  -- mktime, strftime, systime

  -- Built-in Functions - Bitwise (GAWK)
  highlight(0, 'awkBitFunc',           { link = "Function" })  -- and, compl, lshift, or, rshift, xor

  -- Built-in Functions - Type (GAWK)
  highlight(0, 'awkTypeFunc',          { link = "Type" })  -- isarray, typeof

  -- Built-in Functions - I18N (GAWK)
  highlight(0, 'awkI18nFunc',          { link = "Function" })  -- bindtextdomain, dcgettext, dcngettext

  -- Variables - Built-in (Modifiable)
  highlight(0, 'awkVariables',         { link = "Variable" })  -- All built-in variables
  highlight(0, 'awkFieldSep',          { fg = colors.pink,       bg = 'NONE'            })  -- FS, RS, OFS, ORS
  highlight(0, 'awkOutputFormat',      { fg = colors.pink,       bg = 'NONE'            })  -- OFMT, CONVFMT
  highlight(0, 'awkMiscVar',           { link = "Variable" })  -- SUBSEP, BINMODE, FIELDWIDTHS, FPAT, IGNORECASE, LINT, PREC, ROUNDMODE, TEXTDOMAIN

  -- Variables - Built-in (Auto-set/Read-only)
  highlight(0, 'awkAutoVar',           { link = "Variable" })  -- NR, NF, FNR, FILENAME, ARGC, ARGV, ARGIND, ENVIRON, ERRNO, RLENGTH, RSTART, RT, FUNCTAB, PROCINFO, SYMTAB

  -- Variables - Field References
  highlight(0, 'awkFieldVars',         { link = "Variable" })  -- $0, $1, $2, ... $NF

  -- Variables - User-defined
  highlight(0, 'awkIdentifier',        { fg = colors.purple,     bg = 'NONE'            })  -- User variables
  highlight(0, 'awkVar',               { link = "Variable" })  -- Variable names
  highlight(0, 'awkArrayVar',          { link = "Variable" })  -- Array variables

  -- Strings
  highlight(0, 'awkString',            { link = "String" })  -- "strings"

  -- String Escapes
  highlight(0, 'awkSpecialCharacter',  { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \r, \\, \", etc.
  highlight(0, 'awkSpecialPrintf',     { fg = colors.pink,       bg = 'NONE'            })  -- %s, %d, %f, etc. in printf

  -- Regular Expressions
  highlight(0, 'awkSearch',            { fg = colors.redLight,   bg = 'NONE'            })  -- /regex/
  highlight(0, 'awkRegExp',            { fg = colors.redLight,   bg = 'NONE'            })  -- Regular expression patterns
  highlight(0, 'awkRegExpCharClass',   { fg = colors.pink,       bg = 'NONE'            })  -- Character classes in regex
  highlight(0, 'awkRegExpSpecial',     { fg = colors.pink,       bg = 'NONE'            })  -- Special regex characters

  -- Numbers
  highlight(0, 'awkNumber',            { link = "Number" })  -- Integers
  highlight(0, 'awkFloat',             { fg = colors.greenLight, bg = 'NONE'            })  -- Floating-point numbers
  highlight(0, 'awkHex',               { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex numbers
  highlight(0, 'awkOctal',             { fg = colors.greenLight, bg = 'NONE'            })  -- Octal numbers

  -- Operators - Arithmetic
  highlight(0, 'awkOperator',          { link = "Operator" })  -- All operators
  highlight(0, 'awkArithOp',           { fg = colors.white,      bg = 'NONE'            })  -- + - * / % ^

  -- Operators - Assignment
  highlight(0, 'awkAssignOp',          { fg = colors.white,      bg = 'NONE'            })  -- = += -= *= /= %= ^=

  -- Operators - Comparison/Relational
  highlight(0, 'awkExpression',        { fg = colors.white,      bg = 'NONE'            })  -- Expressions
  highlight(0, 'awkCompareOp',         { fg = colors.white,      bg = 'NONE'            })  -- == != < <= > >=

  -- Operators - Logical
  highlight(0, 'awkBoolLogic',         { fg = colors.white,      bg = 'NONE'            })  -- && || !

  -- Operators - Pattern Match
  highlight(0, 'awkMatchOp',           { fg = colors.white,      bg = 'NONE'            })  -- ~ !~

  -- Operators - Ternary
  highlight(0, 'awkTernary',           { fg = colors.white,      bg = 'NONE'            })  -- ? :

  -- Operators - Unary
  highlight(0, 'awkUnaryOp',           { fg = colors.white,      bg = 'NONE'            })  -- ++ --

  -- Operators - In
  highlight(0, 'awkInOp',              { fg = colors.blue,       bg = 'NONE'            })  -- in (for array membership)

  -- I/O Operators
  highlight(0, 'awkFileIO',            { fg = colors.pink,       bg = 'NONE'            })  -- | |& < > >> (pipes and redirection)
  highlight(0, 'awkPipe',              { fg = colors.pink,       bg = 'NONE'            })  -- | (pipe)
  highlight(0, 'awkRedirect',          { fg = colors.pink,       bg = 'NONE'            })  -- < > >> (redirection)

  -- GAWK Directives
  highlight(0, 'awkInclude',           { fg = colors.pink,       bg = 'NONE'            })  -- @include
  highlight(0, 'awkLoad',              { fg = colors.pink,       bg = 'NONE'            })  -- @load
  highlight(0, 'awkNamespace',         { fg = colors.pink,       bg = 'NONE'            })  -- @namespace
  highlight(0, 'awkDirective',         { fg = colors.pink,       bg = 'NONE'            })  -- GAWK directives

  -- Braces/Brackets
  highlight(0, 'awkBraces',            { fg = colors.white,      bg = 'NONE'            })  -- { }
  highlight(0, 'awkBrackets',          { fg = colors.white,      bg = 'NONE'            })  -- [ ]
  highlight(0, 'awkParens',            { fg = colors.white,      bg = 'NONE'            })  -- ( )

  -- Special Characters
  highlight(0, 'awkFieldRef',          { fg = colors.pink,       bg = 'NONE'            })  -- $ (field reference operator)
  highlight(0, 'awkSemicolon',         { fg = colors.white,      bg = 'NONE'            })  -- ;
  highlight(0, 'awkComma',             { fg = colors.white,      bg = 'NONE'            })  -- ,

  -- Line Continuation
  highlight(0, 'awkLineSkip',          { fg = colors.pink,       bg = 'NONE'            })  -- \ at end of line

  -- Comments
  highlight(0, 'awkComment',           { link = "Comment" })  -- # comments
  highlight(0, 'awkTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'awkError',             { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.awk)

  -- Variables
  highlight(0, '@variable.awk',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.awk',      { link = "Variable" })  -- FS, RS, OFS, ORS, OFMT, CONVFMT, SUBSEP (modifiable)
  highlight(0, '@variable.parameter.awk',    { link = "Variable" })  -- Function parameters

  -- Constants
  highlight(0, '@constant.awk',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.awk',      { link = "Constant" })  -- NR, NF, FNR, FILENAME, ARGC, ARGV, etc. (auto-set)

  -- Functions
  highlight(0, '@function.awk',              { link = "Function" })  -- User-defined functions
  highlight(0, '@function.call.awk',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.awk',      { link = "Function" })  -- Built-in functions (print, printf, substr, split, etc.)

  -- Types
  highlight(0, '@type.awk',                  { link = "Type" })  -- Types

  -- Modules
  highlight(0, '@module.awk',                { fg = colors.turquoise, bg = 'NONE' })  -- Namespace-qualified names

  -- Labels
  highlight(0, '@label.awk',                 { fg = colors.pink,      bg = 'NONE' })  -- BEGIN, END, BEGINFILE, ENDFILE

  -- Keywords
  highlight(0, '@keyword.awk',               { link = "Keyword" })  -- General keywords (delete, in)
  highlight(0, '@keyword.function.awk',      { link = "Keyword" })  -- function, func
  highlight(0, '@keyword.operator.awk',      { link = "Operator" })  -- Operators
  highlight(0, '@keyword.return.awk',        { link = "Keyword" })  -- return, exit
  highlight(0, '@keyword.repeat.awk',        { link = "Keyword" })  -- while, for, do, in
  highlight(0, '@keyword.conditional.awk',   { link = "Conditional" })  -- if, else, switch, case, default
  highlight(0, '@keyword.import.awk',        { link = "Keyword" })  -- @include, @load
  highlight(0, '@keyword.directive.awk',     { link = "Keyword" })  -- @namespace

  -- Strings
  highlight(0, '@string.awk',                { link = "String" })  -- Strings
  highlight(0, '@string.regexp.awk',         { link = "String" })  -- Regular expressions
  highlight(0, '@string.escape.awk',         { link = "String" })  -- Escape sequences
  highlight(0, '@character.awk',             { fg = colors.redLight,  bg = 'NONE' })  -- Characters
  highlight(0, '@character.special.awk',     { fg = colors.pink,      bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number.awk',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.awk',          { link = "Number" })  -- Floats

  -- Comments
  highlight(0, '@comment.awk',               { link = "Comment" })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.awk',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.awk',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.awk', { link = "Delimiter" })  -- ; , : $ / @
  highlight(0, '@punctuation.special.awk',   { fg = colors.pink,      bg = 'NONE' })  -- $ (field reference)


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.awk)

  highlight(0, '@lsp.type.variable.awk',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.awk',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.awk',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.awk',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.type.awk',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.namespace.awk',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.keyword.awk',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.awk',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.awk',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.regexp.awk',        { fg = colors.redLight,  bg = 'NONE' })  -- Regular expressions
  highlight(0, '@lsp.type.number.awk',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.awk',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.awk',    { link = "Variable" })  -- Built-in auto-set variables
  highlight(0, '@lsp.typemod.function.declaration.awk', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.awk', { fg = colors.orange, bg = 'NONE' })  -- Built-in functions
end

return awk

