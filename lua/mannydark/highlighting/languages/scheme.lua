-------------------------------------------------------------------------------
-- Scheme (R5RS/R6RS/R7RS)
-- Highlighting for .scm, .ss, .sld, .sls, .sps files.
-- Also covers Racket (.rkt), Guile, Chicken, Chez Scheme, MIT Scheme.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local scheme    = {}


-------------------------------------------------------------------------------
-- Settings

scheme.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups (vim runtime scheme.vim)
  ---------------------------------------------------------------------------

  -- Identifiers
  highlight(0, 'schemeIdentifier',        { fg = colors.white,      bg = 'NONE' })  -- General identifiers

  -- Parentheses/Brackets
  highlight(0, 'schemeParentheses',       { fg = colors.white,      bg = 'NONE' })  -- ()
  highlight(0, 'schemeBrackets',          { fg = colors.white,      bg = 'NONE' })  -- []
  highlight(0, 'schemeBraces',            { fg = colors.white,      bg = 'NONE' })  -- {}

  -- Quote/Quasiquote
  highlight(0, 'schemeQuote',             { fg = colors.pink,       bg = 'NONE' })  -- '
  highlight(0, 'schemeQuasiquote',        { fg = colors.pink,       bg = 'NONE' })  -- `
  highlight(0, 'schemeUnquote',           { fg = colors.pink,       bg = 'NONE' })  -- , ,@
  highlight(0, 'schemeQuoteForm',         { fg = colors.pink,       bg = 'NONE' })  -- Quoted forms
  highlight(0, 'schemeQuasiquoteForm',    { fg = colors.pink,       bg = 'NONE' })  -- Quasiquoted forms

  -- Symbols
  highlight(0, 'schemeSymbol',            { fg = colors.pink,       bg = 'NONE' })  -- 'symbol

  -- Data
  highlight(0, 'schemeData',              { fg = colors.white,      bg = 'NONE' })  -- Data expressions
  highlight(0, 'schemeConstant',          { link = "Constant" })  -- Constants

  -- Booleans
  highlight(0, 'schemeBoolean',           { link = "Boolean" })  -- #t, #f, #true, #false

  -- Numbers
  highlight(0, 'schemeNumber',            { link = "Number" })  -- Numbers (integer, rational, real, complex)

  -- Characters
  highlight(0, 'schemeCharacter',         { fg = colors.redLight,   bg = 'NONE' })  -- #\a, #\newline, #\space, #\tab

  -- Strings
  highlight(0, 'schemeString',            { link = "String" })  -- "strings"
  highlight(0, 'schemeStringEscape',      { link = "String" })  -- Escape sequences

  -- Keywords (special forms)
  highlight(0, 'schemeKeyword',           { link = "Keyword" })  -- General keywords

  -- Syntax Keywords (R5RS/R6RS/R7RS)
  highlight(0, 'schemeSyntax',            { fg = colors.blue,       bg = 'NONE' })  -- define, lambda, let, let*, letrec, letrec*, if, cond, case, when, unless, and, or, begin, do, set!, delay, force, quote, quasiquote, unquote, unquote-splicing
  highlight(0, 'schemeSyntaxSyntax',      { fg = colors.blue,       bg = 'NONE' })  -- define-syntax, let-syntax, letrec-syntax, syntax-rules, syntax-case, identifier-syntax
  highlight(0, 'schemeSpecialSyntax',     { fg = colors.blue,       bg = 'NONE' })  -- Special syntax forms
  highlight(0, 'schemeTypeSyntax',        { link = "Type" })  -- Type-related syntax
  highlight(0, 'schemeExtraSyntax',       { fg = colors.blue,       bg = 'NONE' })  -- Extra/extension syntax

  -- Library/Module Syntax (R6RS/R7RS)
  highlight(0, 'schemeLibrarySyntax',     { fg = colors.blue,       bg = 'NONE' })  -- library, define-library, export, import
  highlight(0, 'schemeImport',            { fg = colors.pink,       bg = 'NONE' })  -- Import declarations
  highlight(0, 'schemeImportForm',        { fg = colors.white,      bg = 'NONE' })  -- Import form contents
  highlight(0, 'schemeImportKeyword',     { link = "Keyword" })  -- only, except, prefix, rename

  -- Functions
  highlight(0, 'schemeFunction',          { link = "Function" })  -- Built-in functions
  highlight(0, 'schemeFunc',              { link = "Function" })  -- Function calls

  -- Forms
  highlight(0, 'schemeForm',              { fg = colors.white,      bg = 'NONE' })  -- General forms

  -- Vectors
  highlight(0, 'schemeVector',            { fg = colors.pink,       bg = 'NONE' })  -- #(...) vectors

  -- Bytevectors
  highlight(0, 'schemeBytevector',        { fg = colors.pink,       bg = 'NONE' })  -- #u8(...) bytevectors

  -- Delimiters
  highlight(0, 'schemeDelimiter',         { link = "Delimiter" })  -- Delimiters

  -- Comments
  highlight(0, 'schemeComment',           { link = "Comment" })  -- ; comments
  highlight(0, 'schemeMultilineComment',  { link = "Comment" })  -- #| ... |# block comments
  highlight(0, 'schemeDatumComment',      { link = "Comment" })  -- #; datum comment
  highlight(0, 'schemeDatumCommentForm',  { link = "Comment" })  -- Commented-out form
  highlight(0, 'schemeTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Shebang
  highlight(0, 'schemeShebang',           { fg = colors.green,      bg = 'NONE' })  -- #!/usr/bin/env scheme

  -- Errors
  highlight(0, 'schemeError',             { fg = colors.red,        bg = 'NONE' })  -- Syntax errors


  ---------------------------------------------------------------------------
  -- Extended Scheme Syntax (Common across implementations)
  ---------------------------------------------------------------------------

  -- Definition Forms
  highlight(0, 'schemeDefine',            { fg = colors.blue,       bg = 'NONE' })  -- define
  highlight(0, 'schemeDefineSyntax',      { fg = colors.blue,       bg = 'NONE' })  -- define-syntax
  highlight(0, 'schemeDefineValues',      { fg = colors.blue,       bg = 'NONE' })  -- define-values
  highlight(0, 'schemeDefineRecord',      { fg = colors.blue,       bg = 'NONE' })  -- define-record-type
  highlight(0, 'schemeDefineLibrary',     { fg = colors.blue,       bg = 'NONE' })  -- define-library (R7RS)

  -- Lambda Forms
  highlight(0, 'schemeLambda',            { fg = colors.blue,       bg = 'NONE' })  -- lambda
  highlight(0, 'schemeCaseLambda',        { fg = colors.blue,       bg = 'NONE' })  -- case-lambda

  -- Binding Forms
  highlight(0, 'schemeLet',               { fg = colors.blue,       bg = 'NONE' })  -- let, let*, letrec, letrec*
  highlight(0, 'schemeLetSyntax',         { fg = colors.blue,       bg = 'NONE' })  -- let-syntax, letrec-syntax
  highlight(0, 'schemeLetValues',         { fg = colors.blue,       bg = 'NONE' })  -- let-values, let*-values
  highlight(0, 'schemeNamedLet',          { fg = colors.blue,       bg = 'NONE' })  -- Named let
  highlight(0, 'schemeFluid',             { fg = colors.blue,       bg = 'NONE' })  -- fluid-let, parameterize

  -- Control Flow
  highlight(0, 'schemeIf',                { fg = colors.blue,       bg = 'NONE' })  -- if
  highlight(0, 'schemeCond',              { fg = colors.blue,       bg = 'NONE' })  -- cond
  highlight(0, 'schemeCase',              { fg = colors.blue,       bg = 'NONE' })  -- case
  highlight(0, 'schemeWhen',              { fg = colors.blue,       bg = 'NONE' })  -- when
  highlight(0, 'schemeUnless',            { fg = colors.blue,       bg = 'NONE' })  -- unless
  highlight(0, 'schemeAnd',               { fg = colors.blue,       bg = 'NONE' })  -- and
  highlight(0, 'schemeOr',                { fg = colors.blue,       bg = 'NONE' })  -- or
  highlight(0, 'schemeBegin',             { fg = colors.blue,       bg = 'NONE' })  -- begin

  -- Iteration
  highlight(0, 'schemeDo',                { fg = colors.blue,       bg = 'NONE' })  -- do
  highlight(0, 'schemeLoop',              { fg = colors.blue,       bg = 'NONE' })  -- Loop constructs

  -- Delayed Evaluation
  highlight(0, 'schemeDelay',             { fg = colors.blue,       bg = 'NONE' })  -- delay, delay-force
  highlight(0, 'schemeForce',             { fg = colors.orange,     bg = 'NONE' })  -- force
  highlight(0, 'schemePromise',           { fg = colors.blue,       bg = 'NONE' })  -- make-promise

  -- Assignment
  highlight(0, 'schemeSet',               { fg = colors.blue,       bg = 'NONE' })  -- set!

  -- Continuations
  highlight(0, 'schemeCallCC',            { fg = colors.blue,       bg = 'NONE' })  -- call/cc, call-with-current-continuation
  highlight(0, 'schemeDynamicWind',       { fg = colors.blue,       bg = 'NONE' })  -- dynamic-wind
  highlight(0, 'schemeValues',            { fg = colors.blue,       bg = 'NONE' })  -- values, call-with-values

  -- Exception Handling
  highlight(0, 'schemeGuard',             { fg = colors.blue,       bg = 'NONE' })  -- guard
  highlight(0, 'schemeRaise',             { fg = colors.blue,       bg = 'NONE' })  -- raise, raise-continuable
  highlight(0, 'schemeError',             { fg = colors.blue,       bg = 'NONE' })  -- error
  highlight(0, 'schemeWithExceptionHandler', { fg = colors.blue,    bg = 'NONE' })  -- with-exception-handler

  -- Syntax-Rules
  highlight(0, 'schemeSyntaxRules',       { fg = colors.blue,       bg = 'NONE' })  -- syntax-rules
  highlight(0, 'schemeSyntaxCase',        { fg = colors.blue,       bg = 'NONE' })  -- syntax-case (R6RS)
  highlight(0, 'schemePatternVar',        { link = "Variable" })  -- Pattern variables
  highlight(0, 'schemeEllipsis',          { fg = colors.pink,       bg = 'NONE' })  -- ... ellipsis

  -- Parameters (R7RS)
  highlight(0, 'schemeParameter',         { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, 'schemeParameterize',      { fg = colors.blue,       bg = 'NONE' })  -- parameterize


  ---------------------------------------------------------------------------
  -- Built-in Procedures (Core R7RS)
  ---------------------------------------------------------------------------

  -- List Operations
  highlight(0, 'schemeListFunc',          { link = "Function" })  -- car, cdr, cons, list, append, reverse, length, list-ref, list-tail, list-copy, memq, memv, member, assq, assv, assoc

  -- Pair Operations
  highlight(0, 'schemePairFunc',          { link = "Function" })  -- pair?, null?, cons, car, cdr, set-car!, set-cdr!, caar, cadr, cdar, cddr, ...

  -- Type Predicates
  highlight(0, 'schemeTypePredicate',     { link = "Type" })  -- boolean?, number?, string?, symbol?, char?, vector?, list?, pair?, null?, procedure?, port?, eof-object?

  -- Numeric Functions
  highlight(0, 'schemeNumericFunc',       { link = "Function" })  -- +, -, *, /, abs, quotient, remainder, modulo, floor, ceiling, truncate, round, exp, log, sin, cos, tan, sqrt, expt, exact, inexact, number->string, string->number

  -- Comparison
  highlight(0, 'schemeCompareFunc',       { link = "Function" })  -- =, <, >, <=, >=, eq?, eqv?, equal?, zero?, positive?, negative?, odd?, even?

  -- String Functions
  highlight(0, 'schemeStringFunc',        { link = "String" })  -- string, make-string, string-length, string-ref, string-set!, string=?, string<?, string>?, string-append, substring, string->list, list->string, string-copy, string-fill!

  -- Character Functions
  highlight(0, 'schemeCharFunc',          { link = "Function" })  -- char=?, char<?, char>?, char-upcase, char-downcase, char->integer, integer->char, char-alphabetic?, char-numeric?, char-whitespace?

  -- Vector Functions
  highlight(0, 'schemeVectorFunc',        { link = "Function" })  -- vector, make-vector, vector-length, vector-ref, vector-set!, vector->list, list->vector, vector-copy, vector-fill!

  -- I/O Functions
  highlight(0, 'schemeIOFunc',            { link = "Function" })  -- read, write, display, newline, read-char, write-char, peek-char, open-input-file, open-output-file, close-input-port, close-output-port, call-with-input-file, call-with-output-file, with-input-from-file, with-output-to-file, current-input-port, current-output-port

  -- Control Functions
  highlight(0, 'schemeControlFunc',       { link = "Function" })  -- apply, map, for-each, call/cc, values, call-with-values

  -- Symbol Functions
  highlight(0, 'schemeSymbolFunc',        { link = "Function" })  -- symbol->string, string->symbol, symbol=?

  -- Boolean Functions
  highlight(0, 'schemeBooleanFunc',       { link = "Boolean" })  -- not, boolean=?

  -- Bytevector Functions (R7RS)
  highlight(0, 'schemeBytevectorFunc',    { link = "Function" })  -- bytevector, make-bytevector, bytevector-length, bytevector-u8-ref, bytevector-u8-set!, bytevector-copy, bytevector-append, utf8->string, string->utf8


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.scheme)
  ---------------------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.scheme',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.scheme',    { link = "Variable" })  -- Parameters
  highlight(0, '@variable.builtin.scheme',      { link = "Variable" })  -- Built-in variables (., ...)

  -- Constants
  highlight(0, '@constant.scheme',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.scheme',      { link = "Constant" })  -- #t, #f

  -- Booleans
  highlight(0, '@boolean.scheme',               { link = "Boolean" })  -- #t, #f, #true, #false

  -- Numbers
  highlight(0, '@number.scheme',                { link = "Number" })  -- Numbers
  highlight(0, '@number.float.scheme',          { link = "Number" })  -- Floats

  -- Strings
  highlight(0, '@string.scheme',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.scheme',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.symbol.scheme', { link = "String" })  -- Symbols

  -- Characters
  highlight(0, '@character.scheme',             { fg = colors.redLight,   bg = 'NONE' })  -- #\char
  highlight(0, '@character.special.scheme',     { fg = colors.pink,       bg = 'NONE' })  -- #\newline, #\space

  -- Functions
  highlight(0, '@function.scheme',              { link = "Function" })  -- Function definitions/calls
  highlight(0, '@function.call.scheme',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.scheme',      { link = "Function" })  -- Built-in procedures
  highlight(0, '@function.macro.scheme',        { link = "Function" })  -- Macros

  -- Keywords
  highlight(0, '@keyword.scheme',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.scheme',      { link = "Keyword" })  -- define, lambda
  highlight(0, '@keyword.conditional.scheme',   { link = "Conditional" })  -- if, cond, case, when, unless
  highlight(0, '@keyword.repeat.scheme',        { link = "Keyword" })  -- do
  highlight(0, '@keyword.import.scheme',        { link = "Keyword" })  -- import
  highlight(0, '@keyword.export.scheme',        { link = "Keyword" })  -- export

  -- Modules
  highlight(0, '@module.scheme',                { fg = colors.turquoise,  bg = 'NONE' })  -- Library/module names

  -- Operators
  highlight(0, '@operator.scheme',              { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.scheme',   { fg = colors.white,      bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.scheme', { link = "Delimiter" })  -- Delimiters
  highlight(0, '@punctuation.special.scheme',   { fg = colors.pink,       bg = 'NONE' })  -- ', `, ,, ,@, #

  -- Comments
  highlight(0, '@comment.scheme',               { link = "Comment" })  -- Comments

  -- Labels
  highlight(0, '@label.scheme',                 { fg = colors.pink,       bg = 'NONE' })  -- Labels


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.scheme)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.scheme',     { link = "Variable" })
  highlight(0, '@lsp.type.parameter.scheme',    { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.function.scheme',     { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.macro.scheme',        { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.scheme',      { link = "Keyword" })
  highlight(0, '@lsp.type.namespace.scheme',    { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.string.scheme',       { link = "String" })
  highlight(0, '@lsp.type.number.scheme',       { link = "Number" })
  highlight(0, '@lsp.type.comment.scheme',      { link = "Comment" })

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.scheme',    { link = "Variable" })
  highlight(0, '@lsp.typemod.function.declaration.scheme', { fg = colors.orange, bg = 'NONE' })
  highlight(0, '@lsp.typemod.function.defaultLibrary.scheme', { fg = colors.orange, bg = 'NONE' })


  ---------------------------------------------------------------------------
  -- Racket Specific (vim runtime racket.vim)
  ---------------------------------------------------------------------------

  -- Syntax
  highlight(0, 'racketSyntax',            { fg = colors.blue,       bg = 'NONE' })  -- Special forms
  highlight(0, 'racketFunc',              { link = "Function" })  -- Functions
  highlight(0, 'racketExtSyntax',         { fg = colors.pink,       bg = 'NONE' })  -- #:keyword arguments
  highlight(0, 'racketExtFunc',           { link = "Function" })  -- Extension functions

  -- Data Types
  highlight(0, 'racketString',            { link = "String" })  -- Strings
  highlight(0, 'racketStringEscape',      { link = "String" })  -- Escape sequences
  highlight(0, 'racketStringEscapeError', { link = "String" })  -- Invalid escapes
  highlight(0, 'racketUStringEscape',     { link = "String" })  -- Unicode escapes
  highlight(0, 'racketHereString',        { link = "String" })  -- Here-strings
  highlight(0, 'racketChar',              { fg = colors.redLight,   bg = 'NONE' })  -- Characters
  highlight(0, 'racketNumber',            { link = "Number" })  -- Numbers
  highlight(0, 'racketNumberError',       { link = "Number" })  -- Invalid numbers
  highlight(0, 'racketBoolean',           { link = "Boolean" })  -- #t, #f, #true, #false

  -- Structural
  highlight(0, 'racketParen',             { fg = colors.white,      bg = 'NONE' })  -- Parentheses
  highlight(0, 'racketDelimiter',         { link = "Delimiter" })  -- Delimiters
  highlight(0, 'racketStruc',             { fg = colors.pink,       bg = 'NONE' })  -- #(...) structures
  highlight(0, 'racketConstant',          { link = "Constant" })  -- *name*, <name>

  -- Quote
  highlight(0, 'racketQuote',             { fg = colors.pink,       bg = 'NONE' })  -- Quote operators
  highlight(0, 'racketUnquote',           { fg = colors.pink,       bg = 'NONE' })  -- Unquote operators

  -- Comments
  highlight(0, 'racketComment',           { link = "Comment" })  -- Comments
  highlight(0, 'racketMultilineComment',  { link = "Comment" })  -- Block comments
  highlight(0, 'racketFormComment',       { link = "Comment" })  -- #; form comments
  highlight(0, 'racketSharpBang',         { fg = colors.green,      bg = 'NONE' })  -- #! shebang
  highlight(0, 'racketTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO
  highlight(0, 'racketNote',              { fg = colors.blue,       bg = 'NONE', bold = true })  -- NOTE

  -- Literals
  highlight(0, 'racketLit',               { fg = colors.pink,       bg = 'NONE' })  -- Hash literals
  highlight(0, 'racketRe',                { fg = colors.pink,       bg = 'NONE' })  -- Regex prefixes

  -- Errors
  highlight(0, 'racketError',             { fg = colors.red,        bg = 'NONE' })  -- Errors

  -- Racket Treesitter
  highlight(0, '@variable.racket',              { link = "Variable" })
  highlight(0, '@function.racket',              { link = "Function" })
  highlight(0, '@function.builtin.racket',      { link = "Function" })
  highlight(0, '@keyword.racket',               { link = "Keyword" })
  highlight(0, '@string.racket',                { link = "String" })
  highlight(0, '@number.racket',                { link = "Number" })
  highlight(0, '@boolean.racket',               { link = "Boolean" })
  highlight(0, '@comment.racket',               { link = "Comment" })
  highlight(0, '@punctuation.bracket.racket',   { fg = colors.white,      bg = 'NONE' })


  ---------------------------------------------------------------------------
  -- Implementation-Specific (Guile, Chicken, Chez, MIT)
  ---------------------------------------------------------------------------

  -- Guile Specific
  highlight(0, 'guileFunc',               { link = "Function" })  -- Guile functions
  highlight(0, 'guileKeyword',            { link = "Keyword" })  -- Guile keywords
  highlight(0, 'guileMacro',              { fg = colors.blue,       bg = 'NONE' })  -- Guile macros
  highlight(0, 'guileModule',             { fg = colors.turquoise,  bg = 'NONE' })  -- use-modules, define-module

  -- Chicken Specific
  highlight(0, 'chickenFunc',             { link = "Function" })  -- Chicken functions
  highlight(0, 'chickenKeyword',          { link = "Keyword" })  -- Chicken keywords
  highlight(0, 'chickenMacro',            { fg = colors.blue,       bg = 'NONE' })  -- Chicken macros
  highlight(0, 'chickenModule',           { fg = colors.turquoise,  bg = 'NONE' })  -- module, import

  -- Chez Scheme Specific
  highlight(0, 'chezFunc',                { link = "Function" })  -- Chez functions
  highlight(0, 'chezKeyword',             { link = "Keyword" })  -- Chez keywords
  highlight(0, 'chezMacro',               { fg = colors.blue,       bg = 'NONE' })  -- Chez macros

  -- MIT Scheme Specific
  highlight(0, 'mitSchemeFunc',           { link = "Function" })  -- MIT Scheme functions
  highlight(0, 'mitSchemeKeyword',        { link = "Keyword" })  -- MIT Scheme keywords


  ---------------------------------------------------------------------------
  -- Rainbow Parentheses (matching lisp.lua)
  ---------------------------------------------------------------------------

  highlight(0, 'schemeLevel0',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'schemeLevel1',            { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'schemeLevel2',            { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'schemeLevel3',            { fg = colors.pink,       bg = 'NONE' })
  highlight(0, 'schemeLevel4',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'schemeLevel5',            { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'schemeLevel6',            { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, 'schemeLevel7',            { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, 'schemeLevel8',            { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'schemeLevel9',            { fg = colors.orange,     bg = 'NONE' })
end

return scheme
