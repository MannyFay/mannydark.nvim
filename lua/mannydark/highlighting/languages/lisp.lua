-------------------------------------------------------------------------------
-- Lisp (Common Lisp)
-- Highlighting for .lisp, .lsp, .cl, .asd, .asdf files.
-- Also covers general Lisp syntax applicable to other dialects.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local lisp      = {}


-------------------------------------------------------------------------------
-- Settings

lisp.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups (vim runtime lisp.vim)
  ---------------------------------------------------------------------------

  -- Atoms and Symbols
  highlight(0, 'lispAtom',              { fg = colors.pink,       bg = 'NONE' })  -- Atoms (symbols)
  highlight(0, 'lispAtomMark',          { fg = colors.pink,       bg = 'NONE' })  -- Atom markers
  highlight(0, 'lispAtomNmbr',          { fg = colors.greenLight, bg = 'NONE' })  -- Numeric atoms
  highlight(0, 'lispAtomBarSymbol',     { fg = colors.pink,       bg = 'NONE' })  -- |bar symbols|
  highlight(0, 'lispAtomList',          { fg = colors.white,      bg = 'NONE' })  -- Atom lists
  highlight(0, 'lispSymbol',            { fg = colors.pink,       bg = 'NONE' })  -- Symbols
  highlight(0, 'lispBarSymbol',         { fg = colors.pink,       bg = 'NONE' })  -- |bar symbols|

  -- Keywords (package keywords :keyword)
  highlight(0, 'lispKey',               { fg = colors.pink,       bg = 'NONE' })  -- :keywords

  -- Variables
  highlight(0, 'lispVar',               { link = "Variable" })  -- Variables

  -- Functions
  highlight(0, 'lispFunc',              { link = "Function" })  -- Function names/calls

  -- Declarations/Special Forms
  highlight(0, 'lispDecl',              { fg = colors.blue,       bg = 'NONE' })  -- defun, defvar, defmacro, let, lambda, etc.

  -- Numbers
  highlight(0, 'lispNumber',            { link = "Number" })  -- Numbers (including ratios, complex)

  -- Strings
  highlight(0, 'lispString',            { link = "String" })  -- "strings"
  highlight(0, 'lispInString',          { link = "String" })  -- Inside strings
  highlight(0, 'lispInStringString',    { link = "String" })  -- Nested strings

  -- Escape Sequences
  highlight(0, 'lispEscapeSpecial',     { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences

  -- Parentheses
  highlight(0, 'lispParen',             { fg = colors.white,      bg = 'NONE' })  -- Parentheses ()
  highlight(0, 'lispParenError',        { fg = colors.red,        bg = 'NONE' })  -- Mismatched parens

  -- Lists
  highlight(0, 'lispList',              { fg = colors.white,      bg = 'NONE' })  -- List expressions
  highlight(0, 'lispBQList',            { fg = colors.white,      bg = 'NONE' })  -- Backquoted lists

  -- Quote/Backquote
  highlight(0, 'lispMark',              { fg = colors.pink,       bg = 'NONE' })  -- ' ` , ,@
  highlight(0, 'lispConcat',            { fg = colors.pink,       bg = 'NONE' })  -- ,@ (splice)

  -- Comments
  highlight(0, 'lispComment',           { link = "Comment" })  -- ; comments
  highlight(0, 'lispCommentRegion',     { link = "Comment" })  -- #| ... |# block comments
  highlight(0, 'lispTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME

  -- Leading Whitespace (for indentation)
  highlight(0, 'lispLeadWhite',         { fg = colors.white,      bg = 'NONE' })  -- Leading whitespace


  ---------------------------------------------------------------------------
  -- Rainbow Parentheses (when g:lisp_rainbow is enabled)
  ---------------------------------------------------------------------------

  highlight(0, 'hlLevel0',              { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'hlLevel1',              { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'hlLevel2',              { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'hlLevel3',              { fg = colors.pink,       bg = 'NONE' })
  highlight(0, 'hlLevel4',              { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'hlLevel5',              { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'hlLevel6',              { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, 'hlLevel7',              { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, 'hlLevel8',              { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'hlLevel9',              { fg = colors.orange,     bg = 'NONE' })


  ---------------------------------------------------------------------------
  -- Common Lisp Specific Groups
  ---------------------------------------------------------------------------

  -- Special Forms
  highlight(0, 'lispSpecial',           { fg = colors.blue,       bg = 'NONE' })  -- Special forms
  highlight(0, 'lispDefun',             { fg = colors.blue,       bg = 'NONE' })  -- defun
  highlight(0, 'lispDefvar',            { fg = colors.blue,       bg = 'NONE' })  -- defvar, defparameter, defconstant
  highlight(0, 'lispDefmacro',          { fg = colors.blue,       bg = 'NONE' })  -- defmacro
  highlight(0, 'lispDefgeneric',        { fg = colors.blue,       bg = 'NONE' })  -- defgeneric
  highlight(0, 'lispDefmethod',         { fg = colors.blue,       bg = 'NONE' })  -- defmethod
  highlight(0, 'lispDefclass',          { fg = colors.blue,       bg = 'NONE' })  -- defclass
  highlight(0, 'lispDefstruct',         { fg = colors.blue,       bg = 'NONE' })  -- defstruct
  highlight(0, 'lispDeftype',           { fg = colors.blue,       bg = 'NONE' })  -- deftype
  highlight(0, 'lispDefpackage',        { fg = colors.blue,       bg = 'NONE' })  -- defpackage
  highlight(0, 'lispDefcondition',      { fg = colors.blue,       bg = 'NONE' })  -- define-condition
  highlight(0, 'lispDefsystem',         { fg = colors.blue,       bg = 'NONE' })  -- defsystem (ASDF)

  -- Control Flow
  highlight(0, 'lispIf',                { fg = colors.blue,       bg = 'NONE' })  -- if
  highlight(0, 'lispWhen',              { fg = colors.blue,       bg = 'NONE' })  -- when
  highlight(0, 'lispUnless',            { fg = colors.blue,       bg = 'NONE' })  -- unless
  highlight(0, 'lispCond',              { fg = colors.blue,       bg = 'NONE' })  -- cond
  highlight(0, 'lispCase',              { fg = colors.blue,       bg = 'NONE' })  -- case, ecase, ccase, typecase
  highlight(0, 'lispLoop',              { fg = colors.blue,       bg = 'NONE' })  -- loop
  highlight(0, 'lispDo',                { fg = colors.blue,       bg = 'NONE' })  -- do, do*, dolist, dotimes, do-symbols
  highlight(0, 'lispProgn',             { fg = colors.blue,       bg = 'NONE' })  -- progn, prog1, prog2
  highlight(0, 'lispBlock',             { fg = colors.blue,       bg = 'NONE' })  -- block
  highlight(0, 'lispReturn',            { fg = colors.blue,       bg = 'NONE' })  -- return, return-from
  highlight(0, 'lispTagbody',           { fg = colors.blue,       bg = 'NONE' })  -- tagbody, go

  -- Binding Forms
  highlight(0, 'lispLet',               { fg = colors.blue,       bg = 'NONE' })  -- let, let*
  highlight(0, 'lispFlet',              { fg = colors.blue,       bg = 'NONE' })  -- flet, labels, macrolet
  highlight(0, 'lispLambda',            { fg = colors.blue,       bg = 'NONE' })  -- lambda
  highlight(0, 'lispMultipleValueBind', { fg = colors.blue,       bg = 'NONE' })  -- multiple-value-bind
  highlight(0, 'lispDestructuringBind', { fg = colors.blue,       bg = 'NONE' })  -- destructuring-bind

  -- Exception Handling
  highlight(0, 'lispHandler',           { fg = colors.blue,       bg = 'NONE' })  -- handler-case, handler-bind
  highlight(0, 'lispRestart',           { fg = colors.blue,       bg = 'NONE' })  -- restart-case, restart-bind
  highlight(0, 'lispUnwindProtect',     { fg = colors.blue,       bg = 'NONE' })  -- unwind-protect
  highlight(0, 'lispCatch',             { fg = colors.blue,       bg = 'NONE' })  -- catch
  highlight(0, 'lispThrow',             { fg = colors.blue,       bg = 'NONE' })  -- throw
  highlight(0, 'lispError',             { fg = colors.blue,       bg = 'NONE' })  -- error, cerror, signal

  -- Evaluation
  highlight(0, 'lispEval',              { fg = colors.blue,       bg = 'NONE' })  -- eval, eval-when
  highlight(0, 'lispQuote',             { fg = colors.blue,       bg = 'NONE' })  -- quote
  highlight(0, 'lispFunction',          { link = "Function" })  -- function, #'
  highlight(0, 'lispApply',             { fg = colors.orange,     bg = 'NONE' })  -- apply, funcall
  highlight(0, 'lispSetq',              { fg = colors.blue,       bg = 'NONE' })  -- setq, setf, psetq, psetf

  -- Type/Class System
  highlight(0, 'lispType',              { link = "Type" })  -- Type names
  highlight(0, 'lispClass',             { fg = colors.turquoise,  bg = 'NONE' })  -- Class names
  highlight(0, 'lispSlot',              { fg = colors.orange,     bg = 'NONE' })  -- Slot names
  highlight(0, 'lispMethod',            { link = "Function" })  -- Method names
  highlight(0, 'lispGeneric',           { fg = colors.orange,     bg = 'NONE' })  -- Generic function names

  -- Built-in Types
  highlight(0, 'lispBuiltinType',       { link = "Type" })  -- nil, t, list, cons, symbol, number, string, character, array, vector, hash-table, package, stream, function, class, condition, restart

  -- Booleans/Constants
  highlight(0, 'lispBoolean',           { link = "Boolean" })  -- t, nil
  highlight(0, 'lispConstant',          { link = "Constant" })  -- *standard-input*, *standard-output*, etc.
  highlight(0, 'lispSpecialVar',        { link = "Variable" })  -- *earmuff* variables

  -- Characters
  highlight(0, 'lispCharacter',         { fg = colors.redLight,   bg = 'NONE' })  -- #\a, #\newline, #\space

  -- Reader Macros
  highlight(0, 'lispReaderMacro',       { fg = colors.pink,       bg = 'NONE' })  -- #', #\, #(, #*, #+, #-, #., #:, #=, ##, #', #|
  highlight(0, 'lispSharpQuote',        { fg = colors.pink,       bg = 'NONE' })  -- #' (function)
  highlight(0, 'lispSharpPlus',         { fg = colors.pink,       bg = 'NONE' })  -- #+ (read-time conditional)
  highlight(0, 'lispSharpMinus',        { fg = colors.pink,       bg = 'NONE' })  -- #- (read-time conditional)
  highlight(0, 'lispVector',            { fg = colors.pink,       bg = 'NONE' })  -- #() vector literal
  highlight(0, 'lispBitVector',         { fg = colors.pink,       bg = 'NONE' })  -- #*0101 bit vector
  highlight(0, 'lispPathname',          { fg = colors.redLight,   bg = 'NONE' })  -- #p"pathname"
  highlight(0, 'lispComplex',           { fg = colors.greenLight, bg = 'NONE' })  -- #c(real imag)

  -- Package Qualifiers
  highlight(0, 'lispPackage',           { fg = colors.turquoise,  bg = 'NONE' })  -- package:symbol, package::symbol

  -- Format Strings
  highlight(0, 'lispFormat',            { fg = colors.pink,       bg = 'NONE' })  -- ~a, ~s, ~d, ~%, etc. in format strings

  -- Declarations
  highlight(0, 'lispDeclare',           { fg = colors.pink,       bg = 'NONE' })  -- declare
  highlight(0, 'lispProclaim',          { fg = colors.pink,       bg = 'NONE' })  -- proclaim
  highlight(0, 'lispDeclaration',       { fg = colors.pink,       bg = 'NONE' })  -- type, ftype, inline, notinline, optimize, special, dynamic-extent, ignorable, ignore

  -- Common Functions
  highlight(0, 'lispCoreFunc',          { link = "Function" })  -- car, cdr, cons, list, append, mapcar, mapc, reduce, remove, find, position, sort, length, nth, elt, aref, gethash, print, format, read, write


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.commonlisp / @xxx.lisp)
  ---------------------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.commonlisp',          { link = "Variable" })  -- Variables
  highlight(0, '@variable.lisp',                { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.commonlisp', { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.parameter.lisp',      { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.builtin.commonlisp',  { link = "Variable" })  -- *special* variables
  highlight(0, '@variable.builtin.lisp',        { link = "Variable" })  -- *special* variables

  -- Constants
  highlight(0, '@constant.commonlisp',          { link = "Constant" })  -- Constants
  highlight(0, '@constant.lisp',                { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.commonlisp',  { link = "Constant" })  -- t, nil
  highlight(0, '@constant.builtin.lisp',        { link = "Constant" })  -- t, nil

  -- Booleans
  highlight(0, '@boolean.commonlisp',           { link = "Boolean" })  -- t, nil
  highlight(0, '@boolean.lisp',                 { link = "Boolean" })  -- t, nil

  -- Numbers
  highlight(0, '@number.commonlisp',            { link = "Number" })  -- Numbers
  highlight(0, '@number.lisp',                  { link = "Number" })  -- Numbers

  -- Strings
  highlight(0, '@string.commonlisp',            { link = "String" })  -- Strings
  highlight(0, '@string.lisp',                  { link = "String" })  -- Strings
  highlight(0, '@string.escape.commonlisp',     { link = "String" })  -- Escape sequences
  highlight(0, '@string.escape.lisp',           { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.symbol.commonlisp', { link = "String" })  -- Symbols
  highlight(0, '@string.special.symbol.lisp',   { link = "String" })  -- Symbols

  -- Characters
  highlight(0, '@character.commonlisp',         { fg = colors.redLight,   bg = 'NONE' })  -- #\char
  highlight(0, '@character.lisp',               { fg = colors.redLight,   bg = 'NONE' })  -- #\char

  -- Functions
  highlight(0, '@function.commonlisp',          { link = "Function" })  -- Function names
  highlight(0, '@function.lisp',                { link = "Function" })  -- Function names
  highlight(0, '@function.builtin.commonlisp',  { link = "Function" })  -- Built-in functions
  highlight(0, '@function.builtin.lisp',        { link = "Function" })  -- Built-in functions
  highlight(0, '@function.macro.commonlisp',    { link = "Function" })  -- Macros
  highlight(0, '@function.macro.lisp',          { link = "Function" })  -- Macros

  -- Types
  highlight(0, '@type.commonlisp',              { link = "Type" })  -- Type names
  highlight(0, '@type.lisp',                    { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.commonlisp',      { link = "Type" })  -- Built-in types
  highlight(0, '@type.builtin.lisp',            { link = "Type" })  -- Built-in types

  -- Modules/Packages
  highlight(0, '@module.commonlisp',            { fg = colors.turquoise,  bg = 'NONE' })  -- Package names
  highlight(0, '@module.lisp',                  { fg = colors.turquoise,  bg = 'NONE' })  -- Package names

  -- Keywords
  highlight(0, '@keyword.commonlisp',           { link = "Keyword" })  -- Special forms
  highlight(0, '@keyword.lisp',                 { link = "Keyword" })  -- Special forms
  highlight(0, '@keyword.function.commonlisp',  { link = "Keyword" })  -- defun, lambda
  highlight(0, '@keyword.function.lisp',        { link = "Keyword" })  -- defun, lambda
  highlight(0, '@keyword.conditional.commonlisp', { link = "Conditional" })  -- if, when, unless, cond, case
  highlight(0, '@keyword.conditional.lisp',     { link = "Conditional" })  -- if, when, unless, cond, case
  highlight(0, '@keyword.repeat.commonlisp',    { link = "Keyword" })  -- loop, do, dolist, dotimes
  highlight(0, '@keyword.repeat.lisp',          { link = "Keyword" })  -- loop, do, dolist, dotimes

  -- Operators
  highlight(0, '@operator.commonlisp',          { link = "Operator" })  -- Operators
  highlight(0, '@operator.lisp',                { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.commonlisp', { fg = colors.white,    bg = 'NONE' })  -- ()
  highlight(0, '@punctuation.bracket.lisp',     { fg = colors.white,      bg = 'NONE' })  -- ()
  highlight(0, '@punctuation.special.commonlisp', { fg = colors.pink,     bg = 'NONE' })  -- ', `, ,, ,@, #', etc.
  highlight(0, '@punctuation.special.lisp',     { fg = colors.pink,       bg = 'NONE' })  -- ', `, ,, ,@, #', etc.

  -- Comments
  highlight(0, '@comment.commonlisp',           { link = "Comment" })  -- Comments
  highlight(0, '@comment.lisp',                 { link = "Comment" })  -- Comments


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.lisp / @lsp.type.xxx.commonlisp)
  ---------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.lisp',       { link = "Variable" })
  highlight(0, '@lsp.type.variable.commonlisp', { link = "Variable" })
  highlight(0, '@lsp.type.parameter.lisp',      { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@lsp.type.parameter.commonlisp', { fg = colors.purple,    bg = 'NONE' })
  highlight(0, '@lsp.type.function.lisp',       { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.function.commonlisp', { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.macro.lisp',          { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.macro.commonlisp',    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.type.lisp',           { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.type.commonlisp',     { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.class.lisp',          { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.class.commonlisp',    { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@lsp.type.namespace.lisp',      { fg = colors.turquoise,  bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.namespace.commonlisp', { fg = colors.turquoise, bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.keyword.lisp',        { link = "Keyword" })  -- :keywords
  highlight(0, '@lsp.type.keyword.commonlisp',  { link = "Keyword" })  -- :keywords
  highlight(0, '@lsp.type.string.lisp',         { link = "String" })
  highlight(0, '@lsp.type.string.commonlisp',   { link = "String" })
  highlight(0, '@lsp.type.number.lisp',         { link = "Number" })
  highlight(0, '@lsp.type.number.commonlisp',   { link = "Number" })
  highlight(0, '@lsp.type.comment.lisp',        { link = "Comment" })
  highlight(0, '@lsp.type.comment.commonlisp',  { link = "Comment" })

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.lisp',       { link = "Variable" })
  highlight(0, '@lsp.typemod.variable.readonly.commonlisp', { link = "Variable" })
  highlight(0, '@lsp.typemod.variable.global.lisp',         { link = "Variable" })  -- *special*
  highlight(0, '@lsp.typemod.variable.global.commonlisp',   { link = "Variable" })  -- *special*
  highlight(0, '@lsp.typemod.function.declaration.lisp',    { fg = colors.orange, bg = 'NONE' })
  highlight(0, '@lsp.typemod.function.declaration.commonlisp', { fg = colors.orange, bg = 'NONE' })


  ---------------------------------------------------------------------------
  -- Scheme Specific (also uses lisp.vim base)
  ---------------------------------------------------------------------------

  highlight(0, 'schemeFunc',            { link = "Function" })  -- Scheme functions
  highlight(0, 'schemeKeyword',         { link = "Keyword" })  -- define, lambda, let, if, cond, etc.
  highlight(0, 'schemeVariable',        { link = "Variable" })  -- Variables
  highlight(0, 'schemeString',          { link = "String" })  -- Strings
  highlight(0, 'schemeNumber',          { link = "Number" })  -- Numbers
  highlight(0, 'schemeBoolean',         { link = "Boolean" })  -- #t, #f
  highlight(0, 'schemeCharacter',       { fg = colors.redLight,   bg = 'NONE' })  -- Characters
  highlight(0, 'schemeComment',         { link = "Comment" })  -- Comments
  highlight(0, 'schemeQuote',           { fg = colors.pink,       bg = 'NONE' })  -- ', `, etc.
  highlight(0, 'schemeSyntax',          { fg = colors.blue,       bg = 'NONE' })  -- Syntax keywords

  -- Scheme Treesitter
  highlight(0, '@variable.scheme',              { link = "Variable" })
  highlight(0, '@variable.builtin.scheme',      { link = "Variable" })
  highlight(0, '@function.scheme',              { link = "Function" })
  highlight(0, '@function.builtin.scheme',      { link = "Function" })
  highlight(0, '@keyword.scheme',               { link = "Keyword" })
  highlight(0, '@keyword.conditional.scheme',   { link = "Conditional" })
  highlight(0, '@string.scheme',                { link = "String" })
  highlight(0, '@string.escape.scheme',         { link = "String" })
  highlight(0, '@string.special.symbol.scheme', { link = "String" })
  highlight(0, '@number.scheme',                { link = "Number" })
  highlight(0, '@boolean.scheme',               { link = "Boolean" })
  highlight(0, '@character.scheme',             { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@comment.scheme',               { link = "Comment" })
  highlight(0, '@punctuation.bracket.scheme',   { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@module.scheme',                { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@operator.scheme',              { link = "Operator" })


  ---------------------------------------------------------------------------
  -- Emacs Lisp Specific
  ---------------------------------------------------------------------------

  highlight(0, 'elispDefun',            { fg = colors.blue,       bg = 'NONE' })  -- defun
  highlight(0, 'elispDefvar',           { fg = colors.blue,       bg = 'NONE' })  -- defvar, defconst, defcustom
  highlight(0, 'elispDefmacro',         { fg = colors.blue,       bg = 'NONE' })  -- defmacro
  highlight(0, 'elispLambda',           { fg = colors.blue,       bg = 'NONE' })  -- lambda
  highlight(0, 'elispKeyword',          { link = "Keyword" })  -- Keywords
  highlight(0, 'elispFunc',             { link = "Function" })  -- Functions
  highlight(0, 'elispBuiltin',          { link = "Function" })  -- Built-in functions
  highlight(0, 'elispVariable',         { link = "Variable" })  -- Variables
  highlight(0, 'elispSpecialVar',       { link = "Variable" })  -- Special variables
  highlight(0, 'elispString',           { link = "String" })  -- Strings
  highlight(0, 'elispNumber',           { link = "Number" })  -- Numbers
  highlight(0, 'elispComment',          { link = "Comment" })  -- Comments
  highlight(0, 'elispDocstring',        { link = "String" })  -- Docstrings
  highlight(0, 'elispQuote',            { fg = colors.pink,       bg = 'NONE' })  -- Quoting


  ---------------------------------------------------------------------------
  -- SLIME/SLY/Vlime Integration
  ---------------------------------------------------------------------------

  highlight(0, 'slimeReplPrompt',       { fg = colors.blue,       bg = 'NONE', bold = true })  -- REPL prompt
  highlight(0, 'slimeReplResult',       { fg = colors.white,      bg = 'NONE' })  -- REPL result
  highlight(0, 'slimeInspector',        { fg = colors.turquoise,  bg = 'NONE' })  -- Inspector
  highlight(0, 'slimeError',            { fg = colors.red,        bg = 'NONE' })  -- Errors
  highlight(0, 'slimeWarning',          { fg = colors.orange,     bg = 'NONE' })  -- Warnings
  highlight(0, 'slimeNote',             { fg = colors.blue,       bg = 'NONE' })  -- Notes
end

return lisp
