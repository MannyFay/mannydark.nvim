-------------------------------------------------------------------------------
-- Nim Files
-- Highlighting for .nim, .nims, .nimble files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local nim     = {}


-------------------------------------------------------------------------------
-- Settings

nim.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'nimKeyword',            { link = "Keyword" })  -- General keywords
  highlight(0, 'nimStatement',          { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, raise, discard
  highlight(0, 'nimConditional',        { link = "Conditional" })  -- if, elif, else, when, case, of
  highlight(0, 'nimRepeat',             { fg = colors.blue,       bg = 'NONE'            })  -- for, while, block
  highlight(0, 'nimLabel',              { fg = colors.blue,       bg = 'NONE'            })  -- block labels
  highlight(0, 'nimOperator',           { link = "Operator" })  -- and, or, not, xor, in, notin, is, isnot, div, mod, shl, shr
  highlight(0, 'nimException',          { fg = colors.blue,       bg = 'NONE'            })  -- try, except, finally, raise
  highlight(0, 'nimInclude',            { fg = colors.pink,       bg = 'NONE'            })  -- import, include, from, export
  highlight(0, 'nimDefer',              { fg = colors.blue,       bg = 'NONE'            })  -- defer

  -- Storage/Variable Declaration
  highlight(0, 'nimLet',                { fg = colors.blue,       bg = 'NONE'            })  -- let (immutable runtime)
  highlight(0, 'nimVar',                { link = "Variable" })  -- var (mutable)
  highlight(0, 'nimConst',              { fg = colors.blue,       bg = 'NONE'            })  -- const (compile-time constant)
  highlight(0, 'nimUsing',              { fg = colors.blue,       bg = 'NONE'            })  -- using

  -- Routine Types
  highlight(0, 'nimProc',               { fg = colors.blue,       bg = 'NONE'            })  -- proc keyword
  highlight(0, 'nimFunc',               { link = "Function" })  -- func (noSideEffect proc)
  highlight(0, 'nimMethod',             { link = "Function" })  -- method (dynamic dispatch)
  highlight(0, 'nimIterator',           { fg = colors.blue,       bg = 'NONE'            })  -- iterator
  highlight(0, 'nimConverter',          { fg = colors.blue,       bg = 'NONE'            })  -- converter
  highlight(0, 'nimTemplate',           { fg = colors.blue,       bg = 'NONE'            })  -- template
  highlight(0, 'nimMacro',              { fg = colors.pink,       bg = 'NONE'            })  -- macro

  -- Type Definitions
  highlight(0, 'nimTypedef',            { link = "Type" })  -- type keyword
  highlight(0, 'nimObject',             { fg = colors.blue,       bg = 'NONE'            })  -- object
  highlight(0, 'nimEnum',               { fg = colors.blue,       bg = 'NONE'            })  -- enum
  highlight(0, 'nimTuple',              { fg = colors.blue,       bg = 'NONE'            })  -- tuple
  highlight(0, 'nimDistinct',           { fg = colors.blue,       bg = 'NONE'            })  -- distinct
  highlight(0, 'nimConcept',            { fg = colors.blue,       bg = 'NONE'            })  -- concept
  highlight(0, 'nimRef',                { fg = colors.blue,       bg = 'NONE'            })  -- ref
  highlight(0, 'nimPtr',                { fg = colors.blue,       bg = 'NONE'            })  -- ptr
  highlight(0, 'nimMixin',              { fg = colors.blue,       bg = 'NONE'            })  -- mixin
  highlight(0, 'nimBind',               { fg = colors.blue,       bg = 'NONE'            })  -- bind
  highlight(0, 'nimInterface',          { fg = colors.blue,       bg = 'NONE'            })  -- interface

  -- Types
  highlight(0, 'nimType',               { link = "Type" })  -- Type names
  highlight(0, 'nimBuiltinType',        { link = "Type" })  -- int, float, string, bool, char, seq, array, etc.
  highlight(0, 'nimGenericType',        { link = "Type" })  -- Generic type parameters [T]

  -- Functions
  highlight(0, 'nimFunction',           { link = "Function" })  -- Function/proc names
  highlight(0, 'nimFunctionCall',       { link = "Function" })  -- Function calls
  highlight(0, 'nimBuiltinFunc',        { link = "Function" })  -- echo, len, high, low, sizeof, typeof, etc.

  -- Variables
  highlight(0, 'nimIdentifier',         { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'nimParameter',          { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'nimField',              { fg = colors.purple,     bg = 'NONE'            })  -- Object fields

  -- Constants
  highlight(0, 'nimConstant',           { link = "Constant" })  -- User constants
  highlight(0, 'nimBuiltinConstant',    { link = "Constant" })  -- true, false, nil
  highlight(0, 'nimBoolean',            { link = "Boolean" })  -- true, false
  highlight(0, 'nimNil',                { fg = colors.blue,       bg = 'NONE'            })  -- nil

  -- Strings
  highlight(0, 'nimString',             { link = "String" })  -- "strings"
  highlight(0, 'nimRawString',          { link = "String" })  -- r"raw strings"
  highlight(0, 'nimTripleString',       { link = "String" })  -- """triple quoted strings"""
  highlight(0, 'nimCharacter',          { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'nimStringEscape',       { link = "String" })  -- \n, \t, \x00, etc.
  highlight(0, 'nimStringFormat',       { link = "String" })  -- fmt string interpolation {expr}

  -- Numbers
  highlight(0, 'nimNumber',             { link = "Number" })  -- Numbers
  highlight(0, 'nimInteger',            { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'nimFloat',              { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'nimHex',                { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'nimOctal',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0o77 octal
  highlight(0, 'nimBinary',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary

  -- Operators
  highlight(0, 'nimOperatorSign',       { link = "Operator" })  -- + - * / = < > @ $ ~ & % ! ? ^ . : |
  highlight(0, 'nimDotOperator',        { link = "Operator" })  -- . field access
  highlight(0, 'nimRangeOperator',      { link = "Operator" })  -- .. range
  highlight(0, 'nimArrow',              { fg = colors.white,      bg = 'NONE'            })  -- -> return type

  -- Pragmas {. .}
  highlight(0, 'nimPragma',             { fg = colors.pink,       bg = 'NONE'            })  -- {. .} pragma delimiters
  highlight(0, 'nimPragmaKey',          { fg = colors.pink,       bg = 'NONE'            })  -- Pragma names
  highlight(0, 'nimPragmaEffect',       { fg = colors.pink,       bg = 'NONE'            })  -- noSideEffect, gcsafe, raises, tags
  highlight(0, 'nimPragmaInline',       { fg = colors.pink,       bg = 'NONE'            })  -- inline, noinline
  highlight(0, 'nimPragmaExport',       { fg = colors.pink,       bg = 'NONE'            })  -- exportc, importc, dynlib
  highlight(0, 'nimPragmaDeprecated',   { fg = colors.pink,       bg = 'NONE'            })  -- deprecated
  highlight(0, 'nimPragmaCompile',      { fg = colors.pink,       bg = 'NONE'            })  -- compileTime, static

  -- Comments
  highlight(0, 'nimComment',            { link = "Comment" })  -- # comments
  highlight(0, 'nimDocComment',         { link = "Comment" })  -- ## documentation comments
  highlight(0, 'nimMultilineComment',   { link = "Comment" })  -- #[ ]# multiline comments
  highlight(0, 'nimTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Special
  highlight(0, 'nimAsm',                { fg = colors.pink,       bg = 'NONE'            })  -- asm blocks
  highlight(0, 'nimCast',               { fg = colors.blue,       bg = 'NONE'            })  -- cast
  highlight(0, 'nimAddr',               { fg = colors.blue,       bg = 'NONE'            })  -- addr
  highlight(0, 'nimYield',              { fg = colors.blue,       bg = 'NONE'            })  -- yield (in iterators)
  highlight(0, 'nimResult',             { fg = colors.blue,       bg = 'NONE'            })  -- result (implicit return variable)
  highlight(0, 'nimDiscard',            { fg = colors.blue,       bg = 'NONE'            })  -- discard
  highlight(0, 'nimStatic',             { fg = colors.blue,       bg = 'NONE'            })  -- static

  -- Error/Special
  highlight(0, 'nimError',              { fg = colors.red,        bg = 'NONE'            })  -- Syntax errors
  highlight(0, 'nimSpaceError',         { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Indentation errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.nim)

  -- Variables
  highlight(0, '@variable.nim',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.nim',      { link = "Variable" })  -- result, self
  highlight(0, '@variable.parameter.nim',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.nim',       { link = "Variable" })  -- Object fields

  -- Constants
  highlight(0, '@constant.nim',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.nim',      { link = "Constant" })  -- true, false, nil

  -- Functions
  highlight(0, '@function.nim',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.nim',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.nim',      { link = "Function" })  -- Built-in functions (echo, len, etc.)
  highlight(0, '@function.macro.nim',        { link = "Function" })  -- Macro definitions
  highlight(0, '@function.method.nim',       { link = "Function" })  -- Method definitions
  highlight(0, '@method.nim',                { link = "Function" })  -- Methods
  highlight(0, '@method.call.nim',           { link = "Function" })  -- Method calls
  highlight(0, '@constructor.nim',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructors

  -- Types
  highlight(0, '@type.nim',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.nim',          { link = "Type" })  -- int, string, float, bool, etc.
  highlight(0, '@type.definition.nim',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.nim',        { link = "Type" })  -- ref, ptr, var, out
  highlight(0, '@parameter.nim',             { fg = colors.purple,    bg = 'NONE' })  -- Parameters

  -- Keywords
  highlight(0, '@keyword.nim',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.nim',      { link = "Keyword" })  -- proc, func, method, iterator, template, macro, converter
  highlight(0, '@keyword.type.nim',          { link = "Keyword" })  -- type, object, enum, tuple, distinct, concept
  highlight(0, '@keyword.modifier.nim',      { link = "Keyword" })  -- let, var, const
  highlight(0, '@keyword.return.nim',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.repeat.nim',        { link = "Keyword" })  -- for, while
  highlight(0, '@keyword.conditional.nim',   { link = "Conditional" })  -- if, elif, else, when, case, of
  highlight(0, '@keyword.operator.nim',      { link = "Operator" })  -- and, or, not, xor, in, notin, is, isnot, div, mod, shl, shr
  highlight(0, '@keyword.exception.nim',     { link = "Keyword" })  -- try, except, finally, raise
  highlight(0, '@keyword.import.nim',        { link = "Keyword" })  -- import, include, from, export

  -- Includes
  highlight(0, '@include.nim',               { fg = colors.pink,      bg = 'NONE' })  -- import, include, from, export

  -- Conditionals and Exceptions
  highlight(0, '@conditional.nim',           { link = "Conditional" })  -- if, elif, else, when, case, of
  highlight(0, '@repeat.nim',                { fg = colors.blue,      bg = 'NONE' })  -- for, while
  highlight(0, '@exception.nim',             { fg = colors.blue,      bg = 'NONE' })  -- try, except, finally, raise

  -- Strings
  highlight(0, '@string.nim',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.nim',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.nim',        { link = "String" })  -- Format specifiers
  highlight(0, '@character.nim',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.nim',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.nim',          { link = "Number" })  -- Floats
  highlight(0, '@float.nim',                 { fg = colors.greenLight, bg = 'NONE' })  -- Floats (alternative)

  -- Booleans
  highlight(0, '@boolean.nim',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.nim',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.nim', { link = "Comment" })  -- ## doc comments

  -- Labels and Fields
  highlight(0, '@label.nim',                 { fg = colors.blue,      bg = 'NONE' })  -- Block labels
  highlight(0, '@field.nim',                 { fg = colors.purple,    bg = 'NONE' })  -- Object fields
  highlight(0, '@property.nim',              { fg = colors.purple,    bg = 'NONE' })  -- Object properties

  -- Attributes (Pragmas)
  highlight(0, '@attribute.nim',             { fg = colors.pink,      bg = 'NONE' })  -- {.pragma.}

  -- Operators and Punctuation
  highlight(0, '@operator.nim',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.nim',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.nim', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.nim',   { fg = colors.pink,      bg = 'NONE' })  -- {. .} pragma brackets


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.nim)

  highlight(0, '@lsp.type.variable.nim',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.nim',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.nim',      { fg = colors.purple,    bg = 'NONE' })  -- Object fields
  highlight(0, '@lsp.type.function.nim',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.nim',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.macro.nim',         { fg = colors.pink,      bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.type.nim',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.nim',         { fg = colors.turquoise, bg = 'NONE' })  -- Objects (classes)
  highlight(0, '@lsp.type.enum.nim',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.nim',    { fg = colors.purple,    bg = 'NONE' })  -- Enum values
  highlight(0, '@lsp.type.interface.nim',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.namespace.nim',     { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.typeParameter.nim', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type parameters
  highlight(0, '@lsp.type.keyword.nim',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.nim',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.nim',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.nim',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.nim',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.nim',       { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.decorator.nim',     { fg = colors.pink,      bg = 'NONE' })  -- Pragmas

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.nim',    { link = "Variable" })  -- let/const variables
  highlight(0, '@lsp.typemod.variable.declaration.nim', { link = "Variable" })  -- Variable declarations
  highlight(0, '@lsp.typemod.function.declaration.nim', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.definition.nim',  { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@lsp.typemod.type.declaration.nim',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.nim',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@lsp.typemod.macro.declaration.nim',    { fg = colors.pink,      bg = 'NONE' })  -- Macro declarations
end

return nim
