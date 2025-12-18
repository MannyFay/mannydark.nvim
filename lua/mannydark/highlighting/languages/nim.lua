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
  highlight(0, 'nimKeyword',            { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'nimStatement',          { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, raise, discard
  highlight(0, 'nimConditional',        { fg = colors.blue,       bg = 'NONE'            })  -- if, elif, else, when, case, of
  highlight(0, 'nimRepeat',             { fg = colors.blue,       bg = 'NONE'            })  -- for, while, block
  highlight(0, 'nimLabel',              { fg = colors.blue,       bg = 'NONE'            })  -- block labels
  highlight(0, 'nimOperator',           { fg = colors.blue,       bg = 'NONE'            })  -- and, or, not, xor, in, notin, is, isnot, div, mod, shl, shr
  highlight(0, 'nimException',          { fg = colors.blue,       bg = 'NONE'            })  -- try, except, finally, raise
  highlight(0, 'nimInclude',            { fg = colors.pink,       bg = 'NONE'            })  -- import, include, from, export
  highlight(0, 'nimDefer',              { fg = colors.blue,       bg = 'NONE'            })  -- defer

  -- Storage/Variable Declaration
  highlight(0, 'nimLet',                { fg = colors.blue,       bg = 'NONE'            })  -- let (immutable runtime)
  highlight(0, 'nimVar',                { fg = colors.blue,       bg = 'NONE'            })  -- var (mutable)
  highlight(0, 'nimConst',              { fg = colors.blue,       bg = 'NONE'            })  -- const (compile-time constant)
  highlight(0, 'nimUsing',              { fg = colors.blue,       bg = 'NONE'            })  -- using

  -- Routine Types
  highlight(0, 'nimProc',               { fg = colors.blue,       bg = 'NONE'            })  -- proc keyword
  highlight(0, 'nimFunc',               { fg = colors.blue,       bg = 'NONE'            })  -- func (noSideEffect proc)
  highlight(0, 'nimMethod',             { fg = colors.blue,       bg = 'NONE'            })  -- method (dynamic dispatch)
  highlight(0, 'nimIterator',           { fg = colors.blue,       bg = 'NONE'            })  -- iterator
  highlight(0, 'nimConverter',          { fg = colors.blue,       bg = 'NONE'            })  -- converter
  highlight(0, 'nimTemplate',           { fg = colors.blue,       bg = 'NONE'            })  -- template
  highlight(0, 'nimMacro',              { fg = colors.pink,       bg = 'NONE'            })  -- macro

  -- Type Definitions
  highlight(0, 'nimTypedef',            { fg = colors.blue,       bg = 'NONE'            })  -- type keyword
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
  highlight(0, 'nimType',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'nimBuiltinType',        { fg = colors.turquoise,  bg = 'NONE'            })  -- int, float, string, bool, char, seq, array, etc.
  highlight(0, 'nimGenericType',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic type parameters [T]

  -- Functions
  highlight(0, 'nimFunction',           { fg = colors.orange,     bg = 'NONE'            })  -- Function/proc names
  highlight(0, 'nimFunctionCall',       { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'nimBuiltinFunc',        { fg = colors.orange,     bg = 'NONE'            })  -- echo, len, high, low, sizeof, typeof, etc.

  -- Variables
  highlight(0, 'nimIdentifier',         { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'nimParameter',          { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'nimField',              { fg = colors.purple,     bg = 'NONE'            })  -- Object fields

  -- Constants
  highlight(0, 'nimConstant',           { fg = colors.purple,     bg = 'NONE'            })  -- User constants
  highlight(0, 'nimBuiltinConstant',    { fg = colors.blue,       bg = 'NONE'            })  -- true, false, nil
  highlight(0, 'nimBoolean',            { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'nimNil',                { fg = colors.blue,       bg = 'NONE'            })  -- nil

  -- Strings
  highlight(0, 'nimString',             { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'nimRawString',          { fg = colors.redLight,   bg = 'NONE'            })  -- r"raw strings"
  highlight(0, 'nimTripleString',       { fg = colors.redLight,   bg = 'NONE'            })  -- """triple quoted strings"""
  highlight(0, 'nimCharacter',          { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'nimStringEscape',       { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \x00, etc.
  highlight(0, 'nimStringFormat',       { fg = colors.pink,       bg = 'NONE'            })  -- fmt string interpolation {expr}

  -- Numbers
  highlight(0, 'nimNumber',             { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'nimInteger',            { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'nimFloat',              { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'nimHex',                { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'nimOctal',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0o77 octal
  highlight(0, 'nimBinary',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary

  -- Operators
  highlight(0, 'nimOperatorSign',       { fg = colors.white,      bg = 'NONE'            })  -- + - * / = < > @ $ ~ & % ! ? ^ . : |
  highlight(0, 'nimDotOperator',        { fg = colors.white,      bg = 'NONE'            })  -- . field access
  highlight(0, 'nimRangeOperator',      { fg = colors.white,      bg = 'NONE'            })  -- .. range
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
  highlight(0, 'nimComment',            { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'nimDocComment',         { fg = colors.red,        bg = 'NONE'            })  -- ## documentation comments
  highlight(0, 'nimMultilineComment',   { fg = colors.red,        bg = 'NONE'            })  -- #[ ]# multiline comments
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
  highlight(0, '@variable.nim',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.nim',      { fg = colors.blue,      bg = 'NONE' })  -- result, self
  highlight(0, '@variable.parameter.nim',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.nim',       { fg = colors.purple,    bg = 'NONE' })  -- Object fields

  -- Constants
  highlight(0, '@constant.nim',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.nim',      { fg = colors.blue,      bg = 'NONE' })  -- true, false, nil

  -- Functions
  highlight(0, '@function.nim',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.nim',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.nim',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions (echo, len, etc.)
  highlight(0, '@function.macro.nim',        { fg = colors.pink,      bg = 'NONE' })  -- Macro definitions
  highlight(0, '@function.method.nim',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@method.nim',                { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@method.call.nim',           { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@constructor.nim',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructors

  -- Types
  highlight(0, '@type.nim',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.nim',          { fg = colors.turquoise, bg = 'NONE' })  -- int, string, float, bool, etc.
  highlight(0, '@type.definition.nim',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.nim',        { fg = colors.blue,      bg = 'NONE' })  -- ref, ptr, var, out
  highlight(0, '@parameter.nim',             { fg = colors.purple,    bg = 'NONE' })  -- Parameters

  -- Keywords
  highlight(0, '@keyword.nim',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.nim',      { fg = colors.blue,      bg = 'NONE' })  -- proc, func, method, iterator, template, macro, converter
  highlight(0, '@keyword.type.nim',          { fg = colors.blue,      bg = 'NONE' })  -- type, object, enum, tuple, distinct, concept
  highlight(0, '@keyword.modifier.nim',      { fg = colors.blue,      bg = 'NONE' })  -- let, var, const
  highlight(0, '@keyword.return.nim',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.nim',        { fg = colors.blue,      bg = 'NONE' })  -- for, while
  highlight(0, '@keyword.conditional.nim',   { fg = colors.blue,      bg = 'NONE' })  -- if, elif, else, when, case, of
  highlight(0, '@keyword.operator.nim',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not, xor, in, notin, is, isnot, div, mod, shl, shr
  highlight(0, '@keyword.exception.nim',     { fg = colors.blue,      bg = 'NONE' })  -- try, except, finally, raise
  highlight(0, '@keyword.import.nim',        { fg = colors.pink,      bg = 'NONE' })  -- import, include, from, export

  -- Includes
  highlight(0, '@include.nim',               { fg = colors.pink,      bg = 'NONE' })  -- import, include, from, export

  -- Conditionals and Exceptions
  highlight(0, '@conditional.nim',           { fg = colors.blue,      bg = 'NONE' })  -- if, elif, else, when, case, of
  highlight(0, '@repeat.nim',                { fg = colors.blue,      bg = 'NONE' })  -- for, while
  highlight(0, '@exception.nim',             { fg = colors.blue,      bg = 'NONE' })  -- try, except, finally, raise

  -- Strings
  highlight(0, '@string.nim',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.nim',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.nim',        { fg = colors.pink,      bg = 'NONE' })  -- Format specifiers
  highlight(0, '@character.nim',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.nim',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.nim',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, '@float.nim',                 { fg = colors.greenLight, bg = 'NONE' })  -- Floats (alternative)

  -- Booleans
  highlight(0, '@boolean.nim',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.nim',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.nim', { fg = colors.red,       bg = 'NONE' })  -- ## doc comments

  -- Labels and Fields
  highlight(0, '@label.nim',                 { fg = colors.blue,      bg = 'NONE' })  -- Block labels
  highlight(0, '@field.nim',                 { fg = colors.purple,    bg = 'NONE' })  -- Object fields
  highlight(0, '@property.nim',              { fg = colors.purple,    bg = 'NONE' })  -- Object properties

  -- Attributes (Pragmas)
  highlight(0, '@attribute.nim',             { fg = colors.pink,      bg = 'NONE' })  -- {.pragma.}

  -- Operators and Punctuation
  highlight(0, '@operator.nim',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.nim',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.nim', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.nim',   { fg = colors.pink,      bg = 'NONE' })  -- {. .} pragma brackets


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.nim)

  highlight(0, '@lsp.type.variable.nim',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
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
  highlight(0, '@lsp.type.keyword.nim',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.nim',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.nim',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.nim',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.nim',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.nim',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.decorator.nim',     { fg = colors.pink,      bg = 'NONE' })  -- Pragmas

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.nim',    { fg = colors.purple,    bg = 'NONE' })  -- let/const variables
  highlight(0, '@lsp.typemod.variable.declaration.nim', { fg = colors.purple,    bg = 'NONE' })  -- Variable declarations
  highlight(0, '@lsp.typemod.function.declaration.nim', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.definition.nim',  { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@lsp.typemod.type.declaration.nim',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.nim',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@lsp.typemod.macro.declaration.nim',    { fg = colors.pink,      bg = 'NONE' })  -- Macro declarations
end

return nim
