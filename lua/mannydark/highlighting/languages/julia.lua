-------------------------------------------------------------------------------
-- Julia Language Files
-- Highlighting for .jl files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local julia     = {}


-------------------------------------------------------------------------------
-- Settings

julia.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords / Control Flow
  highlight(0, 'juliaKeyword',          { fg = colors.blue,       bg = 'NONE' })  -- General keywords
  highlight(0, 'juliaConditional',      { fg = colors.blue,       bg = 'NONE' })  -- if, elseif, else
  highlight(0, 'juliaRepeat',           { fg = colors.blue,       bg = 'NONE' })  -- for, while
  highlight(0, 'juliaRepKeyword',       { fg = colors.blue,       bg = 'NONE' })  -- in
  highlight(0, 'juliaException',        { fg = colors.blue,       bg = 'NONE' })  -- try, catch, finally
  highlight(0, 'juliaOuter',            { fg = colors.blue,       bg = 'NONE' })  -- outer keyword
  highlight(0, 'juliaBaseTypeBasic',    { fg = colors.blue,       bg = 'NONE' })  -- begin, end, return

  -- Block Keywords
  highlight(0, 'juliaBlKeyword',        { fg = colors.blue,       bg = 'NONE' })  -- begin, do, end
  highlight(0, 'juliaComprehensionFor', { fg = colors.blue,       bg = 'NONE' })  -- for in comprehensions
  highlight(0, 'juliaQuote',            { fg = colors.blue,       bg = 'NONE' })  -- quote

  -- Scope Modifiers
  highlight(0, 'juliaDeclKeyword',      { fg = colors.blue,       bg = 'NONE' })  -- local, global, const

  -- Import / Export
  highlight(0, 'juliaImportKeyword',    { fg = colors.blue,       bg = 'NONE' })  -- import, using, export, public
  highlight(0, 'juliaAsKeyword',        { fg = colors.blue,       bg = 'NONE' })  -- as (import renaming)

  -- Function / Type Definitions
  highlight(0, 'juliaFunctionKeyword',  { fg = colors.blue,       bg = 'NONE' })  -- function
  highlight(0, 'juliaMacroKeyword',     { fg = colors.blue,       bg = 'NONE' })  -- macro
  highlight(0, 'juliaStructKeyword',    { fg = colors.blue,       bg = 'NONE' })  -- struct, mutable struct
  highlight(0, 'juliaTypeKeyword',      { fg = colors.blue,       bg = 'NONE' })  -- abstract type, primitive type
  highlight(0, 'juliaModuleKeyword',    { fg = colors.blue,       bg = 'NONE' })  -- module, baremodule
  highlight(0, 'juliaWhereKeyword',     { fg = colors.blue,       bg = 'NONE' })  -- where

  -- Type Operators
  highlight(0, 'juliaIsaKeyword',       { fg = colors.blue,       bg = 'NONE' })  -- isa
  highlight(0, 'juliaTypeOperator',     { fg = colors.blue,       bg = 'NONE' })  -- <: >: (subtype/supertype)

  -- Constants
  highlight(0, 'juliaConstant',         { fg = colors.blue,       bg = 'NONE' })  -- true, false, nothing, missing
  highlight(0, 'juliaConstNum',         { fg = colors.blue,       bg = 'NONE' })  -- NaN, Inf, pi, ℯ
  highlight(0, 'juliaConstEnv',         { fg = colors.blue,       bg = 'NONE' })  -- ARGS, ENV, VERSION, etc.
  highlight(0, 'juliaConstIO',          { fg = colors.blue,       bg = 'NONE' })  -- stdout, stdin, stderr, devnull
  highlight(0, 'juliaConstGeneric',     { fg = colors.blue,       bg = 'NONE' })  -- nothing, undef, missing
  highlight(0, 'juliaBool',             { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Base Types
  highlight(0, 'juliaBaseTypeNum',      { fg = colors.turquoise,  bg = 'NONE' })  -- Int, Float64, Complex, etc.
  highlight(0, 'juliaBaseTypeC',        { fg = colors.turquoise,  bg = 'NONE' })  -- Cint, Clong, Cfloat, etc.
  highlight(0, 'juliaBaseTypeError',    { fg = colors.turquoise,  bg = 'NONE' })  -- Exception types
  highlight(0, 'juliaBaseTypeIter',     { fg = colors.turquoise,  bg = 'NONE' })  -- Iterator types
  highlight(0, 'juliaBaseTypeString',   { fg = colors.turquoise,  bg = 'NONE' })  -- String, SubString, etc.
  highlight(0, 'juliaBaseTypeArray',    { fg = colors.turquoise,  bg = 'NONE' })  -- Array, Vector, Matrix, Dict
  highlight(0, 'juliaBaseTypeRange',    { fg = colors.turquoise,  bg = 'NONE' })  -- Range types
  highlight(0, 'juliaBaseTypeIO',       { fg = colors.turquoise,  bg = 'NONE' })  -- IO types
  highlight(0, 'juliaBaseTypeCast',     { fg = colors.turquoise,  bg = 'NONE' })  -- Type casting types

  -- Type
  highlight(0, 'juliaType',             { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, 'juliaTypeDef',          { fg = colors.turquoise,  bg = 'NONE' })  -- Type definitions
  highlight(0, 'juliaTypeParameter',    { fg = colors.turquoise,  bg = 'NONE' })  -- Type parameters

  -- Functions
  highlight(0, 'juliaFunctionName',     { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, 'juliaFunctionCall',     { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, 'juliaBuiltinFunction',  { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Macros
  highlight(0, 'juliaMacro',            { fg = colors.pink,       bg = 'NONE' })  -- @macro_name
  highlight(0, 'juliaMacroName',        { fg = colors.pink,       bg = 'NONE' })  -- Macro name after @
  highlight(0, 'juliaMacroCall',        { fg = colors.pink,       bg = 'NONE' })  -- Macro calls
  highlight(0, 'juliaDollarVar',        { fg = colors.pink,       bg = 'NONE' })  -- $variable interpolation
  highlight(0, 'juliaStringPrefixMacro',{ fg = colors.pink,       bg = 'NONE' })  -- String prefix macros (r"", b"")

  -- Operators
  highlight(0, 'juliaOperator',         { fg = colors.white,      bg = 'NONE' })  -- +, -, *, /, etc.
  highlight(0, 'juliaArithOperator',    { fg = colors.white,      bg = 'NONE' })  -- Arithmetic operators
  highlight(0, 'juliaBitOperator',      { fg = colors.white,      bg = 'NONE' })  -- Bitwise operators
  highlight(0, 'juliaBoolOperator',     { fg = colors.white,      bg = 'NONE' })  -- &&, ||, !
  highlight(0, 'juliaCompOperator',     { fg = colors.white,      bg = 'NONE' })  -- ==, !=, <, >, <=, >=
  highlight(0, 'juliaAssignOperator',   { fg = colors.white,      bg = 'NONE' })  -- =, +=, -=, etc.
  highlight(0, 'juliaRangeOperator',    { fg = colors.white,      bg = 'NONE' })  -- : range operator
  highlight(0, 'juliaTernaryOperator',  { fg = colors.white,      bg = 'NONE' })  -- ? :
  highlight(0, 'juliaPipeOperator',     { fg = colors.blue,       bg = 'NONE' })  -- |>
  highlight(0, 'juliaComposeOperator',  { fg = colors.blue,       bg = 'NONE' })  -- ∘
  highlight(0, 'juliaBroadcastOp',      { fg = colors.white,      bg = 'NONE' })  -- .+, .-, .*, etc.
  highlight(0, 'juliaTransposeOp',      { fg = colors.white,      bg = 'NONE' })  -- ' transpose
  highlight(0, 'juliaSplatOp',          { fg = colors.white,      bg = 'NONE' })  -- ... splat

  -- Unicode Operators
  highlight(0, 'juliaUnicodeOp',        { fg = colors.white,      bg = 'NONE' })  -- ∈, ∉, ⊆, ∩, ∪, etc.
  highlight(0, 'juliaArrowOp',          { fg = colors.white,      bg = 'NONE' })  -- →, ←, ↔, etc.
  highlight(0, 'juliaComparisonOp',     { fg = colors.white,      bg = 'NONE' })  -- ≠, ≤, ≥, ≡, ≢

  -- Variables / Parameters
  highlight(0, 'juliaVariable',         { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, 'juliaParameter',        { fg = colors.purple,     bg = 'NONE' })  -- Function parameters
  highlight(0, 'juliaField',            { fg = colors.purple,     bg = 'NONE' })  -- Struct fields
  highlight(0, 'juliaSymbol',           { fg = colors.purple,     bg = 'NONE' })  -- :symbol

  -- Strings
  highlight(0, 'juliaString',           { fg = colors.redLight,   bg = 'NONE' })  -- "string"
  highlight(0, 'juliaStringDelim',      { fg = colors.redLight,   bg = 'NONE' })  -- String delimiters
  highlight(0, 'juliaTripleString',     { fg = colors.redLight,   bg = 'NONE' })  -- """multiline"""
  highlight(0, 'juliaRawString',        { fg = colors.redLight,   bg = 'NONE' })  -- raw"string"
  highlight(0, 'juliaByteString',       { fg = colors.redLight,   bg = 'NONE' })  -- b"bytes"
  highlight(0, 'juliaVersionString',    { fg = colors.redLight,   bg = 'NONE' })  -- v"1.0.0"
  highlight(0, 'juliaRegexString',      { fg = colors.redLight,   bg = 'NONE' })  -- r"regex"
  highlight(0, 'juliaSubstitutionString', { fg = colors.redLight, bg = 'NONE' })  -- s"substitution"
  highlight(0, 'juliaShellString',      { fg = colors.redLight,   bg = 'NONE' })  -- `shell command`
  highlight(0, 'juliaDocString',        { fg = colors.redLight,   bg = 'NONE' })  -- Docstrings
  highlight(0, 'juliaStringEscape',     { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, etc.
  highlight(0, 'juliaStringInterp',     { fg = colors.pink,       bg = 'NONE' })  -- $var, $(expr)
  highlight(0, 'juliaStringInterpDelim', { fg = colors.pink,      bg = 'NONE' })  -- $ in interpolation

  -- Characters
  highlight(0, 'juliaChar',             { fg = colors.redLight,   bg = 'NONE' })  -- 'c'
  highlight(0, 'juliaCharDelim',        { fg = colors.redLight,   bg = 'NONE' })  -- Character delimiters

  -- Numbers
  highlight(0, 'juliaNumber',           { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'juliaFloat',            { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'juliaComplexNum',       { fg = colors.greenLight, bg = 'NONE' })  -- 1+2im
  highlight(0, 'juliaHexNum',           { fg = colors.greenLight, bg = 'NONE' })  -- 0x...
  highlight(0, 'juliaBinNum',           { fg = colors.greenLight, bg = 'NONE' })  -- 0b...
  highlight(0, 'juliaOctNum',           { fg = colors.greenLight, bg = 'NONE' })  -- 0o...
  highlight(0, 'juliaImagUnit',         { fg = colors.greenLight, bg = 'NONE' })  -- im suffix

  -- Comments
  highlight(0, 'juliaComment',          { fg = colors.red,        bg = 'NONE' })  -- # comment
  highlight(0, 'juliaBlockComment',     { fg = colors.red,        bg = 'NONE' })  -- #= comment =#
  highlight(0, 'juliaCommentTodo',      { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.
  highlight(0, 'juliaDocComment',       { fg = colors.red,        bg = 'NONE' })  -- Documentation comments

  -- Delimiters / Punctuation
  highlight(0, 'juliaParDelim',         { fg = colors.white,      bg = 'NONE' })  -- ()
  highlight(0, 'juliaSqBraDelim',       { fg = colors.white,      bg = 'NONE' })  -- []
  highlight(0, 'juliaCurBraDelim',      { fg = colors.white,      bg = 'NONE' })  -- {}
  highlight(0, 'juliaComma',            { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'juliaSemicolon',        { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'juliaColon',            { fg = colors.white,      bg = 'NONE' })  -- : (not range)
  highlight(0, 'juliaDot',              { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'juliaDoubleColon',      { fg = colors.white,      bg = 'NONE' })  -- :: type annotation

  -- Errors
  highlight(0, 'juliaError',            { fg = colors.red,        bg = 'NONE' })  -- Syntax errors
  highlight(0, 'juliaParseError',       { fg = colors.red,        bg = 'NONE' })  -- Parse errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.julia)

  -- Variables
  highlight(0, '@variable.julia',              { fg = colors.white,     bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.julia',      { fg = colors.blue,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.member.julia',       { fg = colors.purple,    bg = 'NONE' })  -- Struct members
  highlight(0, '@variable.parameter.julia',    { fg = colors.purple,    bg = 'NONE' })  -- Parameters

  -- Constants
  highlight(0, '@constant.julia',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.julia',      { fg = colors.blue,      bg = 'NONE' })  -- nothing, missing, true, false

  -- Functions
  highlight(0, '@function.julia',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.julia',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.julia',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.method.julia',       { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@function.method.call.julia',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.macro.julia',        { fg = colors.pink,      bg = 'NONE' })  -- Macros

  -- Types
  highlight(0, '@type.julia',                  { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@type.builtin.julia',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.julia',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.julia',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.julia',      { fg = colors.blue,      bg = 'NONE' })  -- function, macro
  highlight(0, '@keyword.return.julia',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.conditional.julia',   { fg = colors.blue,      bg = 'NONE' })  -- if, elseif, else
  highlight(0, '@keyword.conditional.ternary.julia', { fg = colors.white, bg = 'NONE' })  -- ? :
  highlight(0, '@keyword.repeat.julia',        { fg = colors.blue,      bg = 'NONE' })  -- for, while
  highlight(0, '@keyword.exception.julia',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally
  highlight(0, '@keyword.modifier.julia',      { fg = colors.blue,      bg = 'NONE' })  -- const, local, global
  highlight(0, '@keyword.import.julia',        { fg = colors.blue,      bg = 'NONE' })  -- import, using, export
  highlight(0, '@keyword.type.julia',          { fg = colors.blue,      bg = 'NONE' })  -- struct, abstract type
  highlight(0, '@keyword.operator.julia',      { fg = colors.blue,      bg = 'NONE' })  -- in, isa, where

  -- Strings
  highlight(0, '@string.julia',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.documentation.julia',  { fg = colors.redLight,  bg = 'NONE' })  -- Docstrings
  highlight(0, '@string.escape.julia',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.julia',        { fg = colors.redLight,  bg = 'NONE' })  -- Command strings
  highlight(0, '@string.special.symbol.julia', { fg = colors.purple,    bg = 'NONE' })  -- :symbol
  highlight(0, '@string.regexp.julia',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex

  -- Numbers
  highlight(0, '@number.julia',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.julia',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.julia',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Characters
  highlight(0, '@character.julia',             { fg = colors.redLight,  bg = 'NONE' })  -- Characters

  -- Comments
  highlight(0, '@comment.julia',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.julia', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Operators
  highlight(0, '@operator.julia',              { fg = colors.white,     bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.julia',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.julia', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.julia',   { fg = colors.pink,      bg = 'NONE' })  -- $, @ interpolation

  -- Module
  highlight(0, '@module.julia',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names

  -- Spell
  highlight(0, '@spell.julia',                 { fg = colors.red,       bg = 'NONE' })  -- Spellchecked


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.julia)

  highlight(0, '@lsp.type.variable.julia',      { fg = colors.white,     bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.julia',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.julia',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.julia',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.julia',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.macro.julia',         { fg = colors.pink,      bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.class.julia',         { fg = colors.turquoise, bg = 'NONE' })  -- Types/Structs
  highlight(0, '@lsp.type.struct.julia',        { fg = colors.turquoise, bg = 'NONE' })  -- Structs
  highlight(0, '@lsp.type.type.julia',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.julia', { fg = colors.turquoise, bg = 'NONE' })  -- Type parameters
  highlight(0, '@lsp.type.namespace.julia',     { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.enum.julia',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.julia',    { fg = colors.purple,    bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.keyword.julia',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.julia',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.comment.julia',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.string.julia',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.julia',        { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.julia',  { fg = colors.purple,   bg = 'NONE' })  -- const
  highlight(0, '@lsp.typemod.function.builtin.julia',   { fg = colors.orange,   bg = 'NONE' })  -- Built-ins
  highlight(0, '@lsp.typemod.type.defaultLibrary.julia', { fg = colors.turquoise, bg = 'NONE' })  -- Base types


  -----------------------------------------------------------------------------
  -- Base Types (Custom highlight groups)

  -- Numeric Types
  highlight(0, 'juliaTypeInt',          { fg = colors.turquoise,  bg = 'NONE' })  -- Int8-Int128, UInt8-UInt128
  highlight(0, 'juliaTypeFloat',        { fg = colors.turquoise,  bg = 'NONE' })  -- Float16, Float32, Float64
  highlight(0, 'juliaTypeComplex',      { fg = colors.turquoise,  bg = 'NONE' })  -- Complex{T}
  highlight(0, 'juliaTypeRational',     { fg = colors.turquoise,  bg = 'NONE' })  -- Rational{T}
  highlight(0, 'juliaTypeBigNum',       { fg = colors.turquoise,  bg = 'NONE' })  -- BigInt, BigFloat

  -- Abstract Numeric Types
  highlight(0, 'juliaTypeNumber',       { fg = colors.turquoise,  bg = 'NONE' })  -- Number, Real, Integer, etc.
  highlight(0, 'juliaTypeSigned',       { fg = colors.turquoise,  bg = 'NONE' })  -- Signed, Unsigned
  highlight(0, 'juliaTypeAbstractFloat', { fg = colors.turquoise, bg = 'NONE' })  -- AbstractFloat

  -- Collection Types
  highlight(0, 'juliaTypeArray',        { fg = colors.turquoise,  bg = 'NONE' })  -- Array, Vector, Matrix
  highlight(0, 'juliaTypeDict',         { fg = colors.turquoise,  bg = 'NONE' })  -- Dict, IdDict, WeakKeyDict
  highlight(0, 'juliaTypeSet',          { fg = colors.turquoise,  bg = 'NONE' })  -- Set, BitSet
  highlight(0, 'juliaTypeTuple',        { fg = colors.turquoise,  bg = 'NONE' })  -- Tuple, NamedTuple, NTuple

  -- String Types
  highlight(0, 'juliaTypeString',       { fg = colors.turquoise,  bg = 'NONE' })  -- String, SubString, AbstractString
  highlight(0, 'juliaTypeChar',         { fg = colors.turquoise,  bg = 'NONE' })  -- Char, AbstractChar
  highlight(0, 'juliaTypeRegex',        { fg = colors.turquoise,  bg = 'NONE' })  -- Regex, RegexMatch

  -- I/O Types
  highlight(0, 'juliaTypeIO',           { fg = colors.turquoise,  bg = 'NONE' })  -- IO, IOStream, IOBuffer
  highlight(0, 'juliaTypeFile',         { fg = colors.turquoise,  bg = 'NONE' })  -- File types

  -- Special Types
  highlight(0, 'juliaTypeNothing',      { fg = colors.turquoise,  bg = 'NONE' })  -- Nothing
  highlight(0, 'juliaTypeMissing',      { fg = colors.turquoise,  bg = 'NONE' })  -- Missing
  highlight(0, 'juliaTypeSome',         { fg = colors.turquoise,  bg = 'NONE' })  -- Some{T}
  highlight(0, 'juliaTypeUnion',        { fg = colors.turquoise,  bg = 'NONE' })  -- Union, UnionAll

  -- Fundamental Types
  highlight(0, 'juliaTypeAny',          { fg = colors.turquoise,  bg = 'NONE' })  -- Any
  highlight(0, 'juliaTypeDataType',     { fg = colors.turquoise,  bg = 'NONE' })  -- DataType, Type
  highlight(0, 'juliaTypeFunction',     { fg = colors.turquoise,  bg = 'NONE' })  -- Function
  highlight(0, 'juliaTypeModule',       { fg = colors.turquoise,  bg = 'NONE' })  -- Module
  highlight(0, 'juliaTypeExpr',         { fg = colors.turquoise,  bg = 'NONE' })  -- Expr, Symbol
  highlight(0, 'juliaTypePtr',          { fg = colors.turquoise,  bg = 'NONE' })  -- Ptr, Ref

  -- Concurrency Types
  highlight(0, 'juliaTypeTask',         { fg = colors.turquoise,  bg = 'NONE' })  -- Task
  highlight(0, 'juliaTypeChannel',      { fg = colors.turquoise,  bg = 'NONE' })  -- Channel
  highlight(0, 'juliaTypeLock',         { fg = colors.turquoise,  bg = 'NONE' })  -- ReentrantLock, SpinLock

  -- C Interop Types
  highlight(0, 'juliaTypeCint',         { fg = colors.turquoise,  bg = 'NONE' })  -- Cint, Clong, Cfloat, etc.
  highlight(0, 'juliaTypeCptr',         { fg = colors.turquoise,  bg = 'NONE' })  -- Cstring, Cwstring


  -----------------------------------------------------------------------------
  -- Exception Types

  highlight(0, 'juliaException',        { fg = colors.turquoise,  bg = 'NONE' })  -- Base exception type
  highlight(0, 'juliaExceptionArg',     { fg = colors.turquoise,  bg = 'NONE' })  -- ArgumentError
  highlight(0, 'juliaExceptionBounds',  { fg = colors.turquoise,  bg = 'NONE' })  -- BoundsError
  highlight(0, 'juliaExceptionDomain',  { fg = colors.turquoise,  bg = 'NONE' })  -- DomainError
  highlight(0, 'juliaExceptionType',    { fg = colors.turquoise,  bg = 'NONE' })  -- TypeError
  highlight(0, 'juliaExceptionMethod',  { fg = colors.turquoise,  bg = 'NONE' })  -- MethodError
  highlight(0, 'juliaExceptionKey',     { fg = colors.turquoise,  bg = 'NONE' })  -- KeyError
  highlight(0, 'juliaExceptionLoad',    { fg = colors.turquoise,  bg = 'NONE' })  -- LoadError
  highlight(0, 'juliaExceptionIO',      { fg = colors.turquoise,  bg = 'NONE' })  -- EOFError, SystemError


  -----------------------------------------------------------------------------
  -- Common Macros

  -- Core Macros
  highlight(0, 'juliaMacroShow',        { fg = colors.pink,       bg = 'NONE' })  -- @show
  highlight(0, 'juliaMacroAssert',      { fg = colors.pink,       bg = 'NONE' })  -- @assert
  highlight(0, 'juliaMacroDoc',         { fg = colors.pink,       bg = 'NONE' })  -- @doc

  -- Performance Macros
  highlight(0, 'juliaMacroTime',        { fg = colors.pink,       bg = 'NONE' })  -- @time, @timev, @timed, @elapsed
  highlight(0, 'juliaMacroAlloc',       { fg = colors.pink,       bg = 'NONE' })  -- @allocated, @allocations
  highlight(0, 'juliaMacroInline',      { fg = colors.pink,       bg = 'NONE' })  -- @inline, @noinline
  highlight(0, 'juliaMacroInbounds',    { fg = colors.pink,       bg = 'NONE' })  -- @inbounds, @boundscheck
  highlight(0, 'juliaMacroSimd',        { fg = colors.pink,       bg = 'NONE' })  -- @simd

  -- Metaprogramming Macros
  highlight(0, 'juliaMacroEval',        { fg = colors.pink,       bg = 'NONE' })  -- @eval
  highlight(0, 'juliaMacroGenerated',   { fg = colors.pink,       bg = 'NONE' })  -- @generated
  highlight(0, 'juliaMacroMacroexpand', { fg = colors.pink,       bg = 'NONE' })  -- @macroexpand
  highlight(0, 'juliaMacroGensym',      { fg = colors.pink,       bg = 'NONE' })  -- @gensym

  -- Type Macros
  highlight(0, 'juliaMacroEnum',        { fg = colors.pink,       bg = 'NONE' })  -- @enum
  highlight(0, 'juliaMacroKwdef',       { fg = colors.pink,       bg = 'NONE' })  -- @kwdef

  -- String Macros
  highlight(0, 'juliaMacroString',      { fg = colors.pink,       bg = 'NONE' })  -- @v_str, @r_str, etc.
  highlight(0, 'juliaMacroPrintf',      { fg = colors.pink,       bg = 'NONE' })  -- @printf, @sprintf

  -- Control Flow Macros
  highlight(0, 'juliaMacroGoto',        { fg = colors.pink,       bg = 'NONE' })  -- @goto, @label
  highlight(0, 'juliaMacroStatic',      { fg = colors.pink,       bg = 'NONE' })  -- @static

  -- Parallel Macros
  highlight(0, 'juliaMacroThreads',     { fg = colors.pink,       bg = 'NONE' })  -- @threads, @spawn, @sync
  highlight(0, 'juliaMacroDistributed', { fg = colors.pink,       bg = 'NONE' })  -- @distributed, @everywhere

  -- Testing Macros
  highlight(0, 'juliaMacroTest',        { fg = colors.pink,       bg = 'NONE' })  -- @test, @testset, @test_throws


  -----------------------------------------------------------------------------
  -- Built-in Functions

  -- Type/Reflection Functions
  highlight(0, 'juliaFuncType',         { fg = colors.orange,     bg = 'NONE' })  -- typeof, isa, supertype, etc.
  highlight(0, 'juliaFuncField',        { fg = colors.orange,     bg = 'NONE' })  -- fieldnames, getfield, setfield!

  -- Collection Functions
  highlight(0, 'juliaFuncArray',        { fg = colors.orange,     bg = 'NONE' })  -- push!, pop!, append!, etc.
  highlight(0, 'juliaFuncIteration',    { fg = colors.orange,     bg = 'NONE' })  -- map, filter, reduce, etc.
  highlight(0, 'juliaFuncSort',         { fg = colors.orange,     bg = 'NONE' })  -- sort, sort!, sortperm

  -- Math Functions
  highlight(0, 'juliaFuncMath',         { fg = colors.orange,     bg = 'NONE' })  -- sin, cos, exp, log, sqrt, etc.
  highlight(0, 'juliaFuncLinAlg',       { fg = colors.orange,     bg = 'NONE' })  -- dot, cross, norm, etc.

  -- I/O Functions
  highlight(0, 'juliaFuncIO',           { fg = colors.orange,     bg = 'NONE' })  -- print, println, read, write, etc.
  highlight(0, 'juliaFuncFile',         { fg = colors.orange,     bg = 'NONE' })  -- open, close, readlines, etc.

  -- String Functions
  highlight(0, 'juliaFuncString',       { fg = colors.orange,     bg = 'NONE' })  -- string, join, split, strip, etc.

  -- Error Handling
  highlight(0, 'juliaFuncError',        { fg = colors.orange,     bg = 'NONE' })  -- error, throw, rethrow

  -- System Functions
  highlight(0, 'juliaFuncSystem',       { fg = colors.orange,     bg = 'NONE' })  -- exit, run, cd, pwd, etc.


  -----------------------------------------------------------------------------
  -- Unicode Support

  -- Greek Letters (commonly used in Julia)
  highlight(0, 'juliaGreekLetter',      { fg = colors.white,      bg = 'NONE' })  -- α, β, γ, δ, etc.

  -- Mathematical Symbols
  highlight(0, 'juliaMathSymbol',       { fg = colors.white,      bg = 'NONE' })  -- ∈, ∉, ⊆, ⊇, etc.

  -- Arrows
  highlight(0, 'juliaArrowSymbol',      { fg = colors.white,      bg = 'NONE' })  -- →, ←, ↔, ⇒, etc.

  -- Special Constants
  highlight(0, 'juliaPiSymbol',         { fg = colors.blue,       bg = 'NONE' })  -- π
  highlight(0, 'juliaEulerSymbol',      { fg = colors.blue,       bg = 'NONE' })  -- ℯ


  -----------------------------------------------------------------------------
  -- Package-specific (Common packages)

  -- LinearAlgebra
  highlight(0, 'juliaLinAlgType',       { fg = colors.turquoise,  bg = 'NONE' })  -- Matrix types
  highlight(0, 'juliaLinAlgFunc',       { fg = colors.orange,     bg = 'NONE' })  -- LinearAlgebra functions

  -- Statistics / StatsBase
  highlight(0, 'juliaStatsFunc',        { fg = colors.orange,     bg = 'NONE' })  -- mean, std, var, etc.

  -- DataFrames
  highlight(0, 'juliaDataFrameType',    { fg = colors.turquoise,  bg = 'NONE' })  -- DataFrame, DataFrameRow
  highlight(0, 'juliaDataFrameFunc',    { fg = colors.orange,     bg = 'NONE' })  -- select, transform, etc.

  -- Plots / Makie
  highlight(0, 'juliaPlotFunc',         { fg = colors.orange,     bg = 'NONE' })  -- plot, scatter, heatmap, etc.


  -----------------------------------------------------------------------------
  -- Special Constructs

  -- Anonymous Functions
  highlight(0, 'juliaLambda',           { fg = colors.blue,       bg = 'NONE' })  -- -> in anonymous functions

  -- Broadcast Dot
  highlight(0, 'juliaBroadcastDot',     { fg = colors.white,      bg = 'NONE' })  -- f.() broadcast call

  -- Type Annotations
  highlight(0, 'juliaTypeAnnotation',   { fg = colors.turquoise,  bg = 'NONE' })  -- ::Type

  -- Parametric Types
  highlight(0, 'juliaParametricType',   { fg = colors.turquoise,  bg = 'NONE' })  -- Array{T,N}

  -- Comprehensions
  highlight(0, 'juliaComprehension',    { fg = colors.white,      bg = 'NONE' })  -- [x for x in xs]
  highlight(0, 'juliaGenerator',        { fg = colors.white,      bg = 'NONE' })  -- (x for x in xs)

  -- Do Block
  highlight(0, 'juliaDoBlock',          { fg = colors.blue,       bg = 'NONE' })  -- do keyword

  -- Multiple Dispatch
  highlight(0, 'juliaMethodSig',        { fg = colors.orange,     bg = 'NONE' })  -- Method signatures
end

return julia
