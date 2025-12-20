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
  highlight(0, 'juliaKeyword',          { link = "Keyword" })  -- General keywords
  highlight(0, 'juliaConditional',      { link = "Conditional" })  -- if, elseif, else
  highlight(0, 'juliaRepeat',           { fg = colors.blue,       bg = 'NONE' })  -- for, while
  highlight(0, 'juliaRepKeyword',       { link = "Keyword" })  -- in
  highlight(0, 'juliaException',        { fg = colors.blue,       bg = 'NONE' })  -- try, catch, finally
  highlight(0, 'juliaOuter',            { fg = colors.blue,       bg = 'NONE' })  -- outer keyword
  highlight(0, 'juliaBaseTypeBasic',    { link = "Type" })  -- begin, end, return

  -- Block Keywords
  highlight(0, 'juliaBlKeyword',        { link = "Keyword" })  -- begin, do, end
  highlight(0, 'juliaComprehensionFor', { fg = colors.blue,       bg = 'NONE' })  -- for in comprehensions
  highlight(0, 'juliaQuote',            { fg = colors.blue,       bg = 'NONE' })  -- quote

  -- Scope Modifiers
  highlight(0, 'juliaDeclKeyword',      { link = "Keyword" })  -- local, global, const

  -- Import / Export
  highlight(0, 'juliaImportKeyword',    { link = "Keyword" })  -- import, using, export, public
  highlight(0, 'juliaAsKeyword',        { link = "Keyword" })  -- as (import renaming)

  -- Function / Type Definitions
  highlight(0, 'juliaFunctionKeyword',  { link = "Keyword" })  -- function
  highlight(0, 'juliaMacroKeyword',     { link = "Keyword" })  -- macro
  highlight(0, 'juliaStructKeyword',    { link = "Keyword" })  -- struct, mutable struct
  highlight(0, 'juliaTypeKeyword',      { link = "Keyword" })  -- abstract type, primitive type
  highlight(0, 'juliaModuleKeyword',    { link = "Keyword" })  -- module, baremodule
  highlight(0, 'juliaWhereKeyword',     { link = "Keyword" })  -- where

  -- Type Operators
  highlight(0, 'juliaIsaKeyword',       { link = "Keyword" })  -- isa
  highlight(0, 'juliaTypeOperator',     { link = "Operator" })  -- <: >: (subtype/supertype)

  -- Constants
  highlight(0, 'juliaConstant',         { link = "Constant" })  -- true, false, nothing, missing
  highlight(0, 'juliaConstNum',         { link = "Constant" })  -- NaN, Inf, pi, ℯ
  highlight(0, 'juliaConstEnv',         { link = "Constant" })  -- ARGS, ENV, VERSION, etc.
  highlight(0, 'juliaConstIO',          { link = "Constant" })  -- stdout, stdin, stderr, devnull
  highlight(0, 'juliaConstGeneric',     { link = "Constant" })  -- nothing, undef, missing
  highlight(0, 'juliaBool',             { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Base Types
  highlight(0, 'juliaBaseTypeNum',      { link = "Type" })  -- Int, Float64, Complex, etc.
  highlight(0, 'juliaBaseTypeC',        { link = "Type" })  -- Cint, Clong, Cfloat, etc.
  highlight(0, 'juliaBaseTypeError',    { link = "Type" })  -- Exception types
  highlight(0, 'juliaBaseTypeIter',     { link = "Type" })  -- Iterator types
  highlight(0, 'juliaBaseTypeString',   { link = "String" })  -- String, SubString, etc.
  highlight(0, 'juliaBaseTypeArray',    { link = "Type" })  -- Array, Vector, Matrix, Dict
  highlight(0, 'juliaBaseTypeRange',    { link = "Type" })  -- Range types
  highlight(0, 'juliaBaseTypeIO',       { link = "Type" })  -- IO types
  highlight(0, 'juliaBaseTypeCast',     { link = "Type" })  -- Type casting types

  -- Type
  highlight(0, 'juliaType',             { link = "Type" })  -- Type names
  highlight(0, 'juliaTypeDef',          { link = "Type" })  -- Type definitions
  highlight(0, 'juliaTypeParameter',    { link = "Type" })  -- Type parameters

  -- Functions
  highlight(0, 'juliaFunctionName',     { link = "Function" })  -- Function definitions
  highlight(0, 'juliaFunctionCall',     { link = "Function" })  -- Function calls
  highlight(0, 'juliaBuiltinFunction',  { link = "Function" })  -- Built-in functions

  -- Macros
  highlight(0, 'juliaMacro',            { fg = colors.pink,       bg = 'NONE' })  -- @macro_name
  highlight(0, 'juliaMacroName',        { fg = colors.pink,       bg = 'NONE' })  -- Macro name after @
  highlight(0, 'juliaMacroCall',        { fg = colors.pink,       bg = 'NONE' })  -- Macro calls
  highlight(0, 'juliaDollarVar',        { link = "Variable" })  -- $variable interpolation
  highlight(0, 'juliaStringPrefixMacro',{ link = "String" })  -- String prefix macros (r"", b"")

  -- Operators
  highlight(0, 'juliaOperator',         { link = "Operator" })  -- +, -, *, /, etc.
  highlight(0, 'juliaArithOperator',    { link = "Operator" })  -- Arithmetic operators
  highlight(0, 'juliaBitOperator',      { link = "Operator" })  -- Bitwise operators
  highlight(0, 'juliaBoolOperator',     { link = "Operator" })  -- &&, ||, !
  highlight(0, 'juliaCompOperator',     { link = "Operator" })  -- ==, !=, <, >, <=, >=
  highlight(0, 'juliaAssignOperator',   { link = "Operator" })  -- =, +=, -=, etc.
  highlight(0, 'juliaRangeOperator',    { link = "Operator" })  -- : range operator
  highlight(0, 'juliaTernaryOperator',  { link = "Operator" })  -- ? :
  highlight(0, 'juliaPipeOperator',     { link = "Operator" })  -- |>
  highlight(0, 'juliaComposeOperator',  { link = "Operator" })  -- ∘
  highlight(0, 'juliaBroadcastOp',      { fg = colors.white,      bg = 'NONE' })  -- .+, .-, .*, etc.
  highlight(0, 'juliaTransposeOp',      { fg = colors.white,      bg = 'NONE' })  -- ' transpose
  highlight(0, 'juliaSplatOp',          { fg = colors.white,      bg = 'NONE' })  -- ... splat

  -- Unicode Operators
  highlight(0, 'juliaUnicodeOp',        { fg = colors.white,      bg = 'NONE' })  -- ∈, ∉, ⊆, ∩, ∪, etc.
  highlight(0, 'juliaArrowOp',          { fg = colors.white,      bg = 'NONE' })  -- →, ←, ↔, etc.
  highlight(0, 'juliaComparisonOp',     { fg = colors.white,      bg = 'NONE' })  -- ≠, ≤, ≥, ≡, ≢

  -- Variables / Parameters
  highlight(0, 'juliaVariable',         { link = "Variable" })  -- Variables
  highlight(0, 'juliaParameter',        { fg = colors.purple,     bg = 'NONE' })  -- Function parameters
  highlight(0, 'juliaField',            { fg = colors.purple,     bg = 'NONE' })  -- Struct fields
  highlight(0, 'juliaSymbol',           { fg = colors.purple,     bg = 'NONE' })  -- :symbol

  -- Strings
  highlight(0, 'juliaString',           { link = "String" })  -- "string"
  highlight(0, 'juliaStringDelim',      { link = "Delimiter" })  -- String delimiters
  highlight(0, 'juliaTripleString',     { link = "String" })  -- """multiline"""
  highlight(0, 'juliaRawString',        { link = "String" })  -- raw"string"
  highlight(0, 'juliaByteString',       { link = "String" })  -- b"bytes"
  highlight(0, 'juliaVersionString',    { link = "String" })  -- v"1.0.0"
  highlight(0, 'juliaRegexString',      { link = "String" })  -- r"regex"
  highlight(0, 'juliaSubstitutionString', { link = "String" })  -- s"substitution"
  highlight(0, 'juliaShellString',      { link = "String" })  -- `shell command`
  highlight(0, 'juliaDocString',        { link = "String" })  -- Docstrings
  highlight(0, 'juliaStringEscape',     { link = "String" })  -- \n, \t, etc.
  highlight(0, 'juliaStringInterp',     { link = "String" })  -- $var, $(expr)
  highlight(0, 'juliaStringInterpDelim', { link = "Delimiter" })  -- $ in interpolation

  -- Characters
  highlight(0, 'juliaChar',             { fg = colors.redLight,   bg = 'NONE' })  -- 'c'
  highlight(0, 'juliaCharDelim',        { link = "Delimiter" })  -- Character delimiters

  -- Numbers
  highlight(0, 'juliaNumber',           { link = "Number" })  -- Integers
  highlight(0, 'juliaFloat',            { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'juliaComplexNum',       { fg = colors.greenLight, bg = 'NONE' })  -- 1+2im
  highlight(0, 'juliaHexNum',           { fg = colors.greenLight, bg = 'NONE' })  -- 0x...
  highlight(0, 'juliaBinNum',           { fg = colors.greenLight, bg = 'NONE' })  -- 0b...
  highlight(0, 'juliaOctNum',           { fg = colors.greenLight, bg = 'NONE' })  -- 0o...
  highlight(0, 'juliaImagUnit',         { fg = colors.greenLight, bg = 'NONE' })  -- im suffix

  -- Comments
  highlight(0, 'juliaComment',          { link = "Comment" })  -- # comment
  highlight(0, 'juliaBlockComment',     { link = "Comment" })  -- #= comment =#
  highlight(0, 'juliaCommentTodo',      { link = "Comment" })  -- TODO, FIXME, etc.
  highlight(0, 'juliaDocComment',       { link = "Comment" })  -- Documentation comments

  -- Delimiters / Punctuation
  highlight(0, 'juliaParDelim',         { link = "Delimiter" })  -- ()
  highlight(0, 'juliaSqBraDelim',       { link = "Delimiter" })  -- []
  highlight(0, 'juliaCurBraDelim',      { link = "Delimiter" })  -- {}
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
  highlight(0, '@variable.julia',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.julia',      { link = "Variable" })  -- Built-in variables
  highlight(0, '@variable.member.julia',       { link = "Variable" })  -- Struct members
  highlight(0, '@variable.parameter.julia',    { link = "Variable" })  -- Parameters

  -- Constants
  highlight(0, '@constant.julia',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.julia',      { link = "Constant" })  -- nothing, missing, true, false

  -- Functions
  highlight(0, '@function.julia',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.julia',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.julia',      { link = "Function" })  -- Built-in functions
  highlight(0, '@function.method.julia',       { link = "Function" })  -- Methods
  highlight(0, '@function.method.call.julia',  { link = "Function" })  -- Method calls
  highlight(0, '@function.macro.julia',        { link = "Function" })  -- Macros

  -- Types
  highlight(0, '@type.julia',                  { link = "Type" })  -- Types
  highlight(0, '@type.builtin.julia',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.julia',       { link = "Type" })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.julia',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.julia',      { link = "Keyword" })  -- function, macro
  highlight(0, '@keyword.return.julia',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.conditional.julia',   { link = "Conditional" })  -- if, elseif, else
  highlight(0, '@keyword.conditional.ternary.julia', { link = "Conditional" })  -- ? :
  highlight(0, '@keyword.repeat.julia',        { link = "Keyword" })  -- for, while
  highlight(0, '@keyword.exception.julia',     { link = "Keyword" })  -- try, catch, finally
  highlight(0, '@keyword.modifier.julia',      { link = "Keyword" })  -- const, local, global
  highlight(0, '@keyword.import.julia',        { link = "Keyword" })  -- import, using, export
  highlight(0, '@keyword.type.julia',          { link = "Keyword" })  -- struct, abstract type
  highlight(0, '@keyword.operator.julia',      { link = "Operator" })  -- in, isa, where

  -- Strings
  highlight(0, '@string.julia',                { link = "String" })  -- Strings
  highlight(0, '@string.documentation.julia',  { link = "String" })  -- Docstrings
  highlight(0, '@string.escape.julia',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.julia',        { link = "String" })  -- Command strings
  highlight(0, '@string.special.symbol.julia', { link = "String" })  -- :symbol
  highlight(0, '@string.regexp.julia',         { link = "String" })  -- Regex

  -- Numbers
  highlight(0, '@number.julia',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.julia',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.julia',               { link = "Boolean" })  -- true, false

  -- Characters
  highlight(0, '@character.julia',             { fg = colors.redLight,  bg = 'NONE' })  -- Characters

  -- Comments
  highlight(0, '@comment.julia',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.julia', { link = "Comment" })  -- Doc comments

  -- Operators
  highlight(0, '@operator.julia',              { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.julia',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.julia', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.julia',   { fg = colors.pink,      bg = 'NONE' })  -- $, @ interpolation

  -- Module
  highlight(0, '@module.julia',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names

  -- Spell
  highlight(0, '@spell.julia',                 { fg = colors.red,       bg = 'NONE' })  -- Spellchecked


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.julia)

  highlight(0, '@lsp.type.variable.julia',      { link = "Variable" })  -- Variables
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
  highlight(0, '@lsp.type.keyword.julia',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.julia',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.comment.julia',       { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.string.julia',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.julia',        { link = "Number" })  -- Numbers

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.julia',  { link = "Variable" })  -- const
  highlight(0, '@lsp.typemod.function.builtin.julia',   { fg = colors.orange,   bg = 'NONE' })  -- Built-ins
  highlight(0, '@lsp.typemod.type.defaultLibrary.julia', { fg = colors.turquoise, bg = 'NONE' })  -- Base types


  -----------------------------------------------------------------------------
  -- Base Types (Custom highlight groups)

  -- Numeric Types
  highlight(0, 'juliaTypeInt',          { link = "Type" })  -- Int8-Int128, UInt8-UInt128
  highlight(0, 'juliaTypeFloat',        { link = "Type" })  -- Float16, Float32, Float64
  highlight(0, 'juliaTypeComplex',      { link = "Type" })  -- Complex{T}
  highlight(0, 'juliaTypeRational',     { link = "Type" })  -- Rational{T}
  highlight(0, 'juliaTypeBigNum',       { link = "Type" })  -- BigInt, BigFloat

  -- Abstract Numeric Types
  highlight(0, 'juliaTypeNumber',       { link = "Number" })  -- Number, Real, Integer, etc.
  highlight(0, 'juliaTypeSigned',       { link = "Type" })  -- Signed, Unsigned
  highlight(0, 'juliaTypeAbstractFloat', { link = "Type" })  -- AbstractFloat

  -- Collection Types
  highlight(0, 'juliaTypeArray',        { link = "Type" })  -- Array, Vector, Matrix
  highlight(0, 'juliaTypeDict',         { link = "Type" })  -- Dict, IdDict, WeakKeyDict
  highlight(0, 'juliaTypeSet',          { link = "Type" })  -- Set, BitSet
  highlight(0, 'juliaTypeTuple',        { link = "Type" })  -- Tuple, NamedTuple, NTuple

  -- String Types
  highlight(0, 'juliaTypeString',       { link = "String" })  -- String, SubString, AbstractString
  highlight(0, 'juliaTypeChar',         { link = "Type" })  -- Char, AbstractChar
  highlight(0, 'juliaTypeRegex',        { link = "Type" })  -- Regex, RegexMatch

  -- I/O Types
  highlight(0, 'juliaTypeIO',           { link = "Type" })  -- IO, IOStream, IOBuffer
  highlight(0, 'juliaTypeFile',         { link = "Type" })  -- File types

  -- Special Types
  highlight(0, 'juliaTypeNothing',      { link = "Type" })  -- Nothing
  highlight(0, 'juliaTypeMissing',      { link = "Type" })  -- Missing
  highlight(0, 'juliaTypeSome',         { link = "Type" })  -- Some{T}
  highlight(0, 'juliaTypeUnion',        { link = "Type" })  -- Union, UnionAll

  -- Fundamental Types
  highlight(0, 'juliaTypeAny',          { link = "Type" })  -- Any
  highlight(0, 'juliaTypeDataType',     { link = "Type" })  -- DataType, Type
  highlight(0, 'juliaTypeFunction',     { link = "Type" })  -- Function
  highlight(0, 'juliaTypeModule',       { link = "Type" })  -- Module
  highlight(0, 'juliaTypeExpr',         { link = "Type" })  -- Expr, Symbol
  highlight(0, 'juliaTypePtr',          { link = "Type" })  -- Ptr, Ref

  -- Concurrency Types
  highlight(0, 'juliaTypeTask',         { link = "Type" })  -- Task
  highlight(0, 'juliaTypeChannel',      { link = "Type" })  -- Channel
  highlight(0, 'juliaTypeLock',         { link = "Type" })  -- ReentrantLock, SpinLock

  -- C Interop Types
  highlight(0, 'juliaTypeCint',         { link = "Type" })  -- Cint, Clong, Cfloat, etc.
  highlight(0, 'juliaTypeCptr',         { link = "Type" })  -- Cstring, Cwstring


  -----------------------------------------------------------------------------
  -- Exception Types

  highlight(0, 'juliaException',        { fg = colors.turquoise,  bg = 'NONE' })  -- Base exception type
  highlight(0, 'juliaExceptionArg',     { fg = colors.turquoise,  bg = 'NONE' })  -- ArgumentError
  highlight(0, 'juliaExceptionBounds',  { fg = colors.turquoise,  bg = 'NONE' })  -- BoundsError
  highlight(0, 'juliaExceptionDomain',  { fg = colors.turquoise,  bg = 'NONE' })  -- DomainError
  highlight(0, 'juliaExceptionType',    { link = "Type" })  -- TypeError
  highlight(0, 'juliaExceptionMethod',  { link = "Function" })  -- MethodError
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
  highlight(0, 'juliaMacroString',      { link = "String" })  -- @v_str, @r_str, etc.
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
  highlight(0, 'juliaFuncType',         { link = "Type" })  -- typeof, isa, supertype, etc.
  highlight(0, 'juliaFuncField',        { link = "Function" })  -- fieldnames, getfield, setfield!

  -- Collection Functions
  highlight(0, 'juliaFuncArray',        { link = "Function" })  -- push!, pop!, append!, etc.
  highlight(0, 'juliaFuncIteration',    { link = "Function" })  -- map, filter, reduce, etc.
  highlight(0, 'juliaFuncSort',         { link = "Function" })  -- sort, sort!, sortperm

  -- Math Functions
  highlight(0, 'juliaFuncMath',         { link = "Function" })  -- sin, cos, exp, log, sqrt, etc.
  highlight(0, 'juliaFuncLinAlg',       { link = "Function" })  -- dot, cross, norm, etc.

  -- I/O Functions
  highlight(0, 'juliaFuncIO',           { link = "Function" })  -- print, println, read, write, etc.
  highlight(0, 'juliaFuncFile',         { link = "Function" })  -- open, close, readlines, etc.

  -- String Functions
  highlight(0, 'juliaFuncString',       { link = "String" })  -- string, join, split, strip, etc.

  -- Error Handling
  highlight(0, 'juliaFuncError',        { link = "Function" })  -- error, throw, rethrow

  -- System Functions
  highlight(0, 'juliaFuncSystem',       { link = "Function" })  -- exit, run, cd, pwd, etc.


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
  highlight(0, 'juliaLinAlgType',       { link = "Type" })  -- Matrix types
  highlight(0, 'juliaLinAlgFunc',       { link = "Function" })  -- LinearAlgebra functions

  -- Statistics / StatsBase
  highlight(0, 'juliaStatsFunc',        { link = "Function" })  -- mean, std, var, etc.

  -- DataFrames
  highlight(0, 'juliaDataFrameType',    { link = "Type" })  -- DataFrame, DataFrameRow
  highlight(0, 'juliaDataFrameFunc',    { link = "Function" })  -- select, transform, etc.

  -- Plots / Makie
  highlight(0, 'juliaPlotFunc',         { link = "Function" })  -- plot, scatter, heatmap, etc.


  -----------------------------------------------------------------------------
  -- Special Constructs

  -- Anonymous Functions
  highlight(0, 'juliaLambda',           { fg = colors.blue,       bg = 'NONE' })  -- -> in anonymous functions

  -- Broadcast Dot
  highlight(0, 'juliaBroadcastDot',     { fg = colors.white,      bg = 'NONE' })  -- f.() broadcast call

  -- Type Annotations
  highlight(0, 'juliaTypeAnnotation',   { link = "Type" })  -- ::Type

  -- Parametric Types
  highlight(0, 'juliaParametricType',   { link = "Type" })  -- Array{T,N}

  -- Comprehensions
  highlight(0, 'juliaComprehension',    { fg = colors.white,      bg = 'NONE' })  -- [x for x in xs]
  highlight(0, 'juliaGenerator',        { fg = colors.white,      bg = 'NONE' })  -- (x for x in xs)

  -- Do Block
  highlight(0, 'juliaDoBlock',          { fg = colors.blue,       bg = 'NONE' })  -- do keyword

  -- Multiple Dispatch
  highlight(0, 'juliaMethodSig',        { link = "Function" })  -- Method signatures
end

return julia
