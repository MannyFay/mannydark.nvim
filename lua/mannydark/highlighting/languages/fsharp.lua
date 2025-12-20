-------------------------------------------------------------------------------
-- F# Files
-- Highlighting for .fs, .fsi, .fsx, .fsscript files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local fsharp  = {}


-------------------------------------------------------------------------------
-- Settings

fsharp.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'fsharpKeyword',         { link = "Keyword" })  -- General keywords
  highlight(0, 'fsharpConditional',     { link = "Conditional" })  -- if, then, else, elif
  highlight(0, 'fsharpMatch',           { fg = colors.blue,       bg = 'NONE'            })  -- match, with, when
  highlight(0, 'fsharpFunction',        { link = "Function" })  -- function keyword

  -- Keywords - Loops
  highlight(0, 'fsharpRepeat',          { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'fsharpTo',              { fg = colors.blue,       bg = 'NONE'            })  -- to, downto
  highlight(0, 'fsharpDone',            { fg = colors.blue,       bg = 'NONE'            })  -- done
  highlight(0, 'fsharpIn',              { fg = colors.blue,       bg = 'NONE'            })  -- in

  -- Keywords - Bindings
  highlight(0, 'fsharpLet',             { fg = colors.blue,       bg = 'NONE'            })  -- let
  highlight(0, 'fsharpLetBang',         { fg = colors.blue,       bg = 'NONE'            })  -- let!
  highlight(0, 'fsharpUse',             { fg = colors.blue,       bg = 'NONE'            })  -- use
  highlight(0, 'fsharpUseBang',         { fg = colors.blue,       bg = 'NONE'            })  -- use!
  highlight(0, 'fsharpDo',              { fg = colors.blue,       bg = 'NONE'            })  -- do
  highlight(0, 'fsharpDoBang',          { fg = colors.blue,       bg = 'NONE'            })  -- do!
  highlight(0, 'fsharpRec',             { fg = colors.blue,       bg = 'NONE'            })  -- rec
  highlight(0, 'fsharpAnd',             { fg = colors.blue,       bg = 'NONE'            })  -- and (mutual recursion)
  highlight(0, 'fsharpMutable',         { fg = colors.blue,       bg = 'NONE'            })  -- mutable

  -- Keywords - Functions
  highlight(0, 'fsharpFun',             { fg = colors.blue,       bg = 'NONE'            })  -- fun (lambda)
  highlight(0, 'fsharpInline',          { fg = colors.blue,       bg = 'NONE'            })  -- inline

  -- Keywords - Type Definitions
  highlight(0, 'fsharpType',            { link = "Type" })  -- type
  highlight(0, 'fsharpClass',           { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'fsharpStruct',          { fg = colors.blue,       bg = 'NONE'            })  -- struct
  highlight(0, 'fsharpInterface',       { fg = colors.blue,       bg = 'NONE'            })  -- interface
  highlight(0, 'fsharpModule',          { fg = colors.blue,       bg = 'NONE'            })  -- module
  highlight(0, 'fsharpNamespace',       { fg = colors.blue,       bg = 'NONE'            })  -- namespace
  highlight(0, 'fsharpOf',              { fg = colors.blue,       bg = 'NONE'            })  -- of

  -- Keywords - OOP
  highlight(0, 'fsharpNew',             { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'fsharpMember',          { fg = colors.blue,       bg = 'NONE'            })  -- member
  highlight(0, 'fsharpStatic',          { fg = colors.blue,       bg = 'NONE'            })  -- static
  highlight(0, 'fsharpAbstract',        { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'fsharpDefault',         { fg = colors.blue,       bg = 'NONE'            })  -- default
  highlight(0, 'fsharpOverride',        { fg = colors.blue,       bg = 'NONE'            })  -- override
  highlight(0, 'fsharpInherit',         { fg = colors.blue,       bg = 'NONE'            })  -- inherit
  highlight(0, 'fsharpBase',            { fg = colors.blue,       bg = 'NONE'            })  -- base
  highlight(0, 'fsharpVal',             { fg = colors.blue,       bg = 'NONE'            })  -- val

  -- Keywords - Access Control
  highlight(0, 'fsharpPublic',          { fg = colors.blue,       bg = 'NONE'            })  -- public
  highlight(0, 'fsharpPrivate',         { fg = colors.blue,       bg = 'NONE'            })  -- private
  highlight(0, 'fsharpInternal',        { fg = colors.blue,       bg = 'NONE'            })  -- internal

  -- Keywords - Exception Handling
  highlight(0, 'fsharpException',       { fg = colors.blue,       bg = 'NONE'            })  -- exception, try, finally, raise, failwith
  highlight(0, 'fsharpTry',             { fg = colors.blue,       bg = 'NONE'            })  -- try
  highlight(0, 'fsharpFinally',         { fg = colors.blue,       bg = 'NONE'            })  -- finally
  highlight(0, 'fsharpWith',            { fg = colors.blue,       bg = 'NONE'            })  -- with (catch)

  -- Keywords - Async/Computation Expressions
  highlight(0, 'fsharpAsync',           { fg = colors.blue,       bg = 'NONE'            })  -- async
  highlight(0, 'fsharpLazy',            { fg = colors.blue,       bg = 'NONE'            })  -- lazy
  highlight(0, 'fsharpReturn',          { fg = colors.blue,       bg = 'NONE'            })  -- return
  highlight(0, 'fsharpReturnBang',      { fg = colors.blue,       bg = 'NONE'            })  -- return!
  highlight(0, 'fsharpYield',           { fg = colors.blue,       bg = 'NONE'            })  -- yield
  highlight(0, 'fsharpYieldBang',       { fg = colors.blue,       bg = 'NONE'            })  -- yield!
  highlight(0, 'fsharpMatchBang',       { fg = colors.blue,       bg = 'NONE'            })  -- match!
  highlight(0, 'fsharpAndBang',         { fg = colors.blue,       bg = 'NONE'            })  -- and!

  -- Keywords - Type Conversion
  highlight(0, 'fsharpUpcast',          { fg = colors.blue,       bg = 'NONE'            })  -- upcast
  highlight(0, 'fsharpDowncast',        { fg = colors.blue,       bg = 'NONE'            })  -- downcast

  -- Keywords - Logic/Boolean
  highlight(0, 'fsharpNot',             { fg = colors.blue,       bg = 'NONE'            })  -- not
  highlight(0, 'fsharpOr',              { fg = colors.blue,       bg = 'NONE'            })  -- or (pattern)

  -- Keywords - Block Delimiters
  highlight(0, 'fsharpBegin',           { fg = colors.blue,       bg = 'NONE'            })  -- begin
  highlight(0, 'fsharpEnd',             { fg = colors.blue,       bg = 'NONE'            })  -- end

  -- Keywords - Other
  highlight(0, 'fsharpAs',              { fg = colors.blue,       bg = 'NONE'            })  -- as
  highlight(0, 'fsharpAssert',          { fg = colors.blue,       bg = 'NONE'            })  -- assert
  highlight(0, 'fsharpDelegate',        { fg = colors.blue,       bg = 'NONE'            })  -- delegate
  highlight(0, 'fsharpExtern',          { fg = colors.blue,       bg = 'NONE'            })  -- extern
  highlight(0, 'fsharpFixed',           { fg = colors.blue,       bg = 'NONE'            })  -- fixed
  highlight(0, 'fsharpGlobal',          { fg = colors.blue,       bg = 'NONE'            })  -- global
  highlight(0, 'fsharpVoid',            { fg = colors.blue,       bg = 'NONE'            })  -- void
  highlight(0, 'fsharpConst',           { fg = colors.blue,       bg = 'NONE'            })  -- const

  -- Keywords - Imports
  highlight(0, 'fsharpOpen',            { fg = colors.pink,       bg = 'NONE'            })  -- open

  -- Types
  highlight(0, 'fsharpTypeName',        { link = "Type" })  -- Type names
  highlight(0, 'fsharpBuiltinType',     { link = "Type" })  -- int, float, string, bool, etc.
  highlight(0, 'fsharpIntType',         { link = "Type" })  -- int, int32, int64, byte, etc.
  highlight(0, 'fsharpFloatType',       { link = "Type" })  -- float, float32, double, decimal
  highlight(0, 'fsharpBoolType',        { link = "Type" })  -- bool
  highlight(0, 'fsharpStringType',      { link = "String" })  -- string
  highlight(0, 'fsharpCharType',        { link = "Type" })  -- char
  highlight(0, 'fsharpUnitType',        { link = "Type" })  -- unit
  highlight(0, 'fsharpObjType',         { link = "Type" })  -- obj
  highlight(0, 'fsharpOptionType',      { link = "Type" })  -- option, Option
  highlight(0, 'fsharpResultType',      { link = "Type" })  -- Result
  highlight(0, 'fsharpListType',        { link = "Type" })  -- list, List
  highlight(0, 'fsharpArrayType',       { link = "Type" })  -- array, Array
  highlight(0, 'fsharpSeqType',         { link = "Type" })  -- seq, Seq
  highlight(0, 'fsharpAsyncType',       { link = "Type" })  -- Async, Task
  highlight(0, 'fsharpGenericType',     { link = "Type" })  -- 'a, 'b generic type parameters

  -- Discriminated Unions
  highlight(0, 'fsharpUnionCase',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Union case names (Some, None, Ok, Error)

  -- Functions
  highlight(0, 'fsharpFunctionDef',     { link = "Function" })  -- Function definitions
  highlight(0, 'fsharpFunctionCall',    { link = "Function" })  -- Function calls
  highlight(0, 'fsharpBuiltinFunc',     { link = "Function" })  -- printfn, sprintf, failwith, etc.
  highlight(0, 'fsharpMethodDef',       { link = "Function" })  -- Method definitions
  highlight(0, 'fsharpMethodCall',      { link = "Function" })  -- Method calls
  highlight(0, 'fsharpConstructor',     { fg = colors.orange,     bg = 'NONE'            })  -- Constructors

  -- Variables
  highlight(0, 'fsharpIdentifier',      { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'fsharpParameter',       { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'fsharpBinding',         { fg = colors.purple,     bg = 'NONE'            })  -- let bindings
  highlight(0, 'fsharpField',           { fg = colors.purple,     bg = 'NONE'            })  -- Record fields
  highlight(0, 'fsharpProperty',        { fg = colors.purple,     bg = 'NONE'            })  -- Properties

  -- Pattern Matching
  highlight(0, 'fsharpPattern',         { fg = colors.purple,     bg = 'NONE'            })  -- Pattern variables
  highlight(0, 'fsharpWildcard',        { fg = colors.purple,     bg = 'NONE'            })  -- _ wildcard
  highlight(0, 'fsharpActivePattern',   { fg = colors.pink,       bg = 'NONE'            })  -- (|...|) active patterns

  -- Constants
  highlight(0, 'fsharpConstant',        { link = "Constant" })  -- Constants
  highlight(0, 'fsharpBoolean',         { link = "Boolean" })  -- true, false
  highlight(0, 'fsharpNull',            { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'fsharpUnit',            { fg = colors.blue,       bg = 'NONE'            })  -- ()

  -- Strings
  highlight(0, 'fsharpString',          { link = "String" })  -- "strings"
  highlight(0, 'fsharpVerbatimString',  { link = "String" })  -- @"verbatim"
  highlight(0, 'fsharpTripleString',    { link = "String" })  -- """triple quoted"""
  highlight(0, 'fsharpInterpolated',    { fg = colors.redLight,   bg = 'NONE'            })  -- $"interpolated {x}"
  highlight(0, 'fsharpInterpolation',   { fg = colors.pink,       bg = 'NONE'            })  -- {expression}
  highlight(0, 'fsharpStringEscape',    { link = "String" })  -- \n, \t, etc.
  highlight(0, 'fsharpCharacter',       { fg = colors.redLight,   bg = 'NONE'            })  -- 'c'
  highlight(0, 'fsharpFormat',          { fg = colors.pink,       bg = 'NONE'            })  -- %d, %s, %A in printf

  -- Numbers
  highlight(0, 'fsharpNumber',          { link = "Number" })  -- Numbers
  highlight(0, 'fsharpInteger',         { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'fsharpFloat',           { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'fsharpHex',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'fsharpOctal',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0o77 octal
  highlight(0, 'fsharpBinary',          { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary
  highlight(0, 'fsharpNumericSuffix',   { fg = colors.greenLight, bg = 'NONE'            })  -- L, UL, y, uy, s, us, etc.

  -- Operators - Standard
  highlight(0, 'fsharpOperator',        { link = "Operator" })  -- + - * / % = < >
  highlight(0, 'fsharpArithmetic',      { fg = colors.white,      bg = 'NONE'            })  -- + - * / %
  highlight(0, 'fsharpComparison',      { fg = colors.white,      bg = 'NONE'            })  -- = <> < > <= >=
  highlight(0, 'fsharpLogical',         { fg = colors.white,      bg = 'NONE'            })  -- && || not

  -- Operators - Pipeline and Composition
  highlight(0, 'fsharpPipeForward',     { fg = colors.white,      bg = 'NONE'            })  -- |>
  highlight(0, 'fsharpPipeBackward',    { fg = colors.white,      bg = 'NONE'            })  -- <|
  highlight(0, 'fsharpComposeForward',  { fg = colors.white,      bg = 'NONE'            })  -- >>
  highlight(0, 'fsharpComposeBackward', { fg = colors.white,      bg = 'NONE'            })  -- <<

  -- Operators - Type/Cast
  highlight(0, 'fsharpTypeAnnotation',  { link = "Type" })  -- : type annotation
  highlight(0, 'fsharpUpcastOp',        { fg = colors.white,      bg = 'NONE'            })  -- :>
  highlight(0, 'fsharpDowncastOp',      { fg = colors.white,      bg = 'NONE'            })  -- :?>
  highlight(0, 'fsharpTypeTest',        { link = "Type" })  -- :?

  -- Operators - Other
  highlight(0, 'fsharpArrow',           { fg = colors.white,      bg = 'NONE'            })  -- ->
  highlight(0, 'fsharpAssignment',      { fg = colors.white,      bg = 'NONE'            })  -- <-
  highlight(0, 'fsharpCons',            { fg = colors.white,      bg = 'NONE'            })  -- :: list cons
  highlight(0, 'fsharpConcat',          { fg = colors.white,      bg = 'NONE'            })  -- @ list concat
  highlight(0, 'fsharpRange',           { fg = colors.white,      bg = 'NONE'            })  -- .. range
  highlight(0, 'fsharpLambda',          { fg = colors.white,      bg = 'NONE'            })  -- fun ... ->

  -- Attributes
  highlight(0, 'fsharpAttribute',       { fg = colors.pink,       bg = 'NONE'            })  -- [<Attribute>]
  highlight(0, 'fsharpAttributeName',   { fg = colors.pink,       bg = 'NONE'            })  -- Attribute name
  highlight(0, 'fsharpAttributeBracket',{ fg = colors.pink,       bg = 'NONE'            })  -- [< >]

  -- Preprocessor Directives
  highlight(0, 'fsharpPreProc',         { fg = colors.pink,       bg = 'NONE'            })  -- Preprocessor
  highlight(0, 'fsharpIfDef',           { fg = colors.pink,       bg = 'NONE'            })  -- #if, #else, #endif
  highlight(0, 'fsharpLight',           { fg = colors.pink,       bg = 'NONE'            })  -- #light
  highlight(0, 'fsharpNowarn',          { fg = colors.pink,       bg = 'NONE'            })  -- #nowarn
  highlight(0, 'fsharpLoad',            { fg = colors.pink,       bg = 'NONE'            })  -- #load
  highlight(0, 'fsharpReference',       { fg = colors.pink,       bg = 'NONE'            })  -- #r

  -- Comments
  highlight(0, 'fsharpComment',         { link = "Comment" })  -- // comments
  highlight(0, 'fsharpLineComment',     { link = "Comment" })  -- // line comments
  highlight(0, 'fsharpBlockComment',    { link = "Comment" })  -- (* *) block comments
  highlight(0, 'fsharpDocComment',      { link = "Comment" })  -- /// XML doc comments
  highlight(0, 'fsharpXmlTag',          { fg = colors.green,      bg = 'NONE'            })  -- <summary>, <param>
  highlight(0, 'fsharpTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Modules
  highlight(0, 'fsharpModuleName',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'fsharpNamespaceName',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace names

  -- Error
  highlight(0, 'fsharpError',           { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.fsharp)

  -- Variables
  highlight(0, '@variable.fsharp',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.fsharp',      { link = "Variable" })  -- this, base
  highlight(0, '@variable.parameter.fsharp',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.parameter.builtin.fsharp', { link = "Variable" })  -- __SOURCE_DIRECTORY__, etc.
  highlight(0, '@variable.member.fsharp',       { link = "Variable" })  -- Fields

  -- Constants
  highlight(0, '@constant.fsharp',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.fsharp',      { link = "Constant" })  -- true, false, null
  highlight(0, '@constant.macro.fsharp',        { link = "Constant" })  -- Preprocessor symbols

  -- Functions
  highlight(0, '@function.fsharp',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.fsharp',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.fsharp',      { link = "Function" })  -- Built-in functions
  highlight(0, '@function.method.fsharp',       { link = "Function" })  -- Method definitions
  highlight(0, '@constructor.fsharp',           { fg = colors.turquoise, bg = 'NONE' })  -- Union case constructors

  -- Types
  highlight(0, '@type.fsharp',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.fsharp',          { link = "Type" })  -- int, string, bool, etc.
  highlight(0, '@type.definition.fsharp',       { link = "Type" })  -- Type definitions

  -- Modules
  highlight(0, '@module.fsharp',                { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@module.builtin.fsharp',        { fg = colors.turquoise, bg = 'NONE' })  -- Built-in modules

  -- Properties
  highlight(0, '@property.fsharp',              { fg = colors.purple,    bg = 'NONE' })  -- Properties/fields

  -- Keywords
  highlight(0, '@keyword.fsharp',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.type.fsharp',          { link = "Keyword" })  -- type, class, struct, interface
  highlight(0, '@keyword.modifier.fsharp',      { link = "Keyword" })  -- public, private, mutable
  highlight(0, '@keyword.function.fsharp',      { link = "Keyword" })  -- let, fun, function
  highlight(0, '@keyword.operator.fsharp',      { link = "Operator" })  -- and, or, not
  highlight(0, '@keyword.return.fsharp',        { link = "Keyword" })  -- return, return!
  highlight(0, '@keyword.repeat.fsharp',        { link = "Keyword" })  -- for, while, do
  highlight(0, '@keyword.conditional.fsharp',   { link = "Conditional" })  -- if, then, else, elif, match
  highlight(0, '@keyword.exception.fsharp',     { link = "Keyword" })  -- try, with, finally, raise
  highlight(0, '@keyword.import.fsharp',        { link = "Keyword" })  -- open
  highlight(0, '@keyword.directive.fsharp',     { link = "Keyword" })  -- Preprocessor directives

  -- Strings
  highlight(0, '@string.fsharp',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.fsharp',         { link = "String" })  -- Escape sequences
  highlight(0, '@character.fsharp',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals
  highlight(0, '@character.special.fsharp',     { fg = colors.pink,      bg = 'NONE' })  -- Escape chars

  -- Numbers
  highlight(0, '@number.fsharp',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.fsharp',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.fsharp',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.fsharp',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.fsharp', { link = "Comment" })  -- XML doc comments

  -- Attributes
  highlight(0, '@attribute.fsharp',             { fg = colors.pink,      bg = 'NONE' })  -- [<Attribute>]

  -- Operators and Punctuation
  highlight(0, '@operator.fsharp',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.fsharp',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.fsharp', { link = "Delimiter" })  -- , ; |
  highlight(0, '@punctuation.special.fsharp',   { fg = colors.pink,      bg = 'NONE' })  -- # in directives, (| |)


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.fsharp)

  highlight(0, '@lsp.type.variable.fsharp',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.fsharp',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.fsharp',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.fsharp',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.fsharp',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.fsharp',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.fsharp',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.struct.fsharp',        { fg = colors.turquoise, bg = 'NONE' })  -- Structs/Records
  highlight(0, '@lsp.type.interface.fsharp',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.enum.fsharp',          { fg = colors.turquoise, bg = 'NONE' })  -- Discriminated unions
  highlight(0, '@lsp.type.enumMember.fsharp',    { fg = colors.turquoise, bg = 'NONE' })  -- Union cases
  highlight(0, '@lsp.type.namespace.fsharp',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.module.fsharp',        { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.typeParameter.fsharp', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type parameters
  highlight(0, '@lsp.type.keyword.fsharp',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.fsharp',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.fsharp',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.fsharp',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.fsharp',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.mutable.fsharp',  { link = "Variable" })  -- mutable variables
  highlight(0, '@lsp.typemod.function.declaration.fsharp', { fg = colors.orange, bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.type.declaration.fsharp', { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.member.static.fsharp',    { fg = colors.orange,    bg = 'NONE' })  -- Static members
end

return fsharp
