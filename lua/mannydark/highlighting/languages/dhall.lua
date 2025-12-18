-------------------------------------------------------------------------------
-- Dhall Files
-- Highlighting for .dhall files.
-- Dhall is a programmable configuration language (JSON + functions + types + imports)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local dhall     = {}


-------------------------------------------------------------------------------
-- Settings

dhall.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Comments
  highlight(0, 'dhallComment',             { fg = colors.red,        bg = 'NONE' })  -- -- single line
  highlight(0, 'dhallLineComment',         { fg = colors.red,        bg = 'NONE' })  -- -- comment
  highlight(0, 'dhallBlockComment',        { fg = colors.red,        bg = 'NONE' })  -- {- block -}
  highlight(0, 'dhallMultilineComment',    { fg = colors.red,        bg = 'NONE' })  -- {- ... -}
  highlight(0, 'dhallTodo',                { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME

  -- Keywords
  highlight(0, 'dhallKeyword',             { fg = colors.pink,       bg = 'NONE' })  -- let, in, etc.
  highlight(0, 'dhallLet',                 { fg = colors.pink,       bg = 'NONE' })  -- let
  highlight(0, 'dhallIn',                  { fg = colors.pink,       bg = 'NONE' })  -- in
  highlight(0, 'dhallAssert',              { fg = colors.pink,       bg = 'NONE' })  -- assert
  highlight(0, 'dhallWith',                { fg = colors.pink,       bg = 'NONE' })  -- with
  highlight(0, 'dhallUsing',               { fg = colors.pink,       bg = 'NONE' })  -- using
  highlight(0, 'dhallAs',                  { fg = colors.pink,       bg = 'NONE' })  -- as
  highlight(0, 'dhallMerge',               { fg = colors.pink,       bg = 'NONE' })  -- merge
  highlight(0, 'dhallToMap',               { fg = colors.pink,       bg = 'NONE' })  -- toMap
  highlight(0, 'dhallShowConstructor',     { fg = colors.pink,       bg = 'NONE' })  -- showConstructor
  highlight(0, 'dhallMissing',             { fg = colors.pink,       bg = 'NONE' })  -- missing

  -- Conditional
  highlight(0, 'dhallConditional',         { fg = colors.pink,       bg = 'NONE' })  -- if, then, else
  highlight(0, 'dhallIf',                  { fg = colors.pink,       bg = 'NONE' })  -- if
  highlight(0, 'dhallThen',                { fg = colors.pink,       bg = 'NONE' })  -- then
  highlight(0, 'dhallElse',                { fg = colors.pink,       bg = 'NONE' })  -- else

  -- Lambda and Forall
  highlight(0, 'dhallLambda',              { fg = colors.pink,       bg = 'NONE' })  -- λ, \
  highlight(0, 'dhallForall',              { fg = colors.pink,       bg = 'NONE' })  -- forall, ∀
  highlight(0, 'dhallArrow',               { fg = colors.pink,       bg = 'NONE' })  -- ->, →

  -- Built-in Types
  highlight(0, 'dhallType',                { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, 'dhallBuiltinType',         { fg = colors.turquoise,  bg = 'NONE' })  -- Built-in types
  highlight(0, 'dhallBool',                { fg = colors.turquoise,  bg = 'NONE' })  -- Bool type
  highlight(0, 'dhallNatural',             { fg = colors.turquoise,  bg = 'NONE' })  -- Natural type
  highlight(0, 'dhallInteger',             { fg = colors.turquoise,  bg = 'NONE' })  -- Integer type
  highlight(0, 'dhallDouble',              { fg = colors.turquoise,  bg = 'NONE' })  -- Double type
  highlight(0, 'dhallText',                { fg = colors.turquoise,  bg = 'NONE' })  -- Text type
  highlight(0, 'dhallList',                { fg = colors.turquoise,  bg = 'NONE' })  -- List type
  highlight(0, 'dhallOptional',            { fg = colors.turquoise,  bg = 'NONE' })  -- Optional type
  highlight(0, 'dhallDate',                { fg = colors.turquoise,  bg = 'NONE' })  -- Date type
  highlight(0, 'dhallTime',                { fg = colors.turquoise,  bg = 'NONE' })  -- Time type
  highlight(0, 'dhallTimeZone',            { fg = colors.turquoise,  bg = 'NONE' })  -- TimeZone type
  highlight(0, 'dhallBytes',               { fg = colors.turquoise,  bg = 'NONE' })  -- Bytes type

  -- Type Universe
  highlight(0, 'dhallTypeKeyword',         { fg = colors.turquoise,  bg = 'NONE' })  -- Type
  highlight(0, 'dhallKind',                { fg = colors.turquoise,  bg = 'NONE' })  -- Kind
  highlight(0, 'dhallSort',                { fg = colors.turquoise,  bg = 'NONE' })  -- Sort

  -- Boolean Constants
  highlight(0, 'dhallBoolean',             { fg = colors.blue,       bg = 'NONE' })  -- True, False
  highlight(0, 'dhallTrue',                { fg = colors.blue,       bg = 'NONE' })  -- True
  highlight(0, 'dhallFalse',               { fg = colors.blue,       bg = 'NONE' })  -- False

  -- Optional Constructors
  highlight(0, 'dhallSome',                { fg = colors.blue,       bg = 'NONE' })  -- Some
  highlight(0, 'dhallNone',                { fg = colors.blue,       bg = 'NONE' })  -- None

  -- Numbers
  highlight(0, 'dhallNumber',              { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, 'dhallNaturalLit',          { fg = colors.greenLight, bg = 'NONE' })  -- 42, 0xFF, 0b1010
  highlight(0, 'dhallIntegerLit',          { fg = colors.greenLight, bg = 'NONE' })  -- +5, -3
  highlight(0, 'dhallDoubleLit',           { fg = colors.greenLight, bg = 'NONE' })  -- 3.14, 1e6
  highlight(0, 'dhallHexNumber',           { fg = colors.greenLight, bg = 'NONE' })  -- 0xFF
  highlight(0, 'dhallBinaryNumber',        { fg = colors.greenLight, bg = 'NONE' })  -- 0b1010

  -- Strings
  highlight(0, 'dhallString',              { fg = colors.redLight,   bg = 'NONE' })  -- "string"
  highlight(0, 'dhallStringDouble',        { fg = colors.redLight,   bg = 'NONE' })  -- "double quoted"
  highlight(0, 'dhallStringMultiline',     { fg = colors.redLight,   bg = 'NONE' })  -- ''multi-line''
  highlight(0, 'dhallTextLiteral',         { fg = colors.redLight,   bg = 'NONE' })  -- Text content

  -- String Interpolation
  highlight(0, 'dhallInterpolation',       { fg = colors.purple,     bg = 'NONE' })  -- ${...}
  highlight(0, 'dhallInterpDelim',         { fg = colors.pink,       bg = 'NONE' })  -- ${ and }

  -- Escape Sequences
  highlight(0, 'dhallEscape',              { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, \\, etc.
  highlight(0, 'dhallUnicodeEscape',       { fg = colors.pink,       bg = 'NONE' })  -- \uXXXX

  -- Labels and Identifiers
  highlight(0, 'dhallLabel',               { fg = colors.white,      bg = 'NONE' })  -- Variable names
  highlight(0, 'dhallIdentifier',          { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'dhallVariable',            { fg = colors.white,      bg = 'NONE' })  -- Variables

  -- Record Fields
  highlight(0, 'dhallField',               { fg = colors.blue,       bg = 'NONE' })  -- Field names
  highlight(0, 'dhallFieldName',           { fg = colors.blue,       bg = 'NONE' })  -- record.field
  highlight(0, 'dhallRecordKey',           { fg = colors.blue,       bg = 'NONE' })  -- { key : Type }
  highlight(0, 'dhallSelector',            { fg = colors.blue,       bg = 'NONE' })  -- .field selector

  -- Record/Union Syntax
  highlight(0, 'dhallRecord',              { fg = colors.white,      bg = 'NONE' })  -- { } record delimiters
  highlight(0, 'dhallRecordBraces',        { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'dhallUnion',               { fg = colors.turquoise,  bg = 'NONE' })  -- < > union type
  highlight(0, 'dhallUnionBrackets',       { fg = colors.white,      bg = 'NONE' })  -- < >
  highlight(0, 'dhallAlternative',         { fg = colors.turquoise,  bg = 'NONE' })  -- Union alternatives

  -- Operators
  highlight(0, 'dhallOperator',            { fg = colors.white,      bg = 'NONE' })  -- General operators

  -- Arithmetic Operators
  highlight(0, 'dhallArithmeticOp',        { fg = colors.white,      bg = 'NONE' })  -- +, *

  -- Logical Operators
  highlight(0, 'dhallLogicalOp',           { fg = colors.white,      bg = 'NONE' })  -- &&, ||, ==, !=

  -- Text Operators
  highlight(0, 'dhallTextConcat',          { fg = colors.white,      bg = 'NONE' })  -- ++

  -- List Operators
  highlight(0, 'dhallListConcat',          { fg = colors.white,      bg = 'NONE' })  -- #

  -- Record Operators
  highlight(0, 'dhallRecordMerge',         { fg = colors.pink,       bg = 'NONE' })  -- /\ ∧ (recursive merge)
  highlight(0, 'dhallRecordOverride',      { fg = colors.pink,       bg = 'NONE' })  -- // ⫽ (prefer right)
  highlight(0, 'dhallRecordTypeMerge',     { fg = colors.pink,       bg = 'NONE' })  -- //\ ⩓ (type merge)
  highlight(0, 'dhallRecordComplete',      { fg = colors.pink,       bg = 'NONE' })  -- :: (auto-complete)

  -- Assert Operators
  highlight(0, 'dhallAssertOp',            { fg = colors.pink,       bg = 'NONE' })  -- ===, ≡

  -- Import Operators
  highlight(0, 'dhallImportAlt',           { fg = colors.pink,       bg = 'NONE' })  -- ? (fallback)

  -- Type Annotation
  highlight(0, 'dhallTypeAnnotation',      { fg = colors.white,      bg = 'NONE' })  -- : in type annotations

  -- Delimiters and Punctuation
  highlight(0, 'dhallParens',              { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'dhallBrackets',            { fg = colors.white,      bg = 'NONE' })  -- [ ] < >
  highlight(0, 'dhallBraces',              { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'dhallComma',               { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'dhallPipe',                { fg = colors.white,      bg = 'NONE' })  -- | (union separator)
  highlight(0, 'dhallColon',               { fg = colors.white,      bg = 'NONE' })  -- :
  highlight(0, 'dhallDot',                 { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'dhallEquals',              { fg = colors.white,      bg = 'NONE' })  -- =

  -- De Bruijn Index
  highlight(0, 'dhallIndex',               { fg = colors.purple,     bg = 'NONE' })  -- @N index


  -----------------------------------------------------------------------------
  -- Built-in Functions

  -- Natural Functions
  highlight(0, 'dhallBuiltinFunc',         { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions
  highlight(0, 'dhallNaturalFunc',         { fg = colors.orange,     bg = 'NONE' })  -- Natural/* functions
  highlight(0, 'dhallNaturalBuild',        { fg = colors.orange,     bg = 'NONE' })  -- Natural/build
  highlight(0, 'dhallNaturalFold',         { fg = colors.orange,     bg = 'NONE' })  -- Natural/fold
  highlight(0, 'dhallNaturalIsZero',       { fg = colors.orange,     bg = 'NONE' })  -- Natural/isZero
  highlight(0, 'dhallNaturalEven',         { fg = colors.orange,     bg = 'NONE' })  -- Natural/even
  highlight(0, 'dhallNaturalOdd',          { fg = colors.orange,     bg = 'NONE' })  -- Natural/odd
  highlight(0, 'dhallNaturalToInteger',    { fg = colors.orange,     bg = 'NONE' })  -- Natural/toInteger
  highlight(0, 'dhallNaturalShow',         { fg = colors.orange,     bg = 'NONE' })  -- Natural/show
  highlight(0, 'dhallNaturalSubtract',     { fg = colors.orange,     bg = 'NONE' })  -- Natural/subtract

  -- Integer Functions
  highlight(0, 'dhallIntegerFunc',         { fg = colors.orange,     bg = 'NONE' })  -- Integer/* functions
  highlight(0, 'dhallIntegerClamp',        { fg = colors.orange,     bg = 'NONE' })  -- Integer/clamp
  highlight(0, 'dhallIntegerNegate',       { fg = colors.orange,     bg = 'NONE' })  -- Integer/negate
  highlight(0, 'dhallIntegerShow',         { fg = colors.orange,     bg = 'NONE' })  -- Integer/show
  highlight(0, 'dhallIntegerToDouble',     { fg = colors.orange,     bg = 'NONE' })  -- Integer/toDouble

  -- Double Functions
  highlight(0, 'dhallDoubleFunc',          { fg = colors.orange,     bg = 'NONE' })  -- Double/* functions
  highlight(0, 'dhallDoubleShow',          { fg = colors.orange,     bg = 'NONE' })  -- Double/show

  -- Text Functions
  highlight(0, 'dhallTextFunc',            { fg = colors.orange,     bg = 'NONE' })  -- Text/* functions
  highlight(0, 'dhallTextShow',            { fg = colors.orange,     bg = 'NONE' })  -- Text/show
  highlight(0, 'dhallTextReplace',         { fg = colors.orange,     bg = 'NONE' })  -- Text/replace

  -- List Functions
  highlight(0, 'dhallListFunc',            { fg = colors.orange,     bg = 'NONE' })  -- List/* functions
  highlight(0, 'dhallListBuild',           { fg = colors.orange,     bg = 'NONE' })  -- List/build
  highlight(0, 'dhallListFold',            { fg = colors.orange,     bg = 'NONE' })  -- List/fold
  highlight(0, 'dhallListLength',          { fg = colors.orange,     bg = 'NONE' })  -- List/length
  highlight(0, 'dhallListHead',            { fg = colors.orange,     bg = 'NONE' })  -- List/head
  highlight(0, 'dhallListLast',            { fg = colors.orange,     bg = 'NONE' })  -- List/last
  highlight(0, 'dhallListIndexed',         { fg = colors.orange,     bg = 'NONE' })  -- List/indexed
  highlight(0, 'dhallListReverse',         { fg = colors.orange,     bg = 'NONE' })  -- List/reverse

  -- Date/Time Functions
  highlight(0, 'dhallDateFunc',            { fg = colors.orange,     bg = 'NONE' })  -- Date/* functions
  highlight(0, 'dhallDateShow',            { fg = colors.orange,     bg = 'NONE' })  -- Date/show
  highlight(0, 'dhallTimeFunc',            { fg = colors.orange,     bg = 'NONE' })  -- Time/* functions
  highlight(0, 'dhallTimeShow',            { fg = colors.orange,     bg = 'NONE' })  -- Time/show
  highlight(0, 'dhallTimeZoneFunc',        { fg = colors.orange,     bg = 'NONE' })  -- TimeZone/* functions
  highlight(0, 'dhallTimeZoneShow',        { fg = colors.orange,     bg = 'NONE' })  -- TimeZone/show


  -----------------------------------------------------------------------------
  -- Imports

  -- Import Types
  highlight(0, 'dhallImport',              { fg = colors.turquoise,  bg = 'NONE' })  -- Imports
  highlight(0, 'dhallImportPath',          { fg = colors.redLight,   bg = 'NONE' })  -- ./path/file.dhall
  highlight(0, 'dhallImportRelative',      { fg = colors.redLight,   bg = 'NONE' })  -- ./relative
  highlight(0, 'dhallImportAbsolute',      { fg = colors.redLight,   bg = 'NONE' })  -- /absolute
  highlight(0, 'dhallImportHome',          { fg = colors.redLight,   bg = 'NONE' })  -- ~/home

  -- URL Imports
  highlight(0, 'dhallUrl',                 { fg = colors.turquoise,  bg = 'NONE' })  -- URLs
  highlight(0, 'dhallUrlHttp',             { fg = colors.turquoise,  bg = 'NONE' })  -- http://...
  highlight(0, 'dhallUrlHttps',            { fg = colors.turquoise,  bg = 'NONE' })  -- https://...

  -- Environment Variable Imports
  highlight(0, 'dhallEnvImport',           { fg = colors.purple,     bg = 'NONE' })  -- env:VAR

  -- Import Hash (Integrity Check)
  highlight(0, 'dhallHash',                { fg = colors.gray,       bg = 'NONE' })  -- sha256:...
  highlight(0, 'dhallHashPrefix',          { fg = colors.pink,       bg = 'NONE' })  -- sha256:
  highlight(0, 'dhallHashValue',           { fg = colors.gray,       bg = 'NONE' })  -- hex digits

  -- Import Modifiers
  highlight(0, 'dhallImportAs',            { fg = colors.pink,       bg = 'NONE' })  -- as Text, as Bytes, as Location
  highlight(0, 'dhallAsText',              { fg = colors.pink,       bg = 'NONE' })  -- as Text
  highlight(0, 'dhallAsBytes',             { fg = colors.pink,       bg = 'NONE' })  -- as Bytes
  highlight(0, 'dhallAsLocation',          { fg = colors.pink,       bg = 'NONE' })  -- as Location


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.dhall)

  -- Comments
  highlight(0, '@comment.dhall',               { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Keywords
  highlight(0, '@keyword.dhall',               { fg = colors.pink,       bg = 'NONE' })  -- let, in, assert
  highlight(0, '@keyword.operator.dhall',      { fg = colors.pink,       bg = 'NONE' })  -- using, as, with
  highlight(0, '@keyword.conditional.dhall',   { fg = colors.pink,       bg = 'NONE' })  -- if, then, else
  highlight(0, '@keyword.import.dhall',        { fg = colors.pink,       bg = 'NONE' })  -- missing

  -- Types
  highlight(0, '@type.dhall',                  { fg = colors.turquoise,  bg = 'NONE' })  -- Type labels, import modifiers
  highlight(0, '@type.builtin.dhall',          { fg = colors.turquoise,  bg = 'NONE' })  -- Bool, Natural, List, etc.

  -- Variables
  highlight(0, '@variable.dhall',              { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, '@variable.parameter.dhall',    { fg = colors.purple,     bg = 'NONE' })  -- Lambda parameters
  highlight(0, '@variable.member.dhall',       { fg = colors.blue,       bg = 'NONE' })  -- Record/selector members

  -- Functions
  highlight(0, '@function.dhall',              { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@function.builtin.dhall',      { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Strings
  highlight(0, '@string.dhall',                { fg = colors.redLight,   bg = 'NONE' })  -- Text literals
  highlight(0, '@string.escape.dhall',         { fg = colors.pink,       bg = 'NONE' })  -- Escaped quotes
  highlight(0, '@string.special.dhall',        { fg = colors.purple,     bg = 'NONE' })  -- env:, hash
  highlight(0, '@string.special.path.dhall',   { fg = colors.redLight,   bg = 'NONE' })  -- File paths
  highlight(0, '@string.special.url.dhall',    { fg = colors.turquoise,  bg = 'NONE' })  -- HTTP imports

  -- Numbers
  highlight(0, '@number.dhall',                { fg = colors.greenLight, bg = 'NONE' })  -- Natural, Integer
  highlight(0, '@number.float.dhall',          { fg = colors.greenLight, bg = 'NONE' })  -- Double

  -- Booleans
  highlight(0, '@boolean.dhall',               { fg = colors.blue,       bg = 'NONE' })  -- True, False

  -- Constants
  highlight(0, '@constant.dhall',              { fg = colors.blue,       bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.dhall',      { fg = colors.blue,       bg = 'NONE' })  -- None

  -- Operators
  highlight(0, '@operator.dhall',              { fg = colors.white,      bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.dhall',   { fg = colors.white,      bg = 'NONE' })  -- {}, [], (), <>
  highlight(0, '@punctuation.delimiter.dhall', { fg = colors.white,      bg = 'NONE' })  -- , | .


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.dhall)

  highlight(0, '@lsp.type.type.dhall',         { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.variable.dhall',     { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.dhall',    { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.dhall',     { fg = colors.blue,       bg = 'NONE' })  -- Fields
  highlight(0, '@lsp.type.function.dhall',     { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.string.dhall',       { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.dhall',       { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.keyword.dhall',      { fg = colors.pink,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.comment.dhall',      { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.namespace.dhall',    { fg = colors.turquoise,  bg = 'NONE' })  -- Namespaces


  -----------------------------------------------------------------------------
  -- Unicode Operators (Dhall supports Unicode alternatives)

  highlight(0, 'dhallUnicodeLambda',       { fg = colors.pink,       bg = 'NONE' })  -- λ
  highlight(0, 'dhallUnicodeForall',       { fg = colors.pink,       bg = 'NONE' })  -- ∀
  highlight(0, 'dhallUnicodeArrow',        { fg = colors.pink,       bg = 'NONE' })  -- →
  highlight(0, 'dhallUnicodeMerge',        { fg = colors.pink,       bg = 'NONE' })  -- ∧
  highlight(0, 'dhallUnicodePrefer',       { fg = colors.pink,       bg = 'NONE' })  -- ⫽
  highlight(0, 'dhallUnicodeTypeMerge',    { fg = colors.pink,       bg = 'NONE' })  -- ⩓
  highlight(0, 'dhallUnicodeEquiv',        { fg = colors.pink,       bg = 'NONE' })  -- ≡


  -----------------------------------------------------------------------------
  -- Prelude (Standard Library)

  highlight(0, 'dhallPrelude',             { fg = colors.purple,     bg = 'NONE' })  -- Prelude
  highlight(0, 'dhallPreludeFunc',         { fg = colors.orange,     bg = 'NONE' })  -- Prelude functions

  -- Prelude/Bool
  highlight(0, 'dhallPreludeBool',         { fg = colors.orange,     bg = 'NONE' })  -- Bool functions
  highlight(0, 'dhallBoolAnd',             { fg = colors.orange,     bg = 'NONE' })  -- Bool/and
  highlight(0, 'dhallBoolOr',              { fg = colors.orange,     bg = 'NONE' })  -- Bool/or
  highlight(0, 'dhallBoolNot',             { fg = colors.orange,     bg = 'NONE' })  -- Bool/not
  highlight(0, 'dhallBoolShow',            { fg = colors.orange,     bg = 'NONE' })  -- Bool/show

  -- Prelude/Natural
  highlight(0, 'dhallPreludeNatural',      { fg = colors.orange,     bg = 'NONE' })  -- Natural functions
  highlight(0, 'dhallNaturalSum',          { fg = colors.orange,     bg = 'NONE' })  -- Natural/sum
  highlight(0, 'dhallNaturalProduct',      { fg = colors.orange,     bg = 'NONE' })  -- Natural/product
  highlight(0, 'dhallNaturalMin',          { fg = colors.orange,     bg = 'NONE' })  -- Natural/min
  highlight(0, 'dhallNaturalMax',          { fg = colors.orange,     bg = 'NONE' })  -- Natural/max
  highlight(0, 'dhallNaturalEqual',        { fg = colors.orange,     bg = 'NONE' })  -- Natural/equal
  highlight(0, 'dhallNaturalGreaterThan',  { fg = colors.orange,     bg = 'NONE' })  -- Natural/greaterThan
  highlight(0, 'dhallNaturalLessThan',     { fg = colors.orange,     bg = 'NONE' })  -- Natural/lessThan

  -- Prelude/List
  highlight(0, 'dhallPreludeList',         { fg = colors.orange,     bg = 'NONE' })  -- List functions
  highlight(0, 'dhallListMap',             { fg = colors.orange,     bg = 'NONE' })  -- List/map
  highlight(0, 'dhallListFilter',          { fg = colors.orange,     bg = 'NONE' })  -- List/filter
  highlight(0, 'dhallListConcat',          { fg = colors.orange,     bg = 'NONE' })  -- List/concat
  highlight(0, 'dhallListConcatMap',       { fg = colors.orange,     bg = 'NONE' })  -- List/concatMap
  highlight(0, 'dhallListGenerate',        { fg = colors.orange,     bg = 'NONE' })  -- List/generate
  highlight(0, 'dhallListIterate',         { fg = colors.orange,     bg = 'NONE' })  -- List/iterate
  highlight(0, 'dhallListReplicate',       { fg = colors.orange,     bg = 'NONE' })  -- List/replicate
  highlight(0, 'dhallListTake',            { fg = colors.orange,     bg = 'NONE' })  -- List/take
  highlight(0, 'dhallListDrop',            { fg = colors.orange,     bg = 'NONE' })  -- List/drop
  highlight(0, 'dhallListNull',            { fg = colors.orange,     bg = 'NONE' })  -- List/null
  highlight(0, 'dhallListAll',             { fg = colors.orange,     bg = 'NONE' })  -- List/all
  highlight(0, 'dhallListAny',             { fg = colors.orange,     bg = 'NONE' })  -- List/any
  highlight(0, 'dhallListUnzip',           { fg = colors.orange,     bg = 'NONE' })  -- List/unzip

  -- Prelude/Optional
  highlight(0, 'dhallPreludeOptional',     { fg = colors.orange,     bg = 'NONE' })  -- Optional functions
  highlight(0, 'dhallOptionalMap',         { fg = colors.orange,     bg = 'NONE' })  -- Optional/map
  highlight(0, 'dhallOptionalFold',        { fg = colors.orange,     bg = 'NONE' })  -- Optional/fold
  highlight(0, 'dhallOptionalBuild',       { fg = colors.orange,     bg = 'NONE' })  -- Optional/build
  highlight(0, 'dhallOptionalDefault',     { fg = colors.orange,     bg = 'NONE' })  -- Optional/default
  highlight(0, 'dhallOptionalNull',        { fg = colors.orange,     bg = 'NONE' })  -- Optional/null

  -- Prelude/Text
  highlight(0, 'dhallPreludeText',         { fg = colors.orange,     bg = 'NONE' })  -- Text functions
  highlight(0, 'dhallTextConcat',          { fg = colors.orange,     bg = 'NONE' })  -- Text/concat
  highlight(0, 'dhallTextConcatSep',       { fg = colors.orange,     bg = 'NONE' })  -- Text/concatSep
  highlight(0, 'dhallTextConcatMap',       { fg = colors.orange,     bg = 'NONE' })  -- Text/concatMap
  highlight(0, 'dhallTextConcatMapSep',    { fg = colors.orange,     bg = 'NONE' })  -- Text/concatMapSep
  highlight(0, 'dhallTextDefault',         { fg = colors.orange,     bg = 'NONE' })  -- Text/default

  -- Prelude/Function
  highlight(0, 'dhallPreludeFunction',     { fg = colors.orange,     bg = 'NONE' })  -- Function functions
  highlight(0, 'dhallFunctionCompose',     { fg = colors.orange,     bg = 'NONE' })  -- Function/compose
  highlight(0, 'dhallFunctionIdentity',    { fg = colors.orange,     bg = 'NONE' })  -- Function/identity

  -- Prelude/JSON
  highlight(0, 'dhallPreludeJSON',         { fg = colors.orange,     bg = 'NONE' })  -- JSON rendering
  highlight(0, 'dhallJSONRender',          { fg = colors.orange,     bg = 'NONE' })  -- JSON/render

  -- Prelude/Map
  highlight(0, 'dhallPreludeMap',          { fg = colors.orange,     bg = 'NONE' })  -- Map type and functions
  highlight(0, 'dhallMapType',             { fg = colors.turquoise,  bg = 'NONE' })  -- Map type
  highlight(0, 'dhallMapEntry',            { fg = colors.turquoise,  bg = 'NONE' })  -- Map.Entry


  -----------------------------------------------------------------------------
  -- Common Dhall Patterns (Kubernetes, etc.)

  -- Kubernetes Dhall
  highlight(0, 'dhallK8s',                 { fg = colors.purple,     bg = 'NONE' })  -- kubernetes package
  highlight(0, 'dhallK8sDeployment',       { fg = colors.turquoise,  bg = 'NONE' })  -- Deployment
  highlight(0, 'dhallK8sService',          { fg = colors.turquoise,  bg = 'NONE' })  -- Service
  highlight(0, 'dhallK8sConfigMap',        { fg = colors.turquoise,  bg = 'NONE' })  -- ConfigMap
  highlight(0, 'dhallK8sSecret',           { fg = colors.turquoise,  bg = 'NONE' })  -- Secret
  highlight(0, 'dhallK8sIngress',          { fg = colors.turquoise,  bg = 'NONE' })  -- Ingress
  highlight(0, 'dhallK8sNamespace',        { fg = colors.turquoise,  bg = 'NONE' })  -- Namespace
  highlight(0, 'dhallK8sContainer',        { fg = colors.turquoise,  bg = 'NONE' })  -- Container
  highlight(0, 'dhallK8sPod',              { fg = colors.turquoise,  bg = 'NONE' })  -- Pod


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'dhallSyntaxError',         { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'dhallTypeError',           { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Type errors
  highlight(0, 'dhallImportError',         { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Import errors
end

return dhall
