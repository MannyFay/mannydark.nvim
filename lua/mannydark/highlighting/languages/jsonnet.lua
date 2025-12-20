-------------------------------------------------------------------------------
-- Jsonnet Files
-- Highlighting for .jsonnet, .libsonnet files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local jsonnet   = {}


-------------------------------------------------------------------------------
-- Settings

jsonnet.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Comments
  highlight(0, 'jsonnetComment',           { link = "Comment" })  -- // and # comments
  highlight(0, 'jsonnetBlockComment',      { link = "Comment" })  -- /* */ block comments
  highlight(0, 'jsonnetTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Keywords
  highlight(0, 'jsonnetKeyword',           { link = "Keyword" })  -- local, function, etc.
  highlight(0, 'jsonnetLocal',             { fg = colors.pink,       bg = 'NONE' })  -- local
  highlight(0, 'jsonnetFunction',          { link = "Function" })  -- function
  highlight(0, 'jsonnetTailstrict',        { fg = colors.pink,       bg = 'NONE' })  -- tailstrict

  -- Control Flow
  highlight(0, 'jsonnetConditional',       { link = "Conditional" })  -- if, then, else
  highlight(0, 'jsonnetIf',                { fg = colors.pink,       bg = 'NONE' })  -- if
  highlight(0, 'jsonnetThen',              { fg = colors.pink,       bg = 'NONE' })  -- then
  highlight(0, 'jsonnetElse',              { fg = colors.pink,       bg = 'NONE' })  -- else
  highlight(0, 'jsonnetRepeat',            { fg = colors.pink,       bg = 'NONE' })  -- for
  highlight(0, 'jsonnetFor',               { fg = colors.pink,       bg = 'NONE' })  -- for
  highlight(0, 'jsonnetIn',                { fg = colors.pink,       bg = 'NONE' })  -- in

  -- Error/Assert
  highlight(0, 'jsonnetAssert',            { fg = colors.pink,       bg = 'NONE' })  -- assert
  highlight(0, 'jsonnetError',             { fg = colors.pink,       bg = 'NONE' })  -- error

  -- Imports
  highlight(0, 'jsonnetImport',            { fg = colors.pink,       bg = 'NONE' })  -- import
  highlight(0, 'jsonnetImportStr',         { fg = colors.pink,       bg = 'NONE' })  -- importstr
  highlight(0, 'jsonnetImportBin',         { fg = colors.pink,       bg = 'NONE' })  -- importbin

  -- Built-in Variables
  highlight(0, 'jsonnetSelf',              { fg = colors.purple,     bg = 'NONE' })  -- self
  highlight(0, 'jsonnetSuper',             { fg = colors.purple,     bg = 'NONE' })  -- super
  highlight(0, 'jsonnetDollar',            { fg = colors.purple,     bg = 'NONE' })  -- $ (outer self)

  -- Boolean and Null
  highlight(0, 'jsonnetBoolean',           { link = "Boolean" })  -- true, false
  highlight(0, 'jsonnetTrue',              { fg = colors.blue,       bg = 'NONE' })  -- true
  highlight(0, 'jsonnetFalse',             { fg = colors.blue,       bg = 'NONE' })  -- false
  highlight(0, 'jsonnetNull',              { fg = colors.blue,       bg = 'NONE' })  -- null

  -- Numbers
  highlight(0, 'jsonnetNumber',            { link = "Number" })  -- Numbers
  highlight(0, 'jsonnetInteger',           { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'jsonnetFloat',             { fg = colors.greenLight, bg = 'NONE' })  -- Floats
  highlight(0, 'jsonnetExponent',          { fg = colors.greenLight, bg = 'NONE' })  -- Scientific notation

  -- Strings
  highlight(0, 'jsonnetString',            { link = "String" })  -- "string" and 'string'
  highlight(0, 'jsonnetStringDouble',      { link = "String" })  -- "double quoted"
  highlight(0, 'jsonnetStringSingle',      { link = "String" })  -- 'single quoted'
  highlight(0, 'jsonnetTextBlock',         { fg = colors.redLight,   bg = 'NONE' })  -- ||| text blocks
  highlight(0, 'jsonnetTextBlockDelim',    { link = "Delimiter" })  -- ||| delimiters
  highlight(0, 'jsonnetVerbatimString',    { link = "String" })  -- @"verbatim string"

  -- Escape Sequences
  highlight(0, 'jsonnetEscape',            { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, \\, etc.
  highlight(0, 'jsonnetUnicodeEscape',     { fg = colors.pink,       bg = 'NONE' })  -- \uXXXX

  -- String Formatting (Python-style)
  highlight(0, 'jsonnetFormat',            { fg = colors.pink,       bg = 'NONE' })  -- %s, %d, %f, etc.
  highlight(0, 'jsonnetFormatSpec',        { fg = colors.pink,       bg = 'NONE' })  -- Format specifiers

  -- Fields and Properties
  highlight(0, 'jsonnetField',             { fg = colors.blue,       bg = 'NONE' })  -- Field names
  highlight(0, 'jsonnetFieldName',         { fg = colors.blue,       bg = 'NONE' })  -- Object keys
  highlight(0, 'jsonnetFieldExpr',         { fg = colors.blue,       bg = 'NONE' })  -- [computed] field names

  -- Field Visibility Operators
  highlight(0, 'jsonnetFieldColon',        { fg = colors.white,      bg = 'NONE' })  -- : (default visibility)
  highlight(0, 'jsonnetFieldHidden',       { fg = colors.pink,       bg = 'NONE' })  -- :: (hidden)
  highlight(0, 'jsonnetFieldForced',       { fg = colors.pink,       bg = 'NONE' })  -- ::: (forced visible)

  -- Field Inheritance Operators
  highlight(0, 'jsonnetFieldInherit',      { fg = colors.pink,       bg = 'NONE' })  -- +: (inherit default)
  highlight(0, 'jsonnetFieldInheritHidden',{ fg = colors.pink,       bg = 'NONE' })  -- +:: (inherit hidden)
  highlight(0, 'jsonnetFieldInheritForced',{ fg = colors.pink,       bg = 'NONE' })  -- +::: (inherit forced)

  -- Operators
  highlight(0, 'jsonnetOperator',          { link = "Operator" })  -- General operators
  highlight(0, 'jsonnetArithmeticOp',      { fg = colors.white,      bg = 'NONE' })  -- + - * / %
  highlight(0, 'jsonnetComparisonOp',      { fg = colors.white,      bg = 'NONE' })  -- == != < > <= >=
  highlight(0, 'jsonnetLogicalOp',         { fg = colors.white,      bg = 'NONE' })  -- && || !
  highlight(0, 'jsonnetBitwiseOp',         { fg = colors.white,      bg = 'NONE' })  -- & | ^ ~ << >>
  highlight(0, 'jsonnetUnaryOp',           { fg = colors.white,      bg = 'NONE' })  -- ! - + ~

  -- Delimiters and Punctuation
  highlight(0, 'jsonnetBraces',            { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'jsonnetBrackets',          { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'jsonnetParens',            { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'jsonnetComma',             { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'jsonnetSemicolon',         { fg = colors.white,      bg = 'NONE' })  -- ;
  highlight(0, 'jsonnetDot',               { fg = colors.white,      bg = 'NONE' })  -- .

  -- Functions
  highlight(0, 'jsonnetFunctionCall',      { link = "Function" })  -- Function calls
  highlight(0, 'jsonnetFunctionDef',       { link = "Function" })  -- Function definitions
  highlight(0, 'jsonnetParameter',         { fg = colors.purple,     bg = 'NONE' })  -- Function parameters
  highlight(0, 'jsonnetNamedArg',          { fg = colors.blue,       bg = 'NONE' })  -- Named arguments

  -- Variables
  highlight(0, 'jsonnetVariable',          { link = "Variable" })  -- Variables
  highlight(0, 'jsonnetIdentifier',        { fg = colors.white,      bg = 'NONE' })  -- Identifiers


  -----------------------------------------------------------------------------
  -- Standard Library (std.*)

  -- std object
  highlight(0, 'jsonnetStd',               { fg = colors.purple,     bg = 'NONE' })  -- std

  -- Type/Reflection Functions
  highlight(0, 'jsonnetStdType',           { link = "Type" })  -- std.type
  highlight(0, 'jsonnetStdIsArray',        { fg = colors.orange,     bg = 'NONE' })  -- std.isArray
  highlight(0, 'jsonnetStdIsBoolean',      { link = "Boolean" })  -- std.isBoolean
  highlight(0, 'jsonnetStdIsFunction',     { link = "Function" })  -- std.isFunction
  highlight(0, 'jsonnetStdIsNumber',       { link = "Number" })  -- std.isNumber
  highlight(0, 'jsonnetStdIsObject',       { fg = colors.orange,     bg = 'NONE' })  -- std.isObject
  highlight(0, 'jsonnetStdIsString',       { link = "String" })  -- std.isString
  highlight(0, 'jsonnetStdLength',         { fg = colors.orange,     bg = 'NONE' })  -- std.length
  highlight(0, 'jsonnetStdPrune',          { fg = colors.orange,     bg = 'NONE' })  -- std.prune
  highlight(0, 'jsonnetStdThisFile',       { fg = colors.orange,     bg = 'NONE' })  -- std.thisFile

  -- Math Functions
  highlight(0, 'jsonnetStdMath',           { fg = colors.orange,     bg = 'NONE' })  -- Math functions
  highlight(0, 'jsonnetStdAbs',            { fg = colors.orange,     bg = 'NONE' })  -- std.abs
  highlight(0, 'jsonnetStdSign',           { fg = colors.orange,     bg = 'NONE' })  -- std.sign
  highlight(0, 'jsonnetStdMax',            { fg = colors.orange,     bg = 'NONE' })  -- std.max
  highlight(0, 'jsonnetStdMin',            { fg = colors.orange,     bg = 'NONE' })  -- std.min
  highlight(0, 'jsonnetStdPow',            { fg = colors.orange,     bg = 'NONE' })  -- std.pow
  highlight(0, 'jsonnetStdExp',            { fg = colors.orange,     bg = 'NONE' })  -- std.exp
  highlight(0, 'jsonnetStdLog',            { fg = colors.orange,     bg = 'NONE' })  -- std.log
  highlight(0, 'jsonnetStdSqrt',           { fg = colors.orange,     bg = 'NONE' })  -- std.sqrt
  highlight(0, 'jsonnetStdFloor',          { fg = colors.orange,     bg = 'NONE' })  -- std.floor
  highlight(0, 'jsonnetStdCeil',           { fg = colors.orange,     bg = 'NONE' })  -- std.ceil
  highlight(0, 'jsonnetStdRound',          { fg = colors.orange,     bg = 'NONE' })  -- std.round
  highlight(0, 'jsonnetStdSin',            { fg = colors.orange,     bg = 'NONE' })  -- std.sin
  highlight(0, 'jsonnetStdCos',            { fg = colors.orange,     bg = 'NONE' })  -- std.cos
  highlight(0, 'jsonnetStdTan',            { fg = colors.orange,     bg = 'NONE' })  -- std.tan
  highlight(0, 'jsonnetStdAsin',           { fg = colors.orange,     bg = 'NONE' })  -- std.asin
  highlight(0, 'jsonnetStdAcos',           { fg = colors.orange,     bg = 'NONE' })  -- std.acos
  highlight(0, 'jsonnetStdAtan',           { fg = colors.orange,     bg = 'NONE' })  -- std.atan
  highlight(0, 'jsonnetStdMod',            { fg = colors.orange,     bg = 'NONE' })  -- std.mod
  highlight(0, 'jsonnetStdClamp',          { fg = colors.orange,     bg = 'NONE' })  -- std.clamp
  highlight(0, 'jsonnetStdPi',             { fg = colors.greenLight, bg = 'NONE' })  -- std.pi (constant)

  -- String Functions
  highlight(0, 'jsonnetStdString',         { link = "String" })  -- String functions
  highlight(0, 'jsonnetStdToString',       { link = "String" })  -- std.toString
  highlight(0, 'jsonnetStdCodepoint',      { fg = colors.orange,     bg = 'NONE' })  -- std.codepoint
  highlight(0, 'jsonnetStdChar',           { fg = colors.orange,     bg = 'NONE' })  -- std.char
  highlight(0, 'jsonnetStdSubstr',         { link = "Variable" })  -- std.substr
  highlight(0, 'jsonnetStdFindSubstr',     { link = "Variable" })  -- std.findSubstr
  highlight(0, 'jsonnetStdStartsWith',     { fg = colors.orange,     bg = 'NONE' })  -- std.startsWith
  highlight(0, 'jsonnetStdEndsWith',       { fg = colors.orange,     bg = 'NONE' })  -- std.endsWith
  highlight(0, 'jsonnetStdSplit',          { fg = colors.orange,     bg = 'NONE' })  -- std.split
  highlight(0, 'jsonnetStdSplitLimit',     { fg = colors.orange,     bg = 'NONE' })  -- std.splitLimit
  highlight(0, 'jsonnetStdStrReplace',     { fg = colors.orange,     bg = 'NONE' })  -- std.strReplace
  highlight(0, 'jsonnetStdAsciiUpper',     { fg = colors.orange,     bg = 'NONE' })  -- std.asciiUpper
  highlight(0, 'jsonnetStdAsciiLower',     { fg = colors.orange,     bg = 'NONE' })  -- std.asciiLower
  highlight(0, 'jsonnetStdTrim',           { fg = colors.orange,     bg = 'NONE' })  -- std.trim
  highlight(0, 'jsonnetStdStripChars',     { fg = colors.orange,     bg = 'NONE' })  -- std.stripChars
  highlight(0, 'jsonnetStdFormat',         { fg = colors.orange,     bg = 'NONE' })  -- std.format
  highlight(0, 'jsonnetStdIsEmpty',        { fg = colors.orange,     bg = 'NONE' })  -- std.isEmpty
  highlight(0, 'jsonnetStdStringChars',    { link = "String" })  -- std.stringChars

  -- Escape Functions
  highlight(0, 'jsonnetStdEscape',         { fg = colors.orange,     bg = 'NONE' })  -- Escape functions
  highlight(0, 'jsonnetStdEscapeJson',     { fg = colors.orange,     bg = 'NONE' })  -- std.escapeStringJson
  highlight(0, 'jsonnetStdEscapePython',   { fg = colors.orange,     bg = 'NONE' })  -- std.escapeStringPython
  highlight(0, 'jsonnetStdEscapeBash',     { fg = colors.orange,     bg = 'NONE' })  -- std.escapeStringBash
  highlight(0, 'jsonnetStdEscapeDollars',  { fg = colors.orange,     bg = 'NONE' })  -- std.escapeStringDollars
  highlight(0, 'jsonnetStdEscapeXml',      { fg = colors.orange,     bg = 'NONE' })  -- std.escapeStringXml

  -- Parsing Functions
  highlight(0, 'jsonnetStdParse',          { fg = colors.orange,     bg = 'NONE' })  -- Parse functions
  highlight(0, 'jsonnetStdParseInt',       { fg = colors.orange,     bg = 'NONE' })  -- std.parseInt
  highlight(0, 'jsonnetStdParseOctal',     { fg = colors.orange,     bg = 'NONE' })  -- std.parseOctal
  highlight(0, 'jsonnetStdParseHex',       { fg = colors.orange,     bg = 'NONE' })  -- std.parseHex
  highlight(0, 'jsonnetStdParseJson',      { fg = colors.orange,     bg = 'NONE' })  -- std.parseJson
  highlight(0, 'jsonnetStdParseYaml',      { fg = colors.orange,     bg = 'NONE' })  -- std.parseYaml

  -- Manifestation Functions
  highlight(0, 'jsonnetStdManifest',       { fg = colors.orange,     bg = 'NONE' })  -- Manifest functions
  highlight(0, 'jsonnetStdManifestJson',   { fg = colors.orange,     bg = 'NONE' })  -- std.manifestJson
  highlight(0, 'jsonnetStdManifestJsonEx', { fg = colors.orange,     bg = 'NONE' })  -- std.manifestJsonEx
  highlight(0, 'jsonnetStdManifestYaml',   { fg = colors.orange,     bg = 'NONE' })  -- std.manifestYamlDoc
  highlight(0, 'jsonnetStdManifestIni',    { fg = colors.orange,     bg = 'NONE' })  -- std.manifestIni
  highlight(0, 'jsonnetStdManifestPython', { fg = colors.orange,     bg = 'NONE' })  -- std.manifestPython
  highlight(0, 'jsonnetStdManifestXml',    { fg = colors.orange,     bg = 'NONE' })  -- std.manifestXmlJsonml
  highlight(0, 'jsonnetStdManifestToml',   { fg = colors.orange,     bg = 'NONE' })  -- std.manifestTomlEx

  -- Array Functions
  highlight(0, 'jsonnetStdArray',          { fg = colors.orange,     bg = 'NONE' })  -- Array functions
  highlight(0, 'jsonnetStdMakeArray',      { fg = colors.orange,     bg = 'NONE' })  -- std.makeArray
  highlight(0, 'jsonnetStdMember',         { fg = colors.orange,     bg = 'NONE' })  -- std.member
  highlight(0, 'jsonnetStdCount',          { fg = colors.orange,     bg = 'NONE' })  -- std.count
  highlight(0, 'jsonnetStdFind',           { fg = colors.orange,     bg = 'NONE' })  -- std.find
  highlight(0, 'jsonnetStdMap',            { fg = colors.orange,     bg = 'NONE' })  -- std.map
  highlight(0, 'jsonnetStdMapWithIndex',   { fg = colors.orange,     bg = 'NONE' })  -- std.mapWithIndex
  highlight(0, 'jsonnetStdFilterMap',      { fg = colors.orange,     bg = 'NONE' })  -- std.filterMap
  highlight(0, 'jsonnetStdFlatMap',        { fg = colors.orange,     bg = 'NONE' })  -- std.flatMap
  highlight(0, 'jsonnetStdFilter',         { fg = colors.orange,     bg = 'NONE' })  -- std.filter
  highlight(0, 'jsonnetStdFoldl',          { fg = colors.orange,     bg = 'NONE' })  -- std.foldl
  highlight(0, 'jsonnetStdFoldr',          { fg = colors.orange,     bg = 'NONE' })  -- std.foldr
  highlight(0, 'jsonnetStdRange',          { fg = colors.orange,     bg = 'NONE' })  -- std.range
  highlight(0, 'jsonnetStdRepeat',         { fg = colors.orange,     bg = 'NONE' })  -- std.repeat
  highlight(0, 'jsonnetStdSlice',          { fg = colors.orange,     bg = 'NONE' })  -- std.slice
  highlight(0, 'jsonnetStdJoin',           { fg = colors.orange,     bg = 'NONE' })  -- std.join
  highlight(0, 'jsonnetStdDeepJoin',       { fg = colors.orange,     bg = 'NONE' })  -- std.deepJoin
  highlight(0, 'jsonnetStdLines',          { fg = colors.orange,     bg = 'NONE' })  -- std.lines
  highlight(0, 'jsonnetStdFlatten',        { fg = colors.orange,     bg = 'NONE' })  -- std.flattenArrays
  highlight(0, 'jsonnetStdReverse',        { fg = colors.orange,     bg = 'NONE' })  -- std.reverse
  highlight(0, 'jsonnetStdSort',           { fg = colors.orange,     bg = 'NONE' })  -- std.sort
  highlight(0, 'jsonnetStdUniq',           { fg = colors.orange,     bg = 'NONE' })  -- std.uniq
  highlight(0, 'jsonnetStdAll',            { fg = colors.orange,     bg = 'NONE' })  -- std.all
  highlight(0, 'jsonnetStdAny',            { fg = colors.orange,     bg = 'NONE' })  -- std.any
  highlight(0, 'jsonnetStdSum',            { fg = colors.orange,     bg = 'NONE' })  -- std.sum
  highlight(0, 'jsonnetStdAvg',            { fg = colors.orange,     bg = 'NONE' })  -- std.avg
  highlight(0, 'jsonnetStdContains',       { fg = colors.orange,     bg = 'NONE' })  -- std.contains
  highlight(0, 'jsonnetStdRemove',         { fg = colors.orange,     bg = 'NONE' })  -- std.remove
  highlight(0, 'jsonnetStdRemoveAt',       { fg = colors.orange,     bg = 'NONE' })  -- std.removeAt

  -- Set Functions
  highlight(0, 'jsonnetStdSet',            { fg = colors.orange,     bg = 'NONE' })  -- std.set
  highlight(0, 'jsonnetStdSetInter',       { fg = colors.orange,     bg = 'NONE' })  -- std.setInter
  highlight(0, 'jsonnetStdSetUnion',       { fg = colors.orange,     bg = 'NONE' })  -- std.setUnion
  highlight(0, 'jsonnetStdSetDiff',        { fg = colors.orange,     bg = 'NONE' })  -- std.setDiff
  highlight(0, 'jsonnetStdSetMember',      { fg = colors.orange,     bg = 'NONE' })  -- std.setMember

  -- Object Functions
  highlight(0, 'jsonnetStdObject',         { fg = colors.orange,     bg = 'NONE' })  -- Object functions
  highlight(0, 'jsonnetStdGet',            { fg = colors.orange,     bg = 'NONE' })  -- std.get
  highlight(0, 'jsonnetStdObjectHas',      { fg = colors.orange,     bg = 'NONE' })  -- std.objectHas
  highlight(0, 'jsonnetStdObjectHasAll',   { fg = colors.orange,     bg = 'NONE' })  -- std.objectHasAll
  highlight(0, 'jsonnetStdObjectFields',   { fg = colors.orange,     bg = 'NONE' })  -- std.objectFields
  highlight(0, 'jsonnetStdObjectFieldsAll',{ fg = colors.orange,     bg = 'NONE' })  -- std.objectFieldsAll
  highlight(0, 'jsonnetStdObjectValues',   { fg = colors.orange,     bg = 'NONE' })  -- std.objectValues
  highlight(0, 'jsonnetStdObjectKeysVals', { fg = colors.orange,     bg = 'NONE' })  -- std.objectKeysValues
  highlight(0, 'jsonnetStdObjectRemoveKey',{ fg = colors.orange,     bg = 'NONE' })  -- std.objectRemoveKey
  highlight(0, 'jsonnetStdMapWithKey',     { fg = colors.orange,     bg = 'NONE' })  -- std.mapWithKey

  -- Encoding Functions
  highlight(0, 'jsonnetStdEncoding',       { fg = colors.orange,     bg = 'NONE' })  -- Encoding functions
  highlight(0, 'jsonnetStdBase64',         { fg = colors.orange,     bg = 'NONE' })  -- std.base64
  highlight(0, 'jsonnetStdBase64Decode',   { fg = colors.orange,     bg = 'NONE' })  -- std.base64Decode
  highlight(0, 'jsonnetStdMd5',            { fg = colors.orange,     bg = 'NONE' })  -- std.md5
  highlight(0, 'jsonnetStdSha1',           { fg = colors.orange,     bg = 'NONE' })  -- std.sha1
  highlight(0, 'jsonnetStdSha256',         { fg = colors.orange,     bg = 'NONE' })  -- std.sha256
  highlight(0, 'jsonnetStdSha512',         { fg = colors.orange,     bg = 'NONE' })  -- std.sha512
  highlight(0, 'jsonnetStdSha3',           { fg = colors.orange,     bg = 'NONE' })  -- std.sha3
  highlight(0, 'jsonnetStdEncodeUTF8',     { fg = colors.orange,     bg = 'NONE' })  -- std.encodeUTF8
  highlight(0, 'jsonnetStdDecodeUTF8',     { fg = colors.orange,     bg = 'NONE' })  -- std.decodeUTF8

  -- External Variables
  highlight(0, 'jsonnetStdExtVar',         { link = "Variable" })  -- std.extVar

  -- Assertions and Debugging
  highlight(0, 'jsonnetStdAssertEqual',    { fg = colors.orange,     bg = 'NONE' })  -- std.assertEqual
  highlight(0, 'jsonnetStdTrace',          { fg = colors.orange,     bg = 'NONE' })  -- std.trace

  -- Boolean Functions
  highlight(0, 'jsonnetStdXor',            { fg = colors.orange,     bg = 'NONE' })  -- std.xor
  highlight(0, 'jsonnetStdXnor',           { fg = colors.orange,     bg = 'NONE' })  -- std.xnor

  -- Merge Functions
  highlight(0, 'jsonnetStdMergePatch',     { fg = colors.orange,     bg = 'NONE' })  -- std.mergePatch


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.jsonnet)

  -- Comments
  highlight(0, '@comment.jsonnet',             { link = "Comment" })  -- Comments
  highlight(0, '@spell.jsonnet',               { fg = colors.red,        bg = 'NONE' })  -- Spell-checkable text

  -- Keywords
  highlight(0, '@keyword.jsonnet',             { link = "Keyword" })  -- local, tailstrict, function
  highlight(0, '@keyword.function.jsonnet',    { link = "Keyword" })  -- function
  highlight(0, '@keyword.repeat.jsonnet',      { link = "Keyword" })  -- for
  highlight(0, '@keyword.operator.jsonnet',    { link = "Operator" })  -- in
  highlight(0, '@keyword.conditional.jsonnet', { link = "Conditional" })  -- if, then, else
  highlight(0, '@keyword.exception.jsonnet',   { link = "Keyword" })  -- assert, error
  highlight(0, '@keyword.import.jsonnet',      { link = "Keyword" })  -- import, importstr

  -- Variables
  highlight(0, '@variable.jsonnet',            { link = "Variable" })  -- Identifiers
  highlight(0, '@variable.builtin.jsonnet',    { link = "Variable" })  -- $, self, super, std
  highlight(0, '@variable.member.jsonnet',     { link = "Variable" })  -- Field names
  highlight(0, '@variable.parameter.jsonnet',  { link = "Variable" })  -- Function parameters

  -- Functions
  highlight(0, '@function.jsonnet',            { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.jsonnet',       { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.jsonnet',    { link = "Function" })  -- std.* functions

  -- Strings
  highlight(0, '@string.jsonnet',              { link = "String" })  -- String values
  highlight(0, '@string.escape.jsonnet',       { link = "String" })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.jsonnet',              { link = "Number" })  -- Numbers
  highlight(0, '@number.float.jsonnet',        { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.jsonnet',             { link = "Boolean" })  -- true, false

  -- Constants
  highlight(0, '@constant.jsonnet',            { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.jsonnet',    { link = "Constant" })  -- null

  -- Operators
  highlight(0, '@operator.jsonnet',            { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.jsonnet', { fg = colors.white,      bg = 'NONE' })  -- [ ] { } ( )
  highlight(0, '@punctuation.delimiter.jsonnet', { link = "Delimiter" })  -- . , ; :
  highlight(0, '@punctuation.special.jsonnet', { fg = colors.pink,       bg = 'NONE' })  -- :: ::: +: +::: field +


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.jsonnet)

  highlight(0, '@lsp.type.property.jsonnet',   { fg = colors.blue,       bg = 'NONE' })  -- Fields
  highlight(0, '@lsp.type.variable.jsonnet',   { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.function.jsonnet',   { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.parameter.jsonnet',  { fg = colors.purple,     bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.string.jsonnet',     { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.jsonnet',     { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.jsonnet',    { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.comment.jsonnet',    { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.namespace.jsonnet',  { fg = colors.turquoise,  bg = 'NONE' })  -- Namespaces


  -----------------------------------------------------------------------------
  -- Comprehensions

  highlight(0, 'jsonnetArrayComp',         { fg = colors.white,      bg = 'NONE' })  -- Array comprehensions
  highlight(0, 'jsonnetObjectComp',        { fg = colors.white,      bg = 'NONE' })  -- Object comprehensions
  highlight(0, 'jsonnetCompFor',           { fg = colors.pink,       bg = 'NONE' })  -- for in comprehensions
  highlight(0, 'jsonnetCompIf',            { fg = colors.pink,       bg = 'NONE' })  -- if filter in comprehensions


  -----------------------------------------------------------------------------
  -- Object Inheritance

  highlight(0, 'jsonnetInheritance',       { fg = colors.pink,       bg = 'NONE' })  -- Object + Object
  highlight(0, 'jsonnetMixin',             { fg = colors.turquoise,  bg = 'NONE' })  -- Mixin patterns


  -----------------------------------------------------------------------------
  -- Tanka/Kubernetes Jsonnet (k.libsonnet patterns)

  highlight(0, 'jsonnetTankaK',            { fg = colors.purple,     bg = 'NONE' })  -- k (kubernetes lib)
  highlight(0, 'jsonnetTankaKube',         { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes objects
  highlight(0, 'jsonnetTankaDeployment',   { fg = colors.turquoise,  bg = 'NONE' })  -- deployment
  highlight(0, 'jsonnetTankaService',      { fg = colors.turquoise,  bg = 'NONE' })  -- service
  highlight(0, 'jsonnetTankaConfigMap',    { fg = colors.turquoise,  bg = 'NONE' })  -- configMap
  highlight(0, 'jsonnetTankaSecret',       { fg = colors.turquoise,  bg = 'NONE' })  -- secret
  highlight(0, 'jsonnetTankaIngress',      { fg = colors.turquoise,  bg = 'NONE' })  -- ingress
  highlight(0, 'jsonnetTankaNamespace',    { fg = colors.turquoise,  bg = 'NONE' })  -- namespace


  -----------------------------------------------------------------------------
  -- Grafonnet (Grafana Jsonnet library)

  highlight(0, 'jsonnetGrafonnet',         { fg = colors.purple,     bg = 'NONE' })  -- grafonnet-lib
  highlight(0, 'jsonnetGrafDashboard',     { fg = colors.turquoise,  bg = 'NONE' })  -- dashboard
  highlight(0, 'jsonnetGrafPanel',         { fg = colors.turquoise,  bg = 'NONE' })  -- panel types
  highlight(0, 'jsonnetGrafRow',           { fg = colors.turquoise,  bg = 'NONE' })  -- row
  highlight(0, 'jsonnetGrafTarget',        { fg = colors.blue,       bg = 'NONE' })  -- prometheus/target
  highlight(0, 'jsonnetGrafTemplate',      { fg = colors.blue,       bg = 'NONE' })  -- template variables


  -----------------------------------------------------------------------------
  -- Ksonnet Patterns

  highlight(0, 'jsonnetKsonnet',           { fg = colors.purple,     bg = 'NONE' })  -- ksonnet
  highlight(0, 'jsonnetKsonnetParams',     { fg = colors.blue,       bg = 'NONE' })  -- params
  highlight(0, 'jsonnetKsonnetComponents', { fg = colors.turquoise,  bg = 'NONE' })  -- components


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'jsonnetSyntaxError',       { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'jsonnetUndefinedVar',      { link = "Variable" })  -- Undefined variables
end

return jsonnet
