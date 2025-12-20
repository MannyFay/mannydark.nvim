-------------------------------------------------------------------------------
-- Dart Files
-- Highlighting for .dart files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local dart    = {}


-------------------------------------------------------------------------------
-- Settings

dart.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'dartKeyword',           { link = "Keyword" })  -- General keywords
  highlight(0, 'dartStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue
  highlight(0, 'dartConditional',       { link = "Conditional" })  -- if, else, switch, case, default
  highlight(0, 'dartRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'dartException',         { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw, rethrow, on
  highlight(0, 'dartLabel',             { fg = colors.blue,       bg = 'NONE'            })  -- Labels
  highlight(0, 'dartImport',            { fg = colors.blue,       bg = 'NONE'            })  -- import, export, library, part, part of
  highlight(0, 'dartShow',              { fg = colors.blue,       bg = 'NONE'            })  -- show, hide
  highlight(0, 'dartAs',                { fg = colors.blue,       bg = 'NONE'            })  -- as
  highlight(0, 'dartDeferred',          { fg = colors.blue,       bg = 'NONE'            })  -- deferred
  highlight(0, 'dartTypeDefinition',    { link = "Type" })  -- class, enum, extension, mixin, typedef
  highlight(0, 'dartStorageClass',      { fg = colors.blue,       bg = 'NONE'            })  -- static, final, const, late
  highlight(0, 'dartModifier',          { fg = colors.blue,       bg = 'NONE'            })  -- abstract, covariant, external
  highlight(0, 'dartOperatorKeyword',   { link = "Operator" })  -- is, is!, as
  highlight(0, 'dartWith',              { fg = colors.blue,       bg = 'NONE'            })  -- with (mixins)
  highlight(0, 'dartExtends',           { fg = colors.blue,       bg = 'NONE'            })  -- extends
  highlight(0, 'dartImplements',        { fg = colors.blue,       bg = 'NONE'            })  -- implements
  highlight(0, 'dartOn',                { fg = colors.blue,       bg = 'NONE'            })  -- on (extension, catch)
  highlight(0, 'dartNew',               { fg = colors.blue,       bg = 'NONE'            })  -- new (optional)
  highlight(0, 'dartAssert',            { fg = colors.blue,       bg = 'NONE'            })  -- assert
  highlight(0, 'dartRequired',          { fg = colors.blue,       bg = 'NONE'            })  -- required

  -- Async/Await
  highlight(0, 'dartAsync',             { fg = colors.blue,       bg = 'NONE'            })  -- async, async*, sync*
  highlight(0, 'dartAwait',             { fg = colors.blue,       bg = 'NONE'            })  -- await
  highlight(0, 'dartYield',             { fg = colors.blue,       bg = 'NONE'            })  -- yield, yield*

  -- Accessors
  highlight(0, 'dartGet',               { fg = colors.blue,       bg = 'NONE'            })  -- get
  highlight(0, 'dartSet',               { fg = colors.blue,       bg = 'NONE'            })  -- set
  highlight(0, 'dartFactory',           { fg = colors.blue,       bg = 'NONE'            })  -- factory

  -- Special Keywords
  highlight(0, 'dartThis',              { fg = colors.blue,       bg = 'NONE'            })  -- this
  highlight(0, 'dartSuper',             { fg = colors.blue,       bg = 'NONE'            })  -- super
  highlight(0, 'dartNull',              { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'dartBoolean',           { link = "Boolean" })  -- true, false

  -- Types
  highlight(0, 'dartType',              { link = "Type" })  -- Type names
  highlight(0, 'dartTypeName',          { link = "Type" })  -- User-defined types
  highlight(0, 'dartBuiltinType',       { link = "Type" })  -- int, double, String, bool, List, Map, Set, Object, dynamic, void, var
  highlight(0, 'dartVoid',              { fg = colors.turquoise,  bg = 'NONE'            })  -- void
  highlight(0, 'dartDynamic',           { fg = colors.turquoise,  bg = 'NONE'            })  -- dynamic
  highlight(0, 'dartGenericType',       { link = "Type" })  -- Generic type parameters <T>
  highlight(0, 'dartFuture',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Future, Stream
  highlight(0, 'dartNullable',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Type? nullable types

  -- Functions
  highlight(0, 'dartFunction',          { link = "Function" })  -- Function definitions
  highlight(0, 'dartFunctionCall',      { link = "Function" })  -- Function calls
  highlight(0, 'dartMethod',            { link = "Function" })  -- Method names
  highlight(0, 'dartConstructor',       { fg = colors.orange,     bg = 'NONE'            })  -- Constructor names

  -- Variables and Parameters
  highlight(0, 'dartIdentifier',        { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'dartParameter',         { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'dartProperty',          { fg = colors.purple,     bg = 'NONE'            })  -- Properties
  highlight(0, 'dartNamedParameter',    { fg = colors.purple,     bg = 'NONE'            })  -- Named parameters

  -- Annotations/Metadata
  highlight(0, 'dartMetadata',          { fg = colors.pink,       bg = 'NONE'            })  -- @override, @deprecated, @required
  highlight(0, 'dartAnnotation',        { fg = colors.pink,       bg = 'NONE'            })  -- Annotations

  -- Strings
  highlight(0, 'dartString',            { link = "String" })  -- "strings" and 'strings'
  highlight(0, 'dartRawString',         { link = "String" })  -- r"raw strings"
  highlight(0, 'dartMultilineString',   { link = "String" })  -- '''multiline''' and """multiline"""
  highlight(0, 'dartStringDelimiter',   { link = "Delimiter" })  -- String delimiters
  highlight(0, 'dartInterpolation',     { fg = colors.pink,       bg = 'NONE'            })  -- $variable and ${expression}
  highlight(0, 'dartInterpolationDelimiter', { link = "Delimiter" })  -- $ and ${ }
  highlight(0, 'dartStringEscape',      { link = "String" })  -- \n, \t, etc.
  highlight(0, 'dartUnicodeEscape',     { fg = colors.pink,       bg = 'NONE'            })  -- \uXXXX

  -- Numbers
  highlight(0, 'dartNumber',            { link = "Number" })  -- Numbers
  highlight(0, 'dartInteger',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'dartDouble',            { fg = colors.greenLight, bg = 'NONE'            })  -- Doubles
  highlight(0, 'dartHex',               { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex

  -- Operators
  highlight(0, 'dartOperator',          { link = "Operator" })  -- Operators
  highlight(0, 'dartCascade',           { fg = colors.white,      bg = 'NONE'            })  -- .. cascade
  highlight(0, 'dartNullAware',         { fg = colors.white,      bg = 'NONE'            })  -- ?., ??, ??=, !
  highlight(0, 'dartSpread',            { fg = colors.white,      bg = 'NONE'            })  -- ... spread
  highlight(0, 'dartArrow',             { fg = colors.white,      bg = 'NONE'            })  -- => arrow
  highlight(0, 'dartTernary',           { fg = colors.white,      bg = 'NONE'            })  -- ? : ternary

  -- Comments
  highlight(0, 'dartComment',           { link = "Comment" })  -- // and /* */ comments
  highlight(0, 'dartLineComment',       { link = "Comment" })  -- // comments
  highlight(0, 'dartBlockComment',      { link = "Comment" })  -- /* */ comments
  highlight(0, 'dartTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Documentation
  highlight(0, 'dartDocComment',        { link = "Comment" })  -- /// doc comments
  highlight(0, 'dartDocTag',            { fg = colors.green,      bg = 'NONE'            })  -- [parameter], {@macro}


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.dart)

  -- Variables
  highlight(0, '@variable.dart',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.dart',      { link = "Variable" })  -- this, super
  highlight(0, '@variable.parameter.dart',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.dart',       { link = "Variable" })  -- Properties

  -- Constants
  highlight(0, '@constant.dart',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.dart',      { link = "Constant" })  -- null, true, false

  -- Functions
  highlight(0, '@function.dart',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.dart',         { link = "Function" })  -- Function calls
  highlight(0, '@function.method.dart',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.dart',  { link = "Function" })  -- Method calls
  highlight(0, '@function.builtin.dart',      { link = "Function" })  -- Built-in functions
  highlight(0, '@constructor.dart',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructor calls

  -- Types
  highlight(0, '@type.dart',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.dart',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.dart',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.dart',        { link = "Type" })  -- Type qualifiers

  -- Annotations
  highlight(0, '@attribute.dart',             { fg = colors.pink,      bg = 'NONE' })  -- @annotations

  -- Keywords
  highlight(0, '@keyword.dart',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.dart',      { link = "Keyword" })  -- Function related
  highlight(0, '@keyword.type.dart',          { link = "Keyword" })  -- class, enum, extension, mixin
  highlight(0, '@keyword.modifier.dart',      { link = "Keyword" })  -- static, final, const, late, abstract
  highlight(0, '@keyword.return.dart',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.import.dart',        { link = "Keyword" })  -- import, export, library
  highlight(0, '@keyword.repeat.dart',        { link = "Keyword" })  -- for, while, do
  highlight(0, '@keyword.conditional.dart',   { link = "Conditional" })  -- if, else, switch, case
  highlight(0, '@keyword.exception.dart',     { link = "Keyword" })  -- try, catch, finally, throw
  highlight(0, '@keyword.operator.dart',      { link = "Operator" })  -- is, as
  highlight(0, '@keyword.coroutine.dart',     { link = "Keyword" })  -- async, await, yield

  -- Strings
  highlight(0, '@string.dart',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.dart',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.dart',        { link = "String" })  -- $ interpolation
  highlight(0, '@string.regexp.dart',         { link = "String" })  -- Regex

  -- Numbers
  highlight(0, '@number.dart',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.dart',          { link = "Number" })  -- Doubles

  -- Booleans
  highlight(0, '@boolean.dart',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.dart',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.dart', { link = "Comment" })  -- Doc comments

  -- Modules
  highlight(0, '@module.dart',                { fg = colors.turquoise, bg = 'NONE' })  -- Library/package names
  highlight(0, '@label.dart',                 { fg = colors.purple,    bg = 'NONE' })  -- Labels
  highlight(0, '@property.dart',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.dart',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.dart',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.dart', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.dart',   { fg = colors.pink,      bg = 'NONE' })  -- $ and ${} interpolation


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.dart)

  highlight(0, '@lsp.type.variable.dart',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.dart',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.dart',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.dart',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.dart',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.dart',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.enum.dart',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.dart',    { fg = colors.purple,    bg = 'NONE' })  -- Enum values
  highlight(0, '@lsp.type.type.dart',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.dart', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type params
  highlight(0, '@lsp.type.namespace.dart',     { fg = colors.turquoise, bg = 'NONE' })  -- Libraries
  highlight(0, '@lsp.type.annotation.dart',    { fg = colors.pink,      bg = 'NONE' })  -- Annotations
  highlight(0, '@lsp.type.keyword.dart',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.dart',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.dart',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.dart',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.dart',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.dart',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.static.dart',       { link = "Variable" })  -- Static fields
  highlight(0, '@lsp.typemod.variable.final.dart',        { link = "Variable" })  -- Final variables
  highlight(0, '@lsp.typemod.function.declaration.dart',  { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.method.static.dart',         { fg = colors.orange,    bg = 'NONE' })  -- Static methods
  highlight(0, '@lsp.typemod.class.declaration.dart',     { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.class.abstract.dart',        { fg = colors.turquoise, bg = 'NONE' })  -- Abstract classes
  highlight(0, '@lsp.typemod.type.defaultLibrary.dart',   { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types


  -----------------------------------------------------------------------------
  -- Flutter-specific (common patterns)

  highlight(0, 'dartFlutterWidget',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Widget classes
  highlight(0, 'dartFlutterBuildMethod', { link = "Function" })  -- build method
end

return dart
