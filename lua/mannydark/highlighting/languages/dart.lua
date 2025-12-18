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
  highlight(0, 'dartKeyword',           { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'dartStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue
  highlight(0, 'dartConditional',       { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch, case, default
  highlight(0, 'dartRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'dartException',         { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw, rethrow, on
  highlight(0, 'dartLabel',             { fg = colors.blue,       bg = 'NONE'            })  -- Labels
  highlight(0, 'dartImport',            { fg = colors.blue,       bg = 'NONE'            })  -- import, export, library, part, part of
  highlight(0, 'dartShow',              { fg = colors.blue,       bg = 'NONE'            })  -- show, hide
  highlight(0, 'dartAs',                { fg = colors.blue,       bg = 'NONE'            })  -- as
  highlight(0, 'dartDeferred',          { fg = colors.blue,       bg = 'NONE'            })  -- deferred
  highlight(0, 'dartTypeDefinition',    { fg = colors.blue,       bg = 'NONE'            })  -- class, enum, extension, mixin, typedef
  highlight(0, 'dartStorageClass',      { fg = colors.blue,       bg = 'NONE'            })  -- static, final, const, late
  highlight(0, 'dartModifier',          { fg = colors.blue,       bg = 'NONE'            })  -- abstract, covariant, external
  highlight(0, 'dartOperatorKeyword',   { fg = colors.blue,       bg = 'NONE'            })  -- is, is!, as
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
  highlight(0, 'dartBoolean',           { fg = colors.blue,       bg = 'NONE'            })  -- true, false

  -- Types
  highlight(0, 'dartType',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'dartTypeName',          { fg = colors.turquoise,  bg = 'NONE'            })  -- User-defined types
  highlight(0, 'dartBuiltinType',       { fg = colors.turquoise,  bg = 'NONE'            })  -- int, double, String, bool, List, Map, Set, Object, dynamic, void, var
  highlight(0, 'dartVoid',              { fg = colors.turquoise,  bg = 'NONE'            })  -- void
  highlight(0, 'dartDynamic',           { fg = colors.turquoise,  bg = 'NONE'            })  -- dynamic
  highlight(0, 'dartGenericType',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic type parameters <T>
  highlight(0, 'dartFuture',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Future, Stream
  highlight(0, 'dartNullable',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Type? nullable types

  -- Functions
  highlight(0, 'dartFunction',          { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'dartFunctionCall',      { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'dartMethod',            { fg = colors.orange,     bg = 'NONE'            })  -- Method names
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
  highlight(0, 'dartString',            { fg = colors.redLight,   bg = 'NONE'            })  -- "strings" and 'strings'
  highlight(0, 'dartRawString',         { fg = colors.redLight,   bg = 'NONE'            })  -- r"raw strings"
  highlight(0, 'dartMultilineString',   { fg = colors.redLight,   bg = 'NONE'            })  -- '''multiline''' and """multiline"""
  highlight(0, 'dartStringDelimiter',   { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters
  highlight(0, 'dartInterpolation',     { fg = colors.pink,       bg = 'NONE'            })  -- $variable and ${expression}
  highlight(0, 'dartInterpolationDelimiter', { fg = colors.pink,  bg = 'NONE'            })  -- $ and ${ }
  highlight(0, 'dartStringEscape',      { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'dartUnicodeEscape',     { fg = colors.pink,       bg = 'NONE'            })  -- \uXXXX

  -- Numbers
  highlight(0, 'dartNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'dartInteger',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'dartDouble',            { fg = colors.greenLight, bg = 'NONE'            })  -- Doubles
  highlight(0, 'dartHex',               { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex

  -- Operators
  highlight(0, 'dartOperator',          { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'dartCascade',           { fg = colors.white,      bg = 'NONE'            })  -- .. cascade
  highlight(0, 'dartNullAware',         { fg = colors.white,      bg = 'NONE'            })  -- ?., ??, ??=, !
  highlight(0, 'dartSpread',            { fg = colors.white,      bg = 'NONE'            })  -- ... spread
  highlight(0, 'dartArrow',             { fg = colors.white,      bg = 'NONE'            })  -- => arrow
  highlight(0, 'dartTernary',           { fg = colors.white,      bg = 'NONE'            })  -- ? : ternary

  -- Comments
  highlight(0, 'dartComment',           { fg = colors.red,        bg = 'NONE'            })  -- // and /* */ comments
  highlight(0, 'dartLineComment',       { fg = colors.red,        bg = 'NONE'            })  -- // comments
  highlight(0, 'dartBlockComment',      { fg = colors.red,        bg = 'NONE'            })  -- /* */ comments
  highlight(0, 'dartTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Documentation
  highlight(0, 'dartDocComment',        { fg = colors.red,        bg = 'NONE'            })  -- /// doc comments
  highlight(0, 'dartDocTag',            { fg = colors.green,      bg = 'NONE'            })  -- [parameter], {@macro}


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.dart)

  -- Variables
  highlight(0, '@variable.dart',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.dart',      { fg = colors.blue,      bg = 'NONE' })  -- this, super
  highlight(0, '@variable.parameter.dart',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.dart',       { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Constants
  highlight(0, '@constant.dart',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.dart',      { fg = colors.blue,      bg = 'NONE' })  -- null, true, false

  -- Functions
  highlight(0, '@function.dart',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.dart',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.dart',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.dart',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.dart',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@constructor.dart',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructor calls

  -- Types
  highlight(0, '@type.dart',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.dart',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.dart',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.dart',        { fg = colors.blue,      bg = 'NONE' })  -- Type qualifiers

  -- Annotations
  highlight(0, '@attribute.dart',             { fg = colors.pink,      bg = 'NONE' })  -- @annotations

  -- Keywords
  highlight(0, '@keyword.dart',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.dart',      { fg = colors.blue,      bg = 'NONE' })  -- Function related
  highlight(0, '@keyword.type.dart',          { fg = colors.blue,      bg = 'NONE' })  -- class, enum, extension, mixin
  highlight(0, '@keyword.modifier.dart',      { fg = colors.blue,      bg = 'NONE' })  -- static, final, const, late, abstract
  highlight(0, '@keyword.return.dart',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.import.dart',        { fg = colors.blue,      bg = 'NONE' })  -- import, export, library
  highlight(0, '@keyword.repeat.dart',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, do
  highlight(0, '@keyword.conditional.dart',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch, case
  highlight(0, '@keyword.exception.dart',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally, throw
  highlight(0, '@keyword.operator.dart',      { fg = colors.blue,      bg = 'NONE' })  -- is, as
  highlight(0, '@keyword.coroutine.dart',     { fg = colors.blue,      bg = 'NONE' })  -- async, await, yield

  -- Strings
  highlight(0, '@string.dart',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.dart',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.dart',        { fg = colors.pink,      bg = 'NONE' })  -- $ interpolation
  highlight(0, '@string.regexp.dart',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex

  -- Numbers
  highlight(0, '@number.dart',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.dart',          { fg = colors.greenLight, bg = 'NONE' })  -- Doubles

  -- Booleans
  highlight(0, '@boolean.dart',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.dart',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.dart', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Modules
  highlight(0, '@module.dart',                { fg = colors.turquoise, bg = 'NONE' })  -- Library/package names
  highlight(0, '@label.dart',                 { fg = colors.purple,    bg = 'NONE' })  -- Labels
  highlight(0, '@property.dart',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.dart',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.dart',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.dart', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.dart',   { fg = colors.pink,      bg = 'NONE' })  -- $ and ${} interpolation


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.dart)

  highlight(0, '@lsp.type.variable.dart',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
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
  highlight(0, '@lsp.type.keyword.dart',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.dart',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.dart',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.dart',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.dart',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.dart',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.static.dart',       { fg = colors.purple,    bg = 'NONE' })  -- Static fields
  highlight(0, '@lsp.typemod.variable.final.dart',        { fg = colors.purple,    bg = 'NONE' })  -- Final variables
  highlight(0, '@lsp.typemod.function.declaration.dart',  { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.method.static.dart',         { fg = colors.orange,    bg = 'NONE' })  -- Static methods
  highlight(0, '@lsp.typemod.class.declaration.dart',     { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.class.abstract.dart',        { fg = colors.turquoise, bg = 'NONE' })  -- Abstract classes
  highlight(0, '@lsp.typemod.type.defaultLibrary.dart',   { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types


  -----------------------------------------------------------------------------
  -- Flutter-specific (common patterns)

  highlight(0, 'dartFlutterWidget',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Widget classes
  highlight(0, 'dartFlutterBuildMethod', { fg = colors.orange,    bg = 'NONE'            })  -- build method
end

return dart
