-------------------------------------------------------------------------------
-- Scala Files
-- Highlighting for .scala, .sc, .sbt files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local scala   = {}


-------------------------------------------------------------------------------
-- Settings

scala.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'scalaKeyword',          { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'scalaKeywordModifier',  { fg = colors.blue,       bg = 'NONE'            })  -- abstract, final, sealed, implicit, lazy, override
  highlight(0, 'scalaSpecial',          { fg = colors.blue,       bg = 'NONE'            })  -- return, throw
  highlight(0, 'scalaConditional',      { fg = colors.blue,       bg = 'NONE'            })  -- if, else, match
  highlight(0, 'scalaRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do, yield
  highlight(0, 'scalaException',        { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw
  highlight(0, 'scalaInclude',          { fg = colors.blue,       bg = 'NONE'            })  -- import, package
  highlight(0, 'scalaExtends',          { fg = colors.blue,       bg = 'NONE'            })  -- extends, with
  highlight(0, 'scalaNew',              { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'scalaInstanceOf',       { fg = colors.blue,       bg = 'NONE'            })  -- isInstanceOf, asInstanceOf

  -- Definitions
  highlight(0, 'scalaDef',              { fg = colors.blue,       bg = 'NONE'            })  -- def
  highlight(0, 'scalaVal',              { fg = colors.blue,       bg = 'NONE'            })  -- val
  highlight(0, 'scalaVar',              { fg = colors.blue,       bg = 'NONE'            })  -- var
  highlight(0, 'scalaLazy',             { fg = colors.blue,       bg = 'NONE'            })  -- lazy
  highlight(0, 'scalaType',             { fg = colors.blue,       bg = 'NONE'            })  -- type keyword

  -- Class/Object Definitions
  highlight(0, 'scalaClass',            { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'scalaObject',           { fg = colors.blue,       bg = 'NONE'            })  -- object
  highlight(0, 'scalaTrait',            { fg = colors.blue,       bg = 'NONE'            })  -- trait
  highlight(0, 'scalaCaseClass',        { fg = colors.blue,       bg = 'NONE'            })  -- case class
  highlight(0, 'scalaCaseObject',       { fg = colors.blue,       bg = 'NONE'            })  -- case object
  highlight(0, 'scalaSealed',           { fg = colors.blue,       bg = 'NONE'            })  -- sealed
  highlight(0, 'scalaAbstract',         { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'scalaFinal',            { fg = colors.blue,       bg = 'NONE'            })  -- final
  highlight(0, 'scalaPrivate',          { fg = colors.blue,       bg = 'NONE'            })  -- private
  highlight(0, 'scalaProtected',        { fg = colors.blue,       bg = 'NONE'            })  -- protected
  highlight(0, 'scalaOverride',         { fg = colors.blue,       bg = 'NONE'            })  -- override

  -- Scala 3 Keywords
  highlight(0, 'scalaEnum',             { fg = colors.blue,       bg = 'NONE'            })  -- enum
  highlight(0, 'scalaGiven',            { fg = colors.blue,       bg = 'NONE'            })  -- given
  highlight(0, 'scalaUsing',            { fg = colors.blue,       bg = 'NONE'            })  -- using
  highlight(0, 'scalaExtension',        { fg = colors.blue,       bg = 'NONE'            })  -- extension
  highlight(0, 'scalaInline',           { fg = colors.blue,       bg = 'NONE'            })  -- inline
  highlight(0, 'scalaOpaque',           { fg = colors.blue,       bg = 'NONE'            })  -- opaque
  highlight(0, 'scalaTransparent',      { fg = colors.blue,       bg = 'NONE'            })  -- transparent
  highlight(0, 'scalaExport',           { fg = colors.blue,       bg = 'NONE'            })  -- export
  highlight(0, 'scalaThen',             { fg = colors.blue,       bg = 'NONE'            })  -- then (Scala 3)
  highlight(0, 'scalaEnd',              { fg = colors.blue,       bg = 'NONE'            })  -- end (Scala 3)

  -- Implicits
  highlight(0, 'scalaImplicit',         { fg = colors.blue,       bg = 'NONE'            })  -- implicit

  -- Special Keywords
  highlight(0, 'scalaThis',             { fg = colors.blue,       bg = 'NONE'            })  -- this
  highlight(0, 'scalaSuper',            { fg = colors.blue,       bg = 'NONE'            })  -- super
  highlight(0, 'scalaNull',             { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'scalaBoolean',          { fg = colors.blue,       bg = 'NONE'            })  -- true, false

  -- Types
  highlight(0, 'scalaTypeName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'scalaTypeSpecializer',  { fg = colors.turquoise,  bg = 'NONE'            })  -- Type parameters
  highlight(0, 'scalaBuiltinType',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Int, String, Boolean, Double, Float, Long, Short, Byte, Char, Unit, Any, AnyVal, AnyRef, Nothing, Null
  highlight(0, 'scalaUpperBound',       { fg = colors.turquoise,  bg = 'NONE'            })  -- <: upper bound
  highlight(0, 'scalaLowerBound',       { fg = colors.turquoise,  bg = 'NONE'            })  -- >: lower bound
  highlight(0, 'scalaViewBound',        { fg = colors.turquoise,  bg = 'NONE'            })  -- <% view bound (deprecated)
  highlight(0, 'scalaContextBound',     { fg = colors.turquoise,  bg = 'NONE'            })  -- : context bound
  highlight(0, 'scalaVariance',         { fg = colors.turquoise,  bg = 'NONE'            })  -- + - variance annotations

  -- Constructors
  highlight(0, 'scalaConstructor',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Some, None, Left, Right, etc.
  highlight(0, 'scalaCaseFollowing',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Pattern in case

  -- Functions
  highlight(0, 'scalaFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'scalaFunctionCall',     { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'scalaMethodCall',       { fg = colors.orange,     bg = 'NONE'            })  -- Method calls

  -- Variables and Parameters
  highlight(0, 'scalaIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'scalaParameter',        { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'scalaNamedArgument',    { fg = colors.purple,     bg = 'NONE'            })  -- Named arguments
  highlight(0, 'scalaPlaceholder',      { fg = colors.purple,     bg = 'NONE'            })  -- _ placeholder

  -- Annotations
  highlight(0, 'scalaAnnotation',       { fg = colors.pink,       bg = 'NONE'            })  -- @annotation
  highlight(0, 'scalaAt',               { fg = colors.pink,       bg = 'NONE'            })  -- @ symbol

  -- Strings
  highlight(0, 'scalaString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'scalaMultiLineString',  { fg = colors.redLight,   bg = 'NONE'            })  -- """multiline"""
  highlight(0, 'scalaStringEscape',     { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'scalaUnicodeEscape',    { fg = colors.pink,       bg = 'NONE'            })  -- \uXXXX
  highlight(0, 'scalaChar',             { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'scalaInterpolation',    { fg = colors.pink,       bg = 'NONE'            })  -- $var and ${expr} in interpolated strings
  highlight(0, 'scalaInterpolationDelimiter', { fg = colors.pink, bg = 'NONE'            })  -- s, f, raw prefixes
  highlight(0, 'scalaFInterpolation',   { fg = colors.pink,       bg = 'NONE'            })  -- f"..." format specifiers

  -- Symbols
  highlight(0, 'scalaSymbol',           { fg = colors.pink,       bg = 'NONE'            })  -- 'symbol (deprecated)

  -- Numbers
  highlight(0, 'scalaNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'scalaInt',              { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'scalaLong',             { fg = colors.greenLight, bg = 'NONE'            })  -- Long (1L)
  highlight(0, 'scalaFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats (1.0f)
  highlight(0, 'scalaDouble',           { fg = colors.greenLight, bg = 'NONE'            })  -- Doubles
  highlight(0, 'scalaHex',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex

  -- Operators
  highlight(0, 'scalaOperator',         { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'scalaArrow',            { fg = colors.white,      bg = 'NONE'            })  -- => -> arrows
  highlight(0, 'scalaDoubleArrow',      { fg = colors.white,      bg = 'NONE'            })  -- =>
  highlight(0, 'scalaSingleArrow',      { fg = colors.white,      bg = 'NONE'            })  -- ->
  highlight(0, 'scalaAssign',           { fg = colors.white,      bg = 'NONE'            })  -- =
  highlight(0, 'scalaPipe',             { fg = colors.white,      bg = 'NONE'            })  -- | in pattern matching
  highlight(0, 'scalaAt',               { fg = colors.white,      bg = 'NONE'            })  -- @ in patterns
  highlight(0, 'scalaWildcard',         { fg = colors.gray,       bg = 'NONE'            })  -- _ wildcard

  -- Comments
  highlight(0, 'scalaComment',          { fg = colors.red,        bg = 'NONE'            })  -- // and /* */ comments
  highlight(0, 'scalaLineComment',      { fg = colors.red,        bg = 'NONE'            })  -- // comments
  highlight(0, 'scalaBlockComment',     { fg = colors.red,        bg = 'NONE'            })  -- /* */ comments
  highlight(0, 'scalaTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Scaladoc
  highlight(0, 'scalaDocComment',       { fg = colors.red,        bg = 'NONE'            })  -- /** doc comments */
  highlight(0, 'scalaDocTag',           { fg = colors.green,      bg = 'NONE'            })  -- @param, @return, @throws, etc.
  highlight(0, 'scalaDocTagParam',      { fg = colors.purple,     bg = 'NONE'            })  -- Parameter names in docs


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.scala)

  -- Variables
  highlight(0, '@variable.scala',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.scala',      { fg = colors.blue,      bg = 'NONE' })  -- this, super
  highlight(0, '@variable.parameter.scala',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.scala',       { fg = colors.purple,    bg = 'NONE' })  -- Fields

  -- Constants
  highlight(0, '@constant.scala',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.scala',      { fg = colors.blue,      bg = 'NONE' })  -- null, true, false

  -- Functions
  highlight(0, '@function.scala',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.scala',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.scala',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.scala',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.scala',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@constructor.scala',           { fg = colors.turquoise, bg = 'NONE' })  -- Class instantiation

  -- Types
  highlight(0, '@type.scala',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.scala',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.scala',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.scala',        { fg = colors.blue,      bg = 'NONE' })  -- Type qualifiers

  -- Annotations
  highlight(0, '@attribute.scala',             { fg = colors.pink,      bg = 'NONE' })  -- @annotations

  -- Keywords
  highlight(0, '@keyword.scala',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.scala',      { fg = colors.blue,      bg = 'NONE' })  -- def
  highlight(0, '@keyword.type.scala',          { fg = colors.blue,      bg = 'NONE' })  -- class, trait, object, type
  highlight(0, '@keyword.modifier.scala',      { fg = colors.blue,      bg = 'NONE' })  -- abstract, final, sealed, implicit, lazy, override
  highlight(0, '@keyword.return.scala',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.import.scala',        { fg = colors.blue,      bg = 'NONE' })  -- import, package
  highlight(0, '@keyword.repeat.scala',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, do
  highlight(0, '@keyword.conditional.scala',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, match, case
  highlight(0, '@keyword.exception.scala',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally, throw
  highlight(0, '@keyword.operator.scala',      { fg = colors.blue,      bg = 'NONE' })  -- new

  -- Strings
  highlight(0, '@string.scala',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.scala',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.scala',        { fg = colors.pink,      bg = 'NONE' })  -- Interpolation
  highlight(0, '@character.scala',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.scala',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.scala',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.scala',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.scala',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.scala', { fg = colors.red,       bg = 'NONE' })  -- Scaladoc comments

  -- Modules
  highlight(0, '@module.scala',                { fg = colors.turquoise, bg = 'NONE' })  -- Package names
  highlight(0, '@label.scala',                 { fg = colors.purple,    bg = 'NONE' })  -- Labels
  highlight(0, '@property.scala',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.scala',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.scala',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.scala', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.scala',   { fg = colors.pink,      bg = 'NONE' })  -- $ in interpolation


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.scala)

  highlight(0, '@lsp.type.variable.scala',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.scala',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.scala',      { fg = colors.purple,    bg = 'NONE' })  -- Fields
  highlight(0, '@lsp.type.function.scala',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.scala',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.scala',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.trait.scala',         { fg = colors.turquoise, bg = 'NONE' })  -- Traits
  highlight(0, '@lsp.type.object.scala',        { fg = colors.turquoise, bg = 'NONE' })  -- Objects
  highlight(0, '@lsp.type.enum.scala',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.scala',    { fg = colors.turquoise, bg = 'NONE' })  -- Enum cases
  highlight(0, '@lsp.type.type.scala',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.scala', { fg = colors.turquoise, bg = 'NONE' })  -- Type parameters
  highlight(0, '@lsp.type.namespace.scala',     { fg = colors.turquoise, bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.keyword.scala',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.scala',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.scala',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.scala',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.scala',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.scala',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.scala',    { fg = colors.purple,    bg = 'NONE' })  -- val variables
  highlight(0, '@lsp.typemod.function.declaration.scala', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.method.implicit.scala',      { fg = colors.orange,    bg = 'NONE' })  -- Implicit methods
  highlight(0, '@lsp.typemod.class.declaration.scala',    { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.class.case.scala',           { fg = colors.turquoise, bg = 'NONE' })  -- Case classes
  highlight(0, '@lsp.typemod.type.defaultLibrary.scala',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return scala
