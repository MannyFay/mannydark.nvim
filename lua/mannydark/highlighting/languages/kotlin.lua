-------------------------------------------------------------------------------
-- Kotlin Files
-- Highlighting for .kt and .kts files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local kotlin  = {}


-------------------------------------------------------------------------------
-- Settings

kotlin.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'ktKeyword',          { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'ktStatement',        { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue
  highlight(0, 'ktConditional',      { fg = colors.blue,       bg = 'NONE'            })  -- if, else, when
  highlight(0, 'ktRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'ktException',        { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw
  highlight(0, 'ktOperator',         { fg = colors.blue,       bg = 'NONE'            })  -- is, as, in, !is, !in
  highlight(0, 'ktStorageClass',     { fg = colors.blue,       bg = 'NONE'            })  -- val, var, const
  highlight(0, 'ktModifier',         { fg = colors.blue,       bg = 'NONE'            })  -- public, private, internal, protected, open, final
  highlight(0, 'ktStructure',        { fg = colors.blue,       bg = 'NONE'            })  -- class, interface, object, enum, data, sealed
  highlight(0, 'ktInclude',          { fg = colors.blue,       bg = 'NONE'            })  -- import, package
  highlight(0, 'ktLabel',            { fg = colors.blue,       bg = 'NONE'            })  -- Labels for break/continue
  highlight(0, 'ktThis',             { fg = colors.blue,       bg = 'NONE'            })  -- this
  highlight(0, 'ktSuper',            { fg = colors.blue,       bg = 'NONE'            })  -- super
  highlight(0, 'ktNull',             { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'ktBoolean',          { fg = colors.blue,       bg = 'NONE'            })  -- true, false

  -- Special Keywords
  highlight(0, 'ktCoroutine',        { fg = colors.blue,       bg = 'NONE'            })  -- suspend
  highlight(0, 'ktInline',           { fg = colors.blue,       bg = 'NONE'            })  -- inline, noinline, crossinline, reified
  highlight(0, 'ktDelegate',         { fg = colors.blue,       bg = 'NONE'            })  -- by
  highlight(0, 'ktCompanion',        { fg = colors.blue,       bg = 'NONE'            })  -- companion
  highlight(0, 'ktObject',           { fg = colors.blue,       bg = 'NONE'            })  -- object keyword
  highlight(0, 'ktData',             { fg = colors.blue,       bg = 'NONE'            })  -- data keyword
  highlight(0, 'ktSealed',           { fg = colors.blue,       bg = 'NONE'            })  -- sealed keyword
  highlight(0, 'ktInner',            { fg = colors.blue,       bg = 'NONE'            })  -- inner keyword
  highlight(0, 'ktLateinit',         { fg = colors.blue,       bg = 'NONE'            })  -- lateinit
  highlight(0, 'ktTypealias',        { fg = colors.blue,       bg = 'NONE'            })  -- typealias

  -- Types
  highlight(0, 'ktType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'ktBasicType',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Int, Long, String, Boolean, etc.
  highlight(0, 'ktUserType',         { fg = colors.turquoise,  bg = 'NONE'            })  -- User-defined types
  highlight(0, 'ktTypeParameter',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic type parameters <T>
  highlight(0, 'ktNullable',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Nullable types (Type?)

  -- Functions
  highlight(0, 'ktFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'ktFunctionCall',     { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'ktFun',              { fg = colors.blue,       bg = 'NONE'            })  -- fun keyword

  -- Variables
  highlight(0, 'ktIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'ktIt',               { fg = colors.purple,     bg = 'NONE'            })  -- implicit lambda parameter 'it'

  -- Annotations
  highlight(0, 'ktAnnotation',       { fg = colors.pink,       bg = 'NONE'            })  -- @Annotation
  highlight(0, 'ktAnnotationEntry',  { fg = colors.pink,       bg = 'NONE'            })  -- Annotation entries
  highlight(0, 'ktAnnotationTarget', { fg = colors.pink,       bg = 'NONE'            })  -- @file:, @field:, etc.

  -- Strings
  highlight(0, 'ktString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'ktRawString',        { fg = colors.redLight,   bg = 'NONE'            })  -- """raw strings"""
  highlight(0, 'ktCharacter',        { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'ktStringEscape',     { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'ktStringTemplateEntry', { fg = colors.pink,    bg = 'NONE'            })  -- ${expression} and $variable
  highlight(0, 'ktUnicodeEscape',    { fg = colors.pink,       bg = 'NONE'            })  -- \uXXXX

  -- Numbers
  highlight(0, 'ktNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'ktFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats

  -- Comments
  highlight(0, 'ktComment',          { fg = colors.red,        bg = 'NONE'            })  -- // and /* */ comments
  highlight(0, 'ktLineComment',      { fg = colors.red,        bg = 'NONE'            })  -- // comments
  highlight(0, 'ktShebang',          { fg = colors.red,        bg = 'NONE'            })  -- Shebang in .kts files
  highlight(0, 'ktTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- KDoc
  highlight(0, 'ktDocComment',       { fg = colors.red,        bg = 'NONE'            })  -- /** */ doc comments
  highlight(0, 'ktDocTag',           { fg = colors.green,      bg = 'NONE'            })  -- @param, @return, @throws, etc.
  highlight(0, 'ktDocTagParam',      { fg = colors.purple,     bg = 'NONE'            })  -- Parameter names in docs

  -- Operators
  highlight(0, 'ktArrow',            { fg = colors.white,      bg = 'NONE'            })  -- -> arrow
  highlight(0, 'ktDoubleColon',      { fg = colors.white,      bg = 'NONE'            })  -- :: method reference
  highlight(0, 'ktElvis',            { fg = colors.white,      bg = 'NONE'            })  -- ?: elvis operator
  highlight(0, 'ktSafeCall',         { fg = colors.white,      bg = 'NONE'            })  -- ?. safe call
  highlight(0, 'ktNotNull',          { fg = colors.white,      bg = 'NONE'            })  -- !! not-null assertion


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.kotlin)

  -- Variables
  highlight(0, '@variable.kotlin',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.kotlin',      { fg = colors.blue,      bg = 'NONE' })  -- this, super, it
  highlight(0, '@variable.parameter.kotlin',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.kotlin',       { fg = colors.purple,    bg = 'NONE' })  -- Properties/Fields

  -- Constants
  highlight(0, '@constant.kotlin',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.kotlin',      { fg = colors.blue,      bg = 'NONE' })  -- null, true, false

  -- Functions
  highlight(0, '@function.kotlin',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.kotlin',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.kotlin',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.kotlin',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.kotlin',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions (println, etc.)
  highlight(0, '@constructor.kotlin',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructor calls

  -- Types
  highlight(0, '@type.kotlin',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.kotlin',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types (Int, String, etc.)
  highlight(0, '@type.definition.kotlin',       { fg = colors.turquoise, bg = 'NONE' })  -- Type alias definitions
  highlight(0, '@type.qualifier.kotlin',        { fg = colors.blue,      bg = 'NONE' })  -- Type qualifiers

  -- Annotations
  highlight(0, '@attribute.kotlin',             { fg = colors.pink,      bg = 'NONE' })  -- Annotations

  -- Keywords
  highlight(0, '@keyword.kotlin',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.kotlin',      { fg = colors.blue,      bg = 'NONE' })  -- fun
  highlight(0, '@keyword.type.kotlin',          { fg = colors.blue,      bg = 'NONE' })  -- class, interface, object, enum
  highlight(0, '@keyword.modifier.kotlin',      { fg = colors.blue,      bg = 'NONE' })  -- public, private, open, final, data, sealed
  highlight(0, '@keyword.return.kotlin',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.import.kotlin',        { fg = colors.blue,      bg = 'NONE' })  -- import, package
  highlight(0, '@keyword.repeat.kotlin',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, do
  highlight(0, '@keyword.conditional.kotlin',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, when
  highlight(0, '@keyword.exception.kotlin',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally, throw
  highlight(0, '@keyword.operator.kotlin',      { fg = colors.blue,      bg = 'NONE' })  -- is, as, in, !is, !in
  highlight(0, '@keyword.coroutine.kotlin',     { fg = colors.blue,      bg = 'NONE' })  -- suspend

  -- Strings
  highlight(0, '@string.kotlin',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.kotlin',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.kotlin',        { fg = colors.pink,      bg = 'NONE' })  -- String templates ${...}
  highlight(0, '@character.kotlin',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.kotlin',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.kotlin',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.kotlin',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.kotlin',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.kotlin', { fg = colors.red,       bg = 'NONE' })  -- KDoc comments

  -- Modules
  highlight(0, '@module.kotlin',                { fg = colors.turquoise, bg = 'NONE' })  -- Package names
  highlight(0, '@label.kotlin',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels
  highlight(0, '@property.kotlin',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.kotlin',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.kotlin',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.kotlin', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.kotlin',   { fg = colors.white,     bg = 'NONE' })  -- :: -> ?.


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.kotlin)

  highlight(0, '@lsp.type.variable.kotlin',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.kotlin',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.kotlin',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.kotlin',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.kotlin',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.kotlin',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.interface.kotlin',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.enum.kotlin',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.kotlin',    { fg = colors.purple,    bg = 'NONE' })  -- Enum entries
  highlight(0, '@lsp.type.type.kotlin',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.kotlin', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type params <T>
  highlight(0, '@lsp.type.namespace.kotlin',     { fg = colors.turquoise, bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.annotation.kotlin',    { fg = colors.pink,      bg = 'NONE' })  -- Annotations
  highlight(0, '@lsp.type.keyword.kotlin',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.kotlin',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.kotlin',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.kotlin',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.kotlin',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.kotlin',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.kotlin',    { fg = colors.purple,    bg = 'NONE' })  -- val variables
  highlight(0, '@lsp.typemod.property.readonly.kotlin',    { fg = colors.purple,    bg = 'NONE' })  -- val properties
  highlight(0, '@lsp.typemod.function.suspend.kotlin',     { fg = colors.orange,    bg = 'NONE' })  -- suspend functions
  highlight(0, '@lsp.typemod.method.declaration.kotlin',   { fg = colors.orange,    bg = 'NONE' })  -- method declarations
  highlight(0, '@lsp.typemod.class.declaration.kotlin',    { fg = colors.turquoise, bg = 'NONE' })  -- class declarations
  highlight(0, '@lsp.typemod.class.data.kotlin',           { fg = colors.turquoise, bg = 'NONE' })  -- data classes
  highlight(0, '@lsp.typemod.class.sealed.kotlin',         { fg = colors.turquoise, bg = 'NONE' })  -- sealed classes
  highlight(0, '@lsp.typemod.type.defaultLibrary.kotlin',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return kotlin
