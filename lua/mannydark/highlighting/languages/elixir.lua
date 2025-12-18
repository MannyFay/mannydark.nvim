-------------------------------------------------------------------------------
-- Elixir Files
-- Highlighting for .ex, .exs, .eex, .heex, .leex files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local elixir  = {}


-------------------------------------------------------------------------------
-- Settings

elixir.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'elixirKeyword',           { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'elixirBlockDefinition',   { fg = colors.blue,       bg = 'NONE'            })  -- do, end
  highlight(0, 'elixirDefine',            { fg = colors.blue,       bg = 'NONE'            })  -- def, defp, defmacro, defmacrop
  highlight(0, 'elixirPrivateDefine',     { fg = colors.blue,       bg = 'NONE'            })  -- defp, defmacrop
  highlight(0, 'elixirModuleDefine',      { fg = colors.blue,       bg = 'NONE'            })  -- defmodule
  highlight(0, 'elixirProtocolDefine',    { fg = colors.blue,       bg = 'NONE'            })  -- defprotocol, defimpl
  highlight(0, 'elixirStructDefine',      { fg = colors.blue,       bg = 'NONE'            })  -- defstruct
  highlight(0, 'elixirExceptionDefine',   { fg = colors.blue,       bg = 'NONE'            })  -- defexception
  highlight(0, 'elixirCallbackDefine',    { fg = colors.blue,       bg = 'NONE'            })  -- defcallback
  highlight(0, 'elixirGuard',             { fg = colors.blue,       bg = 'NONE'            })  -- when (guards)
  highlight(0, 'elixirConditional',       { fg = colors.blue,       bg = 'NONE'            })  -- if, unless, cond, case
  highlight(0, 'elixirRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- for (comprehensions)
  highlight(0, 'elixirException',         { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, rescue, after, throw, raise
  highlight(0, 'elixirInclude',           { fg = colors.blue,       bg = 'NONE'            })  -- import, require, use, alias
  highlight(0, 'elixirOperator',          { fg = colors.blue,       bg = 'NONE'            })  -- and, or, not, in, when
  highlight(0, 'elixirWith',              { fg = colors.blue,       bg = 'NONE'            })  -- with
  highlight(0, 'elixirReceive',           { fg = colors.blue,       bg = 'NONE'            })  -- receive
  highlight(0, 'elixirQuote',             { fg = colors.blue,       bg = 'NONE'            })  -- quote, unquote

  -- Special Keywords
  highlight(0, 'elixirBoolean',           { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'elixirNil',               { fg = colors.blue,       bg = 'NONE'            })  -- nil
  highlight(0, 'elixirPseudoVariable',    { fg = colors.blue,       bg = 'NONE'            })  -- __MODULE__, __DIR__, __ENV__, __CALLER__
  highlight(0, 'elixirFunctionDeclaration', { fg = colors.blue,     bg = 'NONE'            })  -- fn
  highlight(0, 'elixirAnonymousFunction', { fg = colors.blue,       bg = 'NONE'            })  -- fn -> end

  -- Modules and Types
  highlight(0, 'elixirModuleDeclaration', { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'elixirAlias',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Aliases (PascalCase)
  highlight(0, 'elixirStruct',            { fg = colors.turquoise,  bg = 'NONE'            })  -- %Struct{}
  highlight(0, 'elixirProtocol',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Protocol names

  -- Functions
  highlight(0, 'elixirFunctionCall',      { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'elixirUnusedVariable',    { fg = colors.gray,       bg = 'NONE'            })  -- _unused variables

  -- Variables
  highlight(0, 'elixirVariable',          { fg = colors.purple,     bg = 'NONE'            })  -- Variables
  highlight(0, 'elixirId',                { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers

  -- Atoms
  highlight(0, 'elixirAtom',              { fg = colors.pink,       bg = 'NONE'            })  -- :atoms
  highlight(0, 'elixirQuotedAtom',        { fg = colors.pink,       bg = 'NONE'            })  -- :"quoted atoms"

  -- Module Attributes
  highlight(0, 'elixirAtomInterpolated',  { fg = colors.pink,       bg = 'NONE'            })  -- Interpolated atoms
  highlight(0, 'elixirVariable',          { fg = colors.purple,     bg = 'NONE'            })  -- Variables

  -- Module Attributes and Documentation
  highlight(0, 'elixirDocString',         { fg = colors.red,        bg = 'NONE'            })  -- @doc, @moduledoc strings
  highlight(0, 'elixirDocStringDelimiter', { fg = colors.red,       bg = 'NONE'            })  -- Doc string delimiters
  highlight(0, 'elixirDocTest',           { fg = colors.gray,       bg = 'NONE'            })  -- iex> in doctests
  highlight(0, 'elixirModuleAttribute',   { fg = colors.pink,       bg = 'NONE'            })  -- @attr
  highlight(0, 'elixirCallback',          { fg = colors.pink,       bg = 'NONE'            })  -- @callback
  highlight(0, 'elixirSpec',              { fg = colors.pink,       bg = 'NONE'            })  -- @spec
  highlight(0, 'elixirType',              { fg = colors.pink,       bg = 'NONE'            })  -- @type, @typep, @opaque
  highlight(0, 'elixirBehaviour',         { fg = colors.pink,       bg = 'NONE'            })  -- @behaviour

  -- Strings
  highlight(0, 'elixirString',            { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'elixirStringDelimiter',   { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters
  highlight(0, 'elixirInterpolation',     { fg = colors.pink,       bg = 'NONE'            })  -- #{interpolation}
  highlight(0, 'elixirInterpolationDelimiter', { fg = colors.pink,  bg = 'NONE'            })  -- #{ and }
  highlight(0, 'elixirStringEscape',      { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'elixirCharList',          { fg = colors.redLight,   bg = 'NONE'            })  -- 'charlists'
  highlight(0, 'elixirHeredoc',           { fg = colors.redLight,   bg = 'NONE'            })  -- """heredocs"""
  highlight(0, 'elixirHeredocDelimiter',  { fg = colors.redLight,   bg = 'NONE'            })  -- Heredoc delimiters

  -- Sigils
  highlight(0, 'elixirSigil',             { fg = colors.redLight,   bg = 'NONE'            })  -- ~r, ~s, ~w, ~c, etc.
  highlight(0, 'elixirSigilDelimiter',    { fg = colors.redLight,   bg = 'NONE'            })  -- Sigil delimiters
  highlight(0, 'elixirRegex',             { fg = colors.redLight,   bg = 'NONE'            })  -- ~r/regex/
  highlight(0, 'elixirRegexEscape',       { fg = colors.pink,       bg = 'NONE'            })  -- Regex escapes
  highlight(0, 'elixirRegexCharClass',    { fg = colors.pink,       bg = 'NONE'            })  -- \d, \w, \s
  highlight(0, 'elixirRegexQuantifier',   { fg = colors.pink,       bg = 'NONE'            })  -- *, +, ?, {n,m}

  -- Numbers
  highlight(0, 'elixirNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'elixirInteger',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'elixirFloat',             { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'elixirBinary',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0b binary
  highlight(0, 'elixirOctal',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0o octal
  highlight(0, 'elixirHex',               { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex

  -- Operators
  highlight(0, 'elixirOperator',          { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'elixirPipe',              { fg = colors.white,      bg = 'NONE'            })  -- |> pipe operator
  highlight(0, 'elixirArrow',             { fg = colors.white,      bg = 'NONE'            })  -- -> arrow
  highlight(0, 'elixirStab',              { fg = colors.white,      bg = 'NONE'            })  -- -> in fn
  highlight(0, 'elixirCapture',           { fg = colors.white,      bg = 'NONE'            })  -- & capture
  highlight(0, 'elixirMatch',             { fg = colors.white,      bg = 'NONE'            })  -- = match operator
  highlight(0, 'elixirConcatOperator',    { fg = colors.white,      bg = 'NONE'            })  -- <> concat
  highlight(0, 'elixirListOperator',      { fg = colors.white,      bg = 'NONE'            })  -- ++ and --
  highlight(0, 'elixirRangeOperator',     { fg = colors.white,      bg = 'NONE'            })  -- .. range

  -- Comments
  highlight(0, 'elixirComment',           { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'elixirTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Special
  highlight(0, 'elixirTuple',             { fg = colors.white,      bg = 'NONE'            })  -- {} tuples
  highlight(0, 'elixirMap',               { fg = colors.white,      bg = 'NONE'            })  -- %{} maps
  highlight(0, 'elixirList',              { fg = colors.white,      bg = 'NONE'            })  -- [] lists
  highlight(0, 'elixirBitString',         { fg = colors.white,      bg = 'NONE'            })  -- <<>> bitstrings


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.elixir)

  -- Variables
  highlight(0, '@variable.elixir',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.elixir',      { fg = colors.blue,      bg = 'NONE' })  -- __MODULE__, __DIR__, etc.
  highlight(0, '@variable.parameter.elixir',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.elixir',       { fg = colors.purple,    bg = 'NONE' })  -- Struct fields

  -- Constants
  highlight(0, '@constant.elixir',              { fg = colors.turquoise, bg = 'NONE' })  -- Module names (constants)
  highlight(0, '@constant.builtin.elixir',      { fg = colors.blue,      bg = 'NONE' })  -- nil, true, false

  -- Functions
  highlight(0, '@function.elixir',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.elixir',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.macro.elixir',        { fg = colors.orange,    bg = 'NONE' })  -- Macro calls
  highlight(0, '@function.builtin.elixir',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@constructor.elixir',           { fg = colors.turquoise, bg = 'NONE' })  -- Struct construction

  -- Types (Modules)
  highlight(0, '@type.elixir',                  { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@type.builtin.elixir',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.elixir',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions

  -- Module Attributes
  highlight(0, '@attribute.elixir',             { fg = colors.pink,      bg = 'NONE' })  -- @attributes

  -- Keywords
  highlight(0, '@keyword.elixir',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.elixir',      { fg = colors.blue,      bg = 'NONE' })  -- def, defp, fn
  highlight(0, '@keyword.type.elixir',          { fg = colors.blue,      bg = 'NONE' })  -- defmodule, defstruct
  highlight(0, '@keyword.modifier.elixir',      { fg = colors.blue,      bg = 'NONE' })  -- public/private modifiers
  highlight(0, '@keyword.return.elixir',        { fg = colors.blue,      bg = 'NONE' })  -- return (implicit)
  highlight(0, '@keyword.import.elixir',        { fg = colors.blue,      bg = 'NONE' })  -- import, require, use, alias
  highlight(0, '@keyword.repeat.elixir',        { fg = colors.blue,      bg = 'NONE' })  -- for
  highlight(0, '@keyword.conditional.elixir',   { fg = colors.blue,      bg = 'NONE' })  -- if, unless, cond, case, with
  highlight(0, '@keyword.exception.elixir',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, rescue, raise
  highlight(0, '@keyword.operator.elixir',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not, in, when

  -- Strings
  highlight(0, '@string.elixir',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.documentation.elixir',  { fg = colors.red,       bg = 'NONE' })  -- @doc strings
  highlight(0, '@string.escape.elixir',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.regex.elixir',          { fg = colors.redLight,  bg = 'NONE' })  -- ~r/regex/
  highlight(0, '@string.special.elixir',        { fg = colors.pink,      bg = 'NONE' })  -- Interpolation
  highlight(0, '@string.special.symbol.elixir', { fg = colors.pink,      bg = 'NONE' })  -- :atoms
  highlight(0, '@character.elixir',             { fg = colors.redLight,  bg = 'NONE' })  -- ?c character

  -- Numbers
  highlight(0, '@number.elixir',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.elixir',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.elixir',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.elixir',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.elixir', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Modules
  highlight(0, '@module.elixir',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@label.elixir',                 { fg = colors.pink,      bg = 'NONE' })  -- Keyword list keys
  highlight(0, '@property.elixir',              { fg = colors.purple,    bg = 'NONE' })  -- Map/struct keys

  -- Sigils
  highlight(0, '@string.special.path.elixir',   { fg = colors.redLight,  bg = 'NONE' })  -- ~p sigil
  highlight(0, '@function.macro.elixir',        { fg = colors.orange,    bg = 'NONE' })  -- Sigil calls

  -- Operators and Punctuation
  highlight(0, '@operator.elixir',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.elixir',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <<>>
  highlight(0, '@punctuation.delimiter.elixir', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.elixir',   { fg = colors.pink,      bg = 'NONE' })  -- #{} interpolation


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.elixir)

  highlight(0, '@lsp.type.variable.elixir',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.elixir',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.elixir',      { fg = colors.purple,    bg = 'NONE' })  -- Struct fields
  highlight(0, '@lsp.type.function.elixir',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.macro.elixir',         { fg = colors.orange,    bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.module.elixir',        { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.type.elixir',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.namespace.elixir',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.atom.elixir',          { fg = colors.pink,      bg = 'NONE' })  -- Atoms
  highlight(0, '@lsp.type.attribute.elixir',     { fg = colors.pink,      bg = 'NONE' })  -- @attributes
  highlight(0, '@lsp.type.keyword.elixir',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.elixir',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.elixir',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.elixir',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.elixir',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.sigil.elixir',         { fg = colors.redLight,  bg = 'NONE' })  -- Sigils

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.unused.elixir',    { fg = colors.gray,      bg = 'NONE' })  -- Unused variables
  highlight(0, '@lsp.typemod.function.declaration.elixir', { fg = colors.orange,  bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.private.elixir',   { fg = colors.orange,    bg = 'NONE' })  -- Private functions
  highlight(0, '@lsp.typemod.macro.declaration.elixir',  { fg = colors.orange,    bg = 'NONE' })  -- Macro declarations
  highlight(0, '@lsp.typemod.module.declaration.elixir', { fg = colors.turquoise, bg = 'NONE' })  -- Module declarations


  -----------------------------------------------------------------------------
  -- HEEx/EEx Template Highlighting

  highlight(0, 'eelixirDelimiter',        { fg = colors.blue,       bg = 'NONE'            })  -- <% %>
  highlight(0, 'elixirExpressionDelimiter', { fg = colors.blue,     bg = 'NONE'            })  -- <%= %>
  highlight(0, 'eelixirExpression',       { fg = colors.purple,     bg = 'NONE'            })  -- Expression content

  -- HEEx specific
  highlight(0, 'heexTag',                 { fg = colors.blue,       bg = 'NONE'            })  -- HTML tags
  highlight(0, 'heexComponent',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Phoenix components
  highlight(0, 'heexSlot',                { fg = colors.pink,       bg = 'NONE'            })  -- Slots
  highlight(0, 'heexAttr',                { fg = colors.orange,     bg = 'NONE'            })  -- Attributes
end

return elixir
