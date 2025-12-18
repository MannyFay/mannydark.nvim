-------------------------------------------------------------------------------
-- OCaml Files
-- Highlighting for .ml, .mli, .mll, .mly files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local ocaml   = {}


-------------------------------------------------------------------------------
-- Settings

ocaml.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'ocamlKeyword',          { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'ocamlLet',              { fg = colors.blue,       bg = 'NONE'            })  -- let, let rec, and
  highlight(0, 'ocamlIn',               { fg = colors.blue,       bg = 'NONE'            })  -- in
  highlight(0, 'ocamlConditional',      { fg = colors.blue,       bg = 'NONE'            })  -- if, then, else
  highlight(0, 'ocamlMatch',            { fg = colors.blue,       bg = 'NONE'            })  -- match, with, function
  highlight(0, 'ocamlWhen',             { fg = colors.blue,       bg = 'NONE'            })  -- when (guards)
  highlight(0, 'ocamlLoop',             { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do, done, to, downto
  highlight(0, 'ocamlFun',              { fg = colors.blue,       bg = 'NONE'            })  -- fun, function
  highlight(0, 'ocamlAs',               { fg = colors.blue,       bg = 'NONE'            })  -- as (pattern alias)
  highlight(0, 'ocamlOf',               { fg = colors.blue,       bg = 'NONE'            })  -- of (in type definitions)
  highlight(0, 'ocamlAssert',           { fg = colors.blue,       bg = 'NONE'            })  -- assert
  highlight(0, 'ocamlLazy',             { fg = colors.blue,       bg = 'NONE'            })  -- lazy

  -- Module System
  highlight(0, 'ocamlModule',           { fg = colors.blue,       bg = 'NONE'            })  -- module
  highlight(0, 'ocamlModuleKeyword',    { fg = colors.blue,       bg = 'NONE'            })  -- struct, sig, end, functor
  highlight(0, 'ocamlInclude',          { fg = colors.blue,       bg = 'NONE'            })  -- open, include
  highlight(0, 'ocamlModPath',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Module paths

  -- Type Definitions
  highlight(0, 'ocamlType',             { fg = colors.blue,       bg = 'NONE'            })  -- type keyword
  highlight(0, 'ocamlMutable',          { fg = colors.blue,       bg = 'NONE'            })  -- mutable
  highlight(0, 'ocamlPrivate',          { fg = colors.blue,       bg = 'NONE'            })  -- private
  highlight(0, 'ocamlConstraint',       { fg = colors.blue,       bg = 'NONE'            })  -- constraint

  -- Exception Handling
  highlight(0, 'ocamlException',        { fg = colors.blue,       bg = 'NONE'            })  -- exception, raise, try, with
  highlight(0, 'ocamlTry',              { fg = colors.blue,       bg = 'NONE'            })  -- try

  -- Object-Oriented
  highlight(0, 'ocamlObject',           { fg = colors.blue,       bg = 'NONE'            })  -- object, end
  highlight(0, 'ocamlClass',            { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'ocamlMethod',           { fg = colors.blue,       bg = 'NONE'            })  -- method, val
  highlight(0, 'ocamlInherit',          { fg = colors.blue,       bg = 'NONE'            })  -- inherit
  highlight(0, 'ocamlNew',              { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'ocamlInitializer',      { fg = colors.blue,       bg = 'NONE'            })  -- initializer
  highlight(0, 'ocamlVirtual',          { fg = colors.blue,       bg = 'NONE'            })  -- virtual

  -- References
  highlight(0, 'ocamlRef',              { fg = colors.blue,       bg = 'NONE'            })  -- ref
  highlight(0, 'ocamlDeref',            { fg = colors.white,      bg = 'NONE'            })  -- !
  highlight(0, 'ocamlAssign',           { fg = colors.white,      bg = 'NONE'            })  -- :=

  -- Types
  highlight(0, 'ocamlTypeName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'ocamlTypeBuiltin',      { fg = colors.turquoise,  bg = 'NONE'            })  -- int, float, string, bool, char, unit, list, array, option, result
  highlight(0, 'ocamlTypeVar',          { fg = colors.turquoise,  bg = 'NONE'            })  -- 'a type variables
  highlight(0, 'ocamlModuleName',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'ocamlSigName',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Signature names

  -- Constructors and Variants
  highlight(0, 'ocamlConstructor',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Variant constructors (Some, None, Ok, Error)
  highlight(0, 'ocamlPolyVariant',      { fg = colors.turquoise,  bg = 'NONE'            })  -- `Polymorphic variants

  -- Functions
  highlight(0, 'ocamlFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'ocamlFunctionCall',     { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'ocamlExternal',         { fg = colors.orange,     bg = 'NONE'            })  -- external

  -- Variables and Parameters
  highlight(0, 'ocamlIdentifier',       { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'ocamlParameter',        { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'ocamlLabel',            { fg = colors.purple,     bg = 'NONE'            })  -- ~labeled arguments
  highlight(0, 'ocamlOptionalLabel',    { fg = colors.purple,     bg = 'NONE'            })  -- ?optional arguments
  highlight(0, 'ocamlRecordField',      { fg = colors.purple,     bg = 'NONE'            })  -- Record fields

  -- Attributes and Extensions
  highlight(0, 'ocamlAttribute',        { fg = colors.pink,       bg = 'NONE'            })  -- [@attr], [@@attr], [@@@attr]
  highlight(0, 'ocamlExtension',        { fg = colors.pink,       bg = 'NONE'            })  -- [%ext], [%%ext]
  highlight(0, 'ocamlPpx',              { fg = colors.pink,       bg = 'NONE'            })  -- PPX extensions

  -- Strings
  highlight(0, 'ocamlString',           { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'ocamlQuotedString',     { fg = colors.redLight,   bg = 'NONE'            })  -- {|quoted strings|}
  highlight(0, 'ocamlStringEscape',     { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'ocamlChar',             { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'ocamlCharEscape',       { fg = colors.pink,       bg = 'NONE'            })  -- Character escapes

  -- Numbers
  highlight(0, 'ocamlNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'ocamlInt',              { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'ocamlInt32',            { fg = colors.greenLight, bg = 'NONE'            })  -- 32-bit integers (1l)
  highlight(0, 'ocamlInt64',            { fg = colors.greenLight, bg = 'NONE'            })  -- 64-bit integers (1L)
  highlight(0, 'ocamlNativeInt',        { fg = colors.greenLight, bg = 'NONE'            })  -- Native integers (1n)
  highlight(0, 'ocamlFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'ocamlHex',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex
  highlight(0, 'ocamlOctal',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0o octal
  highlight(0, 'ocamlBinary',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0b binary

  -- Booleans
  highlight(0, 'ocamlBoolean',          { fg = colors.blue,       bg = 'NONE'            })  -- true, false

  -- Operators
  highlight(0, 'ocamlOperator',         { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'ocamlArrow',            { fg = colors.white,      bg = 'NONE'            })  -- -> arrow
  highlight(0, 'ocamlPipe',             { fg = colors.white,      bg = 'NONE'            })  -- |> pipe
  highlight(0, 'ocamlCompose',          { fg = colors.white,      bg = 'NONE'            })  -- @@ compose
  highlight(0, 'ocamlCons',             { fg = colors.white,      bg = 'NONE'            })  -- :: cons
  highlight(0, 'ocamlConcat',           { fg = colors.white,      bg = 'NONE'            })  -- @ list concat, ^ string concat
  highlight(0, 'ocamlEqual',            { fg = colors.white,      bg = 'NONE'            })  -- = equality
  highlight(0, 'ocamlPatternOr',        { fg = colors.white,      bg = 'NONE'            })  -- | in patterns

  -- Comments
  highlight(0, 'ocamlComment',          { fg = colors.red,        bg = 'NONE'            })  -- (* comments *)
  highlight(0, 'ocamlDocComment',       { fg = colors.red,        bg = 'NONE'            })  -- (** doc comments *)
  highlight(0, 'ocamlTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Special
  highlight(0, 'ocamlUnit',             { fg = colors.blue,       bg = 'NONE'            })  -- () unit
  highlight(0, 'ocamlEmptyList',        { fg = colors.white,      bg = 'NONE'            })  -- [] empty list
  highlight(0, 'ocamlWildcard',         { fg = colors.gray,       bg = 'NONE'            })  -- _ wildcard


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.ocaml)

  -- Variables
  highlight(0, '@variable.ocaml',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.ocaml',      { fg = colors.blue,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.parameter.ocaml',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.ocaml',       { fg = colors.purple,    bg = 'NONE' })  -- Record fields

  -- Constants
  highlight(0, '@constant.ocaml',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.ocaml',      { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Functions
  highlight(0, '@function.ocaml',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.ocaml',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.ocaml',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.method.ocaml',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@constructor.ocaml',           { fg = colors.turquoise, bg = 'NONE' })  -- Variant constructors

  -- Types
  highlight(0, '@type.ocaml',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.ocaml',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition.ocaml',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.ocaml',        { fg = colors.blue,      bg = 'NONE' })  -- Type qualifiers

  -- Attributes
  highlight(0, '@attribute.ocaml',             { fg = colors.pink,      bg = 'NONE' })  -- [@attributes]

  -- Keywords
  highlight(0, '@keyword.ocaml',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.ocaml',      { fg = colors.blue,      bg = 'NONE' })  -- fun, function, let
  highlight(0, '@keyword.type.ocaml',          { fg = colors.blue,      bg = 'NONE' })  -- type, module, sig
  highlight(0, '@keyword.modifier.ocaml',      { fg = colors.blue,      bg = 'NONE' })  -- mutable, private
  highlight(0, '@keyword.return.ocaml',        { fg = colors.blue,      bg = 'NONE' })  -- (implicit return)
  highlight(0, '@keyword.import.ocaml',        { fg = colors.blue,      bg = 'NONE' })  -- open, include
  highlight(0, '@keyword.repeat.ocaml',        { fg = colors.blue,      bg = 'NONE' })  -- for, while
  highlight(0, '@keyword.conditional.ocaml',   { fg = colors.blue,      bg = 'NONE' })  -- if, then, else, match, with
  highlight(0, '@keyword.exception.ocaml',     { fg = colors.blue,      bg = 'NONE' })  -- try, raise, exception
  highlight(0, '@keyword.operator.ocaml',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not

  -- Strings
  highlight(0, '@string.ocaml',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.ocaml',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@character.ocaml',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.ocaml',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.ocaml',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.ocaml',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.ocaml',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.ocaml', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Modules
  highlight(0, '@module.ocaml',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@label.ocaml',                 { fg = colors.purple,    bg = 'NONE' })  -- Labeled arguments
  highlight(0, '@property.ocaml',              { fg = colors.purple,    bg = 'NONE' })  -- Record fields

  -- Operators and Punctuation
  highlight(0, '@operator.ocaml',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.ocaml',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, [||]
  highlight(0, '@punctuation.delimiter.ocaml', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.ocaml',   { fg = colors.white,     bg = 'NONE' })  -- | in patterns


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.ocaml)

  highlight(0, '@lsp.type.variable.ocaml',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.ocaml',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.ocaml',      { fg = colors.purple,    bg = 'NONE' })  -- Record fields
  highlight(0, '@lsp.type.function.ocaml',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.ocaml',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.ocaml',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.ocaml',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.interface.ocaml',     { fg = colors.turquoise, bg = 'NONE' })  -- Signatures
  highlight(0, '@lsp.type.namespace.ocaml',     { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.typeParameter.ocaml', { fg = colors.turquoise, bg = 'NONE' })  -- Type variables 'a
  highlight(0, '@lsp.type.enumMember.ocaml',    { fg = colors.turquoise, bg = 'NONE' })  -- Variant constructors
  highlight(0, '@lsp.type.keyword.ocaml',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.ocaml',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.ocaml',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.ocaml',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.ocaml',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.ocaml',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.ocaml',   { fg = colors.purple,    bg = 'NONE' })  -- Immutable bindings
  highlight(0, '@lsp.typemod.function.declaration.ocaml', { fg = colors.orange,   bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.type.declaration.ocaml',    { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.ocaml', { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types


  -----------------------------------------------------------------------------
  -- OCaml Interface Files (.mli)

  highlight(0, 'ocamlSig',              { fg = colors.blue,       bg = 'NONE'            })  -- sig
  highlight(0, 'ocamlVal',              { fg = colors.blue,       bg = 'NONE'            })  -- val in signatures
end

return ocaml
