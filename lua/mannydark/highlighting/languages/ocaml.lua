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
  highlight(0, 'ocamlKeyword',          { link = "Keyword" })  -- General keywords
  highlight(0, 'ocamlLet',              { fg = colors.blue,       bg = 'NONE'            })  -- let, let rec, and
  highlight(0, 'ocamlIn',               { fg = colors.blue,       bg = 'NONE'            })  -- in
  highlight(0, 'ocamlConditional',      { link = "Conditional" })  -- if, then, else
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
  highlight(0, 'ocamlModuleKeyword',    { link = "Keyword" })  -- struct, sig, end, functor
  highlight(0, 'ocamlInclude',          { fg = colors.blue,       bg = 'NONE'            })  -- open, include
  highlight(0, 'ocamlModPath',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Module paths

  -- Type Definitions
  highlight(0, 'ocamlType',             { link = "Type" })  -- type keyword
  highlight(0, 'ocamlMutable',          { fg = colors.blue,       bg = 'NONE'            })  -- mutable
  highlight(0, 'ocamlPrivate',          { fg = colors.blue,       bg = 'NONE'            })  -- private
  highlight(0, 'ocamlConstraint',       { fg = colors.blue,       bg = 'NONE'            })  -- constraint

  -- Exception Handling
  highlight(0, 'ocamlException',        { fg = colors.blue,       bg = 'NONE'            })  -- exception, raise, try, with
  highlight(0, 'ocamlTry',              { fg = colors.blue,       bg = 'NONE'            })  -- try

  -- Object-Oriented
  highlight(0, 'ocamlObject',           { fg = colors.blue,       bg = 'NONE'            })  -- object, end
  highlight(0, 'ocamlClass',            { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'ocamlMethod',           { link = "Function" })  -- method, val
  highlight(0, 'ocamlInherit',          { fg = colors.blue,       bg = 'NONE'            })  -- inherit
  highlight(0, 'ocamlNew',              { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'ocamlInitializer',      { fg = colors.blue,       bg = 'NONE'            })  -- initializer
  highlight(0, 'ocamlVirtual',          { fg = colors.blue,       bg = 'NONE'            })  -- virtual

  -- References
  highlight(0, 'ocamlRef',              { fg = colors.blue,       bg = 'NONE'            })  -- ref
  highlight(0, 'ocamlDeref',            { link = "Variable" })  -- !
  highlight(0, 'ocamlAssign',           { fg = colors.white,      bg = 'NONE'            })  -- :=

  -- Types
  highlight(0, 'ocamlTypeName',         { link = "Type" })  -- Type names
  highlight(0, 'ocamlTypeBuiltin',      { link = "Type" })  -- int, float, string, bool, char, unit, list, array, option, result
  highlight(0, 'ocamlTypeVar',          { link = "Type" })  -- 'a type variables
  highlight(0, 'ocamlModuleName',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'ocamlSigName',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Signature names

  -- Constructors and Variants
  highlight(0, 'ocamlConstructor',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Variant constructors (Some, None, Ok, Error)
  highlight(0, 'ocamlPolyVariant',      { fg = colors.turquoise,  bg = 'NONE'            })  -- `Polymorphic variants

  -- Functions
  highlight(0, 'ocamlFunction',         { link = "Function" })  -- Function definitions
  highlight(0, 'ocamlFunctionCall',     { link = "Function" })  -- Function calls
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
  highlight(0, 'ocamlString',           { link = "String" })  -- "strings"
  highlight(0, 'ocamlQuotedString',     { link = "String" })  -- {|quoted strings|}
  highlight(0, 'ocamlStringEscape',     { link = "String" })  -- \n, \t, etc.
  highlight(0, 'ocamlChar',             { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'ocamlCharEscape',       { fg = colors.pink,       bg = 'NONE'            })  -- Character escapes

  -- Numbers
  highlight(0, 'ocamlNumber',           { link = "Number" })  -- Numbers
  highlight(0, 'ocamlInt',              { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'ocamlInt32',            { fg = colors.greenLight, bg = 'NONE'            })  -- 32-bit integers (1l)
  highlight(0, 'ocamlInt64',            { fg = colors.greenLight, bg = 'NONE'            })  -- 64-bit integers (1L)
  highlight(0, 'ocamlNativeInt',        { fg = colors.greenLight, bg = 'NONE'            })  -- Native integers (1n)
  highlight(0, 'ocamlFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'ocamlHex',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex
  highlight(0, 'ocamlOctal',            { fg = colors.greenLight, bg = 'NONE'            })  -- 0o octal
  highlight(0, 'ocamlBinary',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0b binary

  -- Booleans
  highlight(0, 'ocamlBoolean',          { link = "Boolean" })  -- true, false

  -- Operators
  highlight(0, 'ocamlOperator',         { link = "Operator" })  -- Operators
  highlight(0, 'ocamlArrow',            { fg = colors.white,      bg = 'NONE'            })  -- -> arrow
  highlight(0, 'ocamlPipe',             { fg = colors.white,      bg = 'NONE'            })  -- |> pipe
  highlight(0, 'ocamlCompose',          { fg = colors.white,      bg = 'NONE'            })  -- @@ compose
  highlight(0, 'ocamlCons',             { fg = colors.white,      bg = 'NONE'            })  -- :: cons
  highlight(0, 'ocamlConcat',           { fg = colors.white,      bg = 'NONE'            })  -- @ list concat, ^ string concat
  highlight(0, 'ocamlEqual',            { fg = colors.white,      bg = 'NONE'            })  -- = equality
  highlight(0, 'ocamlPatternOr',        { fg = colors.white,      bg = 'NONE'            })  -- | in patterns

  -- Comments
  highlight(0, 'ocamlComment',          { link = "Comment" })  -- (* comments *)
  highlight(0, 'ocamlDocComment',       { link = "Comment" })  -- (** doc comments *)
  highlight(0, 'ocamlTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Special
  highlight(0, 'ocamlUnit',             { fg = colors.blue,       bg = 'NONE'            })  -- () unit
  highlight(0, 'ocamlEmptyList',        { fg = colors.white,      bg = 'NONE'            })  -- [] empty list
  highlight(0, 'ocamlWildcard',         { fg = colors.gray,       bg = 'NONE'            })  -- _ wildcard


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.ocaml)

  -- Variables
  highlight(0, '@variable.ocaml',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.ocaml',      { link = "Variable" })  -- Built-in variables
  highlight(0, '@variable.parameter.ocaml',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.ocaml',       { link = "Variable" })  -- Record fields

  -- Constants
  highlight(0, '@constant.ocaml',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.ocaml',      { link = "Constant" })  -- true, false

  -- Functions
  highlight(0, '@function.ocaml',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.ocaml',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.ocaml',      { link = "Function" })  -- Built-in functions
  highlight(0, '@function.method.ocaml',       { link = "Function" })  -- Method definitions
  highlight(0, '@constructor.ocaml',           { fg = colors.turquoise, bg = 'NONE' })  -- Variant constructors

  -- Types
  highlight(0, '@type.ocaml',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.ocaml',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.ocaml',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.ocaml',        { link = "Type" })  -- Type qualifiers

  -- Attributes
  highlight(0, '@attribute.ocaml',             { fg = colors.pink,      bg = 'NONE' })  -- [@attributes]

  -- Keywords
  highlight(0, '@keyword.ocaml',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.ocaml',      { link = "Keyword" })  -- fun, function, let
  highlight(0, '@keyword.type.ocaml',          { link = "Keyword" })  -- type, module, sig
  highlight(0, '@keyword.modifier.ocaml',      { link = "Keyword" })  -- mutable, private
  highlight(0, '@keyword.return.ocaml',        { link = "Keyword" })  -- (implicit return)
  highlight(0, '@keyword.import.ocaml',        { link = "Keyword" })  -- open, include
  highlight(0, '@keyword.repeat.ocaml',        { link = "Keyword" })  -- for, while
  highlight(0, '@keyword.conditional.ocaml',   { link = "Conditional" })  -- if, then, else, match, with
  highlight(0, '@keyword.exception.ocaml',     { link = "Keyword" })  -- try, raise, exception
  highlight(0, '@keyword.operator.ocaml',      { link = "Operator" })  -- and, or, not

  -- Strings
  highlight(0, '@string.ocaml',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.ocaml',         { link = "String" })  -- Escape sequences
  highlight(0, '@character.ocaml',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.ocaml',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.ocaml',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.ocaml',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.ocaml',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.ocaml', { link = "Comment" })  -- Doc comments

  -- Modules
  highlight(0, '@module.ocaml',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@label.ocaml',                 { fg = colors.purple,    bg = 'NONE' })  -- Labeled arguments
  highlight(0, '@property.ocaml',              { fg = colors.purple,    bg = 'NONE' })  -- Record fields

  -- Operators and Punctuation
  highlight(0, '@operator.ocaml',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.ocaml',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, [||]
  highlight(0, '@punctuation.delimiter.ocaml', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.ocaml',   { fg = colors.white,     bg = 'NONE' })  -- | in patterns


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.ocaml)

  highlight(0, '@lsp.type.variable.ocaml',      { link = "Variable" })  -- Variables
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
  highlight(0, '@lsp.type.keyword.ocaml',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.ocaml',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.ocaml',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.ocaml',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.ocaml',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.ocaml',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.ocaml',   { link = "Variable" })  -- Immutable bindings
  highlight(0, '@lsp.typemod.function.declaration.ocaml', { fg = colors.orange,   bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.type.declaration.ocaml',    { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.ocaml', { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types


  -----------------------------------------------------------------------------
  -- OCaml Interface Files (.mli)

  highlight(0, 'ocamlSig',              { fg = colors.blue,       bg = 'NONE'            })  -- sig
  highlight(0, 'ocamlVal',              { fg = colors.blue,       bg = 'NONE'            })  -- val in signatures
end

return ocaml
