-------------------------------------------------------------------------------
-- D Files
-- Highlighting for .d, .di files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local d       = {}


-------------------------------------------------------------------------------
-- Settings

d.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'dKeyword',            { link = "Keyword" })  -- General keywords
  highlight(0, 'dStatement',          { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, goto
  highlight(0, 'dConditional',        { link = "Conditional" })  -- if, else, switch, case, default
  highlight(0, 'dRepeat',             { fg = colors.blue,       bg = 'NONE'            })  -- while, do, for, foreach, foreach_reverse
  highlight(0, 'dLabel',              { fg = colors.blue,       bg = 'NONE'            })  -- Labels
  highlight(0, 'dOperator',           { link = "Operator" })  -- is, !is, in, !in, cast, new, delete, typeof, typeid
  highlight(0, 'dWith',               { fg = colors.blue,       bg = 'NONE'            })  -- with

  -- Storage Classes
  highlight(0, 'dStorageClass',       { fg = colors.blue,       bg = 'NONE'            })  -- static, extern, const, immutable, shared, auto, scope, ref, out, lazy, final, override, abstract
  highlight(0, 'dConst',              { fg = colors.blue,       bg = 'NONE'            })  -- const
  highlight(0, 'dImmutable',          { fg = colors.blue,       bg = 'NONE'            })  -- immutable
  highlight(0, 'dShared',             { fg = colors.blue,       bg = 'NONE'            })  -- shared
  highlight(0, 'dScope',              { fg = colors.blue,       bg = 'NONE'            })  -- scope
  highlight(0, 'dInout',              { fg = colors.blue,       bg = 'NONE'            })  -- inout
  highlight(0, 'dRef',                { fg = colors.blue,       bg = 'NONE'            })  -- ref, out
  highlight(0, 'dLazy',               { fg = colors.blue,       bg = 'NONE'            })  -- lazy
  highlight(0, 'dAuto',               { fg = colors.blue,       bg = 'NONE'            })  -- auto
  highlight(0, 'dFinal',              { fg = colors.blue,       bg = 'NONE'            })  -- final
  highlight(0, 'dOverride',           { fg = colors.blue,       bg = 'NONE'            })  -- override
  highlight(0, 'dAbstract',           { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'dSynchronized',       { fg = colors.blue,       bg = 'NONE'            })  -- synchronized

  -- Visibility
  highlight(0, 'dVisibility',         { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected, package, export

  -- Structure/Type Definitions
  highlight(0, 'dStructure',          { fg = colors.blue,       bg = 'NONE'            })  -- struct, class, interface, union, enum, alias
  highlight(0, 'dClass',              { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'dInterface',          { fg = colors.blue,       bg = 'NONE'            })  -- interface
  highlight(0, 'dStruct',             { fg = colors.blue,       bg = 'NONE'            })  -- struct
  highlight(0, 'dUnion',              { fg = colors.blue,       bg = 'NONE'            })  -- union
  highlight(0, 'dEnum',               { fg = colors.blue,       bg = 'NONE'            })  -- enum
  highlight(0, 'dAlias',              { fg = colors.blue,       bg = 'NONE'            })  -- alias
  highlight(0, 'dTemplate',           { fg = colors.blue,       bg = 'NONE'            })  -- template
  highlight(0, 'dMixin',              { fg = colors.blue,       bg = 'NONE'            })  -- mixin

  -- Modules
  highlight(0, 'dModule',             { fg = colors.blue,       bg = 'NONE'            })  -- module
  highlight(0, 'dImport',             { fg = colors.blue,       bg = 'NONE'            })  -- import

  -- Functions
  highlight(0, 'dFunction',           { link = "Function" })  -- function keyword
  highlight(0, 'dDelegate',           { fg = colors.blue,       bg = 'NONE'            })  -- delegate

  -- Exception Handling
  highlight(0, 'dException',          { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw
  highlight(0, 'dAssert',             { fg = colors.blue,       bg = 'NONE'            })  -- assert

  -- Contracts (Design by Contract)
  highlight(0, 'dContract',           { fg = colors.blue,       bg = 'NONE'            })  -- in, out, invariant
  highlight(0, 'dInvariant',          { fg = colors.blue,       bg = 'NONE'            })  -- invariant
  highlight(0, 'dBody',               { fg = colors.blue,       bg = 'NONE'            })  -- body (deprecated, use do)

  -- Conditional Compilation
  highlight(0, 'dDebug',              { fg = colors.blue,       bg = 'NONE'            })  -- debug
  highlight(0, 'dVersion',            { fg = colors.blue,       bg = 'NONE'            })  -- version
  highlight(0, 'dStaticIf',           { fg = colors.blue,       bg = 'NONE'            })  -- static if, static foreach, static assert

  -- Testing
  highlight(0, 'dUnittest',           { fg = colors.blue,       bg = 'NONE'            })  -- unittest

  -- Special
  highlight(0, 'dDeprecated',         { fg = colors.blue,       bg = 'NONE'            })  -- deprecated
  highlight(0, 'dPragma',             { fg = colors.blue,       bg = 'NONE'            })  -- pragma
  highlight(0, 'dAsm',                { fg = colors.blue,       bg = 'NONE'            })  -- asm
  highlight(0, 'dExtern',             { fg = colors.blue,       bg = 'NONE'            })  -- extern(C), extern(C++), extern(D)

  -- Attributes (with @)
  highlight(0, 'dAttribute',          { fg = colors.pink,       bg = 'NONE'            })  -- @safe, @trusted, @system, @nogc, @property, @disable, @live
  highlight(0, 'dSafe',               { fg = colors.pink,       bg = 'NONE'            })  -- @safe
  highlight(0, 'dTrusted',            { fg = colors.pink,       bg = 'NONE'            })  -- @trusted
  highlight(0, 'dSystem',             { fg = colors.pink,       bg = 'NONE'            })  -- @system
  highlight(0, 'dNogc',               { fg = colors.pink,       bg = 'NONE'            })  -- @nogc
  highlight(0, 'dProperty',           { fg = colors.pink,       bg = 'NONE'            })  -- @property
  highlight(0, 'dDisable',            { fg = colors.pink,       bg = 'NONE'            })  -- @disable
  highlight(0, 'dUDA',                { fg = colors.pink,       bg = 'NONE'            })  -- User-defined attributes

  -- Function Attributes (without @)
  highlight(0, 'dFuncAttr',           { link = "Function" })  -- pure, nothrow

  -- Scope Guards
  highlight(0, 'dScopeGuard',         { fg = colors.blue,       bg = 'NONE'            })  -- scope(exit), scope(success), scope(failure)

  -- Types
  highlight(0, 'dType',               { link = "Type" })  -- Type names
  highlight(0, 'dBasicType',          { link = "Type" })  -- bool, byte, ubyte, short, ushort, int, uint, long, ulong, float, double, real, char, wchar, dchar, void
  highlight(0, 'dComplexType',        { link = "Type" })  -- cfloat, cdouble, creal, ifloat, idouble, ireal
  highlight(0, 'dSizeType',           { link = "Type" })  -- size_t, ptrdiff_t
  highlight(0, 'dStringType',         { link = "String" })  -- string, wstring, dstring
  highlight(0, 'dTypeName',           { link = "Type" })  -- User-defined types

  -- Constants
  highlight(0, 'dConstant',           { link = "Constant" })  -- Constants
  highlight(0, 'dBoolean',            { link = "Boolean" })  -- true, false
  highlight(0, 'dNull',               { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'dThis',               { fg = colors.blue,       bg = 'NONE'            })  -- this
  highlight(0, 'dSuper',              { fg = colors.blue,       bg = 'NONE'            })  -- super

  -- Special Tokens
  highlight(0, 'dSpecialToken',       { fg = colors.blue,       bg = 'NONE'            })  -- __FILE__, __LINE__, __MODULE__, __FUNCTION__, __PRETTY_FUNCTION__

  -- Functions (definitions and calls)
  highlight(0, 'dFuncDef',            { link = "Function" })  -- Function definitions
  highlight(0, 'dFuncCall',           { link = "Function" })  -- Function calls
  highlight(0, 'dMethod',             { link = "Function" })  -- Methods

  -- Variables
  highlight(0, 'dIdentifier',         { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'dParameter',          { fg = colors.purple,     bg = 'NONE'            })  -- Function parameters
  highlight(0, 'dMember',             { fg = colors.purple,     bg = 'NONE'            })  -- Struct/class members

  -- Strings
  highlight(0, 'dString',             { link = "String" })  -- "strings"
  highlight(0, 'dWysiwyg',            { fg = colors.redLight,   bg = 'NONE'            })  -- `wysiwyg strings` and r"raw strings"
  highlight(0, 'dHexString',          { link = "String" })  -- x"00 FF" hex strings
  highlight(0, 'dDelimitedString',    { link = "Delimiter" })  -- q"[delimited strings]"
  highlight(0, 'dTokenString',        { link = "String" })  -- q{token strings}
  highlight(0, 'dCharacter',          { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'dEscapeSequence',     { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, \x, etc.

  -- Numbers
  highlight(0, 'dNumber',             { link = "Number" })  -- Numbers
  highlight(0, 'dInteger',            { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'dFloat',              { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'dBinary',             { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary
  highlight(0, 'dHex',                { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'dOctal',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0o777 octal (deprecated)

  -- Operators
  highlight(0, 'dOperatorSign',       { link = "Operator" })  -- + - * / % = < > ! & | ^ ~ ? :
  highlight(0, 'dSlice',              { fg = colors.white,      bg = 'NONE'            })  -- .. slice operator
  highlight(0, 'dConcat',             { fg = colors.white,      bg = 'NONE'            })  -- ~ concatenation
  highlight(0, 'dArrow',              { fg = colors.white,      bg = 'NONE'            })  -- => lambda arrow

  -- Comments
  highlight(0, 'dComment',            { link = "Comment" })  -- // and /* */ and /+ +/ comments
  highlight(0, 'dLineComment',        { link = "Comment" })  -- // comments
  highlight(0, 'dBlockComment',       { link = "Comment" })  -- /* */ comments
  highlight(0, 'dNestedComment',      { link = "Comment" })  -- /+ +/ nested comments
  highlight(0, 'dTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- DDoc (Documentation)
  highlight(0, 'dDocComment',         { link = "Comment" })  -- DDoc comments
  highlight(0, 'dDocTag',             { fg = colors.green,      bg = 'NONE'            })  -- Params:, Returns:, Throws:, etc.
  highlight(0, 'dDocParam',           { fg = colors.purple,     bg = 'NONE'            })  -- Parameter names in docs


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.d)

  -- Variables
  highlight(0, '@variable.d',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.d',      { link = "Variable" })  -- this, super
  highlight(0, '@variable.parameter.d',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.d',       { link = "Variable" })  -- Struct/class members

  -- Constants
  highlight(0, '@constant.d',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.d',      { link = "Constant" })  -- true, false, null

  -- Functions
  highlight(0, '@function.d',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.d',         { link = "Function" })  -- Function calls
  highlight(0, '@function.method.d',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.d',  { link = "Function" })  -- Method calls
  highlight(0, '@function.builtin.d',      { link = "Function" })  -- Built-in functions
  highlight(0, '@constructor.d',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructors

  -- Types
  highlight(0, '@type.d',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.d',          { link = "Type" })  -- int, float, string, etc.
  highlight(0, '@type.definition.d',       { link = "Type" })  -- alias, typedef
  highlight(0, '@type.qualifier.d',        { link = "Type" })  -- const, immutable, shared

  -- Attributes
  highlight(0, '@attribute.d',             { fg = colors.pink,      bg = 'NONE' })  -- @attributes

  -- Keywords
  highlight(0, '@keyword.d',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.d',      { link = "Keyword" })  -- function, delegate
  highlight(0, '@keyword.type.d',          { link = "Keyword" })  -- class, struct, interface, union, enum
  highlight(0, '@keyword.modifier.d',      { link = "Keyword" })  -- static, final, override, abstract, pure, nothrow
  highlight(0, '@keyword.return.d',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.import.d',        { link = "Keyword" })  -- import, module
  highlight(0, '@keyword.repeat.d',        { link = "Keyword" })  -- for, foreach, while, do
  highlight(0, '@keyword.conditional.d',   { link = "Conditional" })  -- if, else, switch, case, static if
  highlight(0, '@keyword.exception.d',     { link = "Keyword" })  -- try, catch, finally, throw, assert
  highlight(0, '@keyword.operator.d',      { link = "Operator" })  -- is, !is, in, !in, cast, new, delete

  -- Strings
  highlight(0, '@string.d',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.d',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.d',        { link = "String" })  -- Wysiwyg/raw strings
  highlight(0, '@character.d',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.d',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.d',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.d',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.d',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.d', { link = "Comment" })  -- DDoc comments

  -- Labels and Modules
  highlight(0, '@label.d',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels
  highlight(0, '@module.d',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@property.d',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.d',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.d',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.d', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.d',   { fg = colors.pink,      bg = 'NONE' })  -- @ in attributes


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.d)

  highlight(0, '@lsp.type.variable.d',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.d',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.d',      { fg = colors.purple,    bg = 'NONE' })  -- Members
  highlight(0, '@lsp.type.function.d',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.d',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.d',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.struct.d',        { fg = colors.turquoise, bg = 'NONE' })  -- Structs
  highlight(0, '@lsp.type.interface.d',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.enum.d',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.d',    { fg = colors.purple,    bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.type.d',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter.d', { fg = colors.turquoise, bg = 'NONE' })  -- Template parameters
  highlight(0, '@lsp.type.namespace.d',     { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.keyword.d',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.d',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.d',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.d',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.d',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.d',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.d',    { link = "Variable" })  -- const/immutable variables
  highlight(0, '@lsp.typemod.function.declaration.d', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.static.d',      { fg = colors.orange,    bg = 'NONE' })  -- Static functions
  highlight(0, '@lsp.typemod.class.declaration.d',    { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.d',  { fg = colors.turquoise, bg = 'NONE' })  -- Phobos types
end

return d
