-------------------------------------------------------------------------------
-- C# Files
-- Highlighting for .cs, .csx files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local csharp  = {}


-------------------------------------------------------------------------------
-- Settings

csharp.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'csKeyword',             { link = "Keyword" })  -- General keywords
  highlight(0, 'csStatement',           { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, goto
  highlight(0, 'csConditional',         { link = "Conditional" })  -- if, else, switch, case, default
  highlight(0, 'csRepeat',              { fg = colors.blue,       bg = 'NONE'            })  -- for, foreach, while, do
  highlight(0, 'csLabel',               { fg = colors.blue,       bg = 'NONE'            })  -- case, default, labels

  -- Keywords - Exception Handling
  highlight(0, 'csException',           { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw, when
  highlight(0, 'csChecked',             { fg = colors.blue,       bg = 'NONE'            })  -- checked, unchecked

  -- Keywords - OOP
  highlight(0, 'csClass',               { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'csStruct',              { fg = colors.blue,       bg = 'NONE'            })  -- struct
  highlight(0, 'csInterface',           { fg = colors.blue,       bg = 'NONE'            })  -- interface
  highlight(0, 'csEnum',                { fg = colors.blue,       bg = 'NONE'            })  -- enum
  highlight(0, 'csDelegate',            { fg = colors.blue,       bg = 'NONE'            })  -- delegate
  highlight(0, 'csEvent',               { fg = colors.blue,       bg = 'NONE'            })  -- event
  highlight(0, 'csRecord',              { fg = colors.blue,       bg = 'NONE'            })  -- record (C# 9+)
  highlight(0, 'csNamespace',           { fg = colors.blue,       bg = 'NONE'            })  -- namespace
  highlight(0, 'csNew',                 { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'csThis',                { fg = colors.blue,       bg = 'NONE'            })  -- this
  highlight(0, 'csBase',                { fg = colors.blue,       bg = 'NONE'            })  -- base

  -- Keywords - Modifiers
  highlight(0, 'csModifier',            { fg = colors.blue,       bg = 'NONE'            })  -- Access modifiers and others
  highlight(0, 'csPublic',              { fg = colors.blue,       bg = 'NONE'            })  -- public
  highlight(0, 'csPrivate',             { fg = colors.blue,       bg = 'NONE'            })  -- private
  highlight(0, 'csProtected',           { fg = colors.blue,       bg = 'NONE'            })  -- protected
  highlight(0, 'csInternal',            { fg = colors.blue,       bg = 'NONE'            })  -- internal
  highlight(0, 'csStatic',              { fg = colors.blue,       bg = 'NONE'            })  -- static
  highlight(0, 'csReadonly',            { fg = colors.blue,       bg = 'NONE'            })  -- readonly
  highlight(0, 'csConst',               { fg = colors.blue,       bg = 'NONE'            })  -- const
  highlight(0, 'csVolatile',            { fg = colors.blue,       bg = 'NONE'            })  -- volatile
  highlight(0, 'csAbstract',            { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'csVirtual',             { fg = colors.blue,       bg = 'NONE'            })  -- virtual
  highlight(0, 'csOverride',            { fg = colors.blue,       bg = 'NONE'            })  -- override
  highlight(0, 'csSealed',              { fg = colors.blue,       bg = 'NONE'            })  -- sealed
  highlight(0, 'csExtern',              { fg = colors.blue,       bg = 'NONE'            })  -- extern
  highlight(0, 'csPartial',             { fg = colors.blue,       bg = 'NONE'            })  -- partial
  highlight(0, 'csRequired',            { fg = colors.blue,       bg = 'NONE'            })  -- required (C# 11+)
  highlight(0, 'csFile',                { fg = colors.blue,       bg = 'NONE'            })  -- file (C# 11+)

  -- Keywords - Parameters
  highlight(0, 'csParams',              { fg = colors.blue,       bg = 'NONE'            })  -- params
  highlight(0, 'csRef',                 { fg = colors.blue,       bg = 'NONE'            })  -- ref
  highlight(0, 'csOut',                 { fg = colors.blue,       bg = 'NONE'            })  -- out
  highlight(0, 'csIn',                  { fg = colors.blue,       bg = 'NONE'            })  -- in

  -- Keywords - Async/Await
  highlight(0, 'csAsync',               { fg = colors.blue,       bg = 'NONE'            })  -- async
  highlight(0, 'csAwait',               { fg = colors.blue,       bg = 'NONE'            })  -- await
  highlight(0, 'csYield',               { fg = colors.blue,       bg = 'NONE'            })  -- yield

  -- Keywords - Operators
  highlight(0, 'csOperatorKeyword',     { link = "Operator" })  -- is, as, typeof, sizeof, nameof
  highlight(0, 'csIs',                  { fg = colors.blue,       bg = 'NONE'            })  -- is
  highlight(0, 'csAs',                  { fg = colors.blue,       bg = 'NONE'            })  -- as
  highlight(0, 'csTypeof',              { link = "Type" })  -- typeof
  highlight(0, 'csSizeof',              { fg = colors.blue,       bg = 'NONE'            })  -- sizeof
  highlight(0, 'csNameof',              { fg = colors.blue,       bg = 'NONE'            })  -- nameof
  highlight(0, 'csDefault',             { fg = colors.blue,       bg = 'NONE'            })  -- default
  highlight(0, 'csStackalloc',          { fg = colors.blue,       bg = 'NONE'            })  -- stackalloc

  -- Keywords - Pattern Matching (C# 9+)
  highlight(0, 'csPatternKeyword',      { link = "Keyword" })  -- and, or, not, when
  highlight(0, 'csAnd',                 { fg = colors.blue,       bg = 'NONE'            })  -- and
  highlight(0, 'csOr',                  { fg = colors.blue,       bg = 'NONE'            })  -- or
  highlight(0, 'csNot',                 { fg = colors.blue,       bg = 'NONE'            })  -- not
  highlight(0, 'csWhen',                { fg = colors.blue,       bg = 'NONE'            })  -- when
  highlight(0, 'csVar',                 { link = "Variable" })  -- var
  highlight(0, 'csWith',                { fg = colors.blue,       bg = 'NONE'            })  -- with

  -- Keywords - Unsafe
  highlight(0, 'csUnsafe',              { fg = colors.blue,       bg = 'NONE'            })  -- unsafe
  highlight(0, 'csFixed',               { fg = colors.blue,       bg = 'NONE'            })  -- fixed

  -- Keywords - Locking
  highlight(0, 'csLock',                { fg = colors.blue,       bg = 'NONE'            })  -- lock

  -- Keywords - Using
  highlight(0, 'csUsing',               { fg = colors.pink,       bg = 'NONE'            })  -- using
  highlight(0, 'csGlobal',              { fg = colors.pink,       bg = 'NONE'            })  -- global

  -- Keywords - Conversions
  highlight(0, 'csExplicit',            { fg = colors.blue,       bg = 'NONE'            })  -- explicit
  highlight(0, 'csImplicit',            { fg = colors.blue,       bg = 'NONE'            })  -- implicit
  highlight(0, 'csOperator',            { link = "Operator" })  -- operator

  -- Property Accessors
  highlight(0, 'csGet',                 { fg = colors.blue,       bg = 'NONE'            })  -- get
  highlight(0, 'csSet',                 { fg = colors.blue,       bg = 'NONE'            })  -- set
  highlight(0, 'csInit',                { fg = colors.blue,       bg = 'NONE'            })  -- init (C# 9+)
  highlight(0, 'csAdd',                 { fg = colors.blue,       bg = 'NONE'            })  -- add
  highlight(0, 'csRemove',              { fg = colors.blue,       bg = 'NONE'            })  -- remove
  highlight(0, 'csValue',               { fg = colors.blue,       bg = 'NONE'            })  -- value

  -- LINQ Keywords
  highlight(0, 'csLinq',                { fg = colors.blue,       bg = 'NONE'            })  -- LINQ keywords
  highlight(0, 'csFrom',                { fg = colors.blue,       bg = 'NONE'            })  -- from
  highlight(0, 'csWhere',               { fg = colors.blue,       bg = 'NONE'            })  -- where
  highlight(0, 'csSelect',              { fg = colors.blue,       bg = 'NONE'            })  -- select
  highlight(0, 'csOrderby',             { fg = colors.blue,       bg = 'NONE'            })  -- orderby
  highlight(0, 'csAscending',           { fg = colors.blue,       bg = 'NONE'            })  -- ascending
  highlight(0, 'csDescending',          { fg = colors.blue,       bg = 'NONE'            })  -- descending
  highlight(0, 'csGroup',               { fg = colors.blue,       bg = 'NONE'            })  -- group
  highlight(0, 'csBy',                  { fg = colors.blue,       bg = 'NONE'            })  -- by
  highlight(0, 'csInto',                { fg = colors.blue,       bg = 'NONE'            })  -- into
  highlight(0, 'csJoin',                { fg = colors.blue,       bg = 'NONE'            })  -- join
  highlight(0, 'csOn',                  { fg = colors.blue,       bg = 'NONE'            })  -- on
  highlight(0, 'csEquals',              { fg = colors.blue,       bg = 'NONE'            })  -- equals
  highlight(0, 'csLet',                 { fg = colors.blue,       bg = 'NONE'            })  -- let

  -- Types - Value Types
  highlight(0, 'csType',                { link = "Type" })  -- Type names
  highlight(0, 'csBuiltinType',         { link = "Type" })  -- int, long, float, double, etc.
  highlight(0, 'csIntType',             { link = "Type" })  -- int, uint, long, ulong, short, ushort, byte, sbyte
  highlight(0, 'csFloatType',           { link = "Type" })  -- float, double, decimal
  highlight(0, 'csBoolType',            { link = "Type" })  -- bool
  highlight(0, 'csCharType',            { link = "Type" })  -- char
  highlight(0, 'csVoidType',            { link = "Type" })  -- void
  highlight(0, 'csObjectType',          { link = "Type" })  -- object
  highlight(0, 'csStringType',          { link = "String" })  -- string
  highlight(0, 'csDynamicType',         { link = "Type" })  -- dynamic
  highlight(0, 'csNintType',            { link = "Type" })  -- nint, nuint (C# 9+)

  -- Types - Generic/Nullable
  highlight(0, 'csGenericType',         { link = "Type" })  -- Generic type parameters <T>
  highlight(0, 'csNullableType',        { link = "Type" })  -- Nullable types int?, string?
  highlight(0, 'csClassName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names

  -- Functions/Methods
  highlight(0, 'csMethod',              { link = "Function" })  -- Method definitions
  highlight(0, 'csMethodCall',          { link = "Function" })  -- Method calls
  highlight(0, 'csConstructor',         { fg = colors.orange,     bg = 'NONE'            })  -- Constructor
  highlight(0, 'csDestructor',          { fg = colors.orange,     bg = 'NONE'            })  -- Destructor ~ClassName
  highlight(0, 'csBuiltinMethod',       { link = "Function" })  -- ToString, GetType, etc.

  -- Variables
  highlight(0, 'csIdentifier',          { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'csParameter',           { fg = colors.purple,     bg = 'NONE'            })  -- Method parameters
  highlight(0, 'csField',               { fg = colors.purple,     bg = 'NONE'            })  -- Fields
  highlight(0, 'csProperty',            { fg = colors.purple,     bg = 'NONE'            })  -- Properties
  highlight(0, 'csLocal',               { fg = colors.purple,     bg = 'NONE'            })  -- Local variables

  -- Constants
  highlight(0, 'csConstant',            { link = "Constant" })  -- Constants
  highlight(0, 'csBoolean',             { link = "Boolean" })  -- true, false
  highlight(0, 'csNull',                { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'csEnumMember',          { fg = colors.purple,     bg = 'NONE'            })  -- Enum values

  -- Strings
  highlight(0, 'csString',              { link = "String" })  -- "strings"
  highlight(0, 'csVerbatimString',      { link = "String" })  -- @"verbatim strings"
  highlight(0, 'csInterpolatedString',  { link = "String" })  -- $"interpolated {var}"
  highlight(0, 'csRawString',           { link = "String" })  -- """raw strings""" (C# 11+)
  highlight(0, 'csInterpolation',       { fg = colors.pink,       bg = 'NONE'            })  -- {expression} in interpolated
  highlight(0, 'csInterpolationBrace',  { fg = colors.pink,       bg = 'NONE'            })  -- { } in interpolated
  highlight(0, 'csStringEscape',        { link = "String" })  -- \n, \t, \", \\
  highlight(0, 'csCharacter',           { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals

  -- Numbers
  highlight(0, 'csNumber',              { link = "Number" })  -- Numbers
  highlight(0, 'csInteger',             { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'csFloat',               { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'csHex',                 { fg = colors.greenLight, bg = 'NONE'            })  -- 0xFF hex
  highlight(0, 'csBinary',              { fg = colors.greenLight, bg = 'NONE'            })  -- 0b1010 binary
  highlight(0, 'csNumericSuffix',       { fg = colors.greenLight, bg = 'NONE'            })  -- f, d, m, L, U, UL

  -- Operators
  highlight(0, 'csOperatorSign',        { link = "Operator" })  -- + - * / % = < > ! & | ^ ~
  highlight(0, 'csAssignment',          { fg = colors.white,      bg = 'NONE'            })  -- = += -= *= /= etc.
  highlight(0, 'csComparison',          { fg = colors.white,      bg = 'NONE'            })  -- == != < > <= >=
  highlight(0, 'csLogical',             { fg = colors.white,      bg = 'NONE'            })  -- && || !
  highlight(0, 'csNullConditional',     { link = "Conditional" })  -- ?. ?[] ?.
  highlight(0, 'csNullCoalescing',      { fg = colors.white,      bg = 'NONE'            })  -- ?? ??=
  highlight(0, 'csRange',               { fg = colors.white,      bg = 'NONE'            })  -- .. ^
  highlight(0, 'csLambda',              { fg = colors.white,      bg = 'NONE'            })  -- =>
  highlight(0, 'csSpread',              { fg = colors.white,      bg = 'NONE'            })  -- .. (collection expressions)

  -- Attributes
  highlight(0, 'csAttribute',           { fg = colors.pink,       bg = 'NONE'            })  -- [Attribute]
  highlight(0, 'csAttributeName',       { fg = colors.pink,       bg = 'NONE'            })  -- Attribute name
  highlight(0, 'csAttributeTarget',     { fg = colors.pink,       bg = 'NONE'            })  -- assembly:, module:, etc.

  -- Preprocessor Directives
  highlight(0, 'csPreProc',             { fg = colors.pink,       bg = 'NONE'            })  -- Preprocessor directives
  highlight(0, 'csDefine',              { fg = colors.pink,       bg = 'NONE'            })  -- #define, #undef
  highlight(0, 'csIfDef',               { fg = colors.pink,       bg = 'NONE'            })  -- #if, #elif, #else, #endif
  highlight(0, 'csRegion',              { fg = colors.pink,       bg = 'NONE'            })  -- #region, #endregion
  highlight(0, 'csPragma',              { fg = colors.pink,       bg = 'NONE'            })  -- #pragma
  highlight(0, 'csNullable',            { fg = colors.pink,       bg = 'NONE'            })  -- #nullable
  highlight(0, 'csWarning',             { fg = colors.pink,       bg = 'NONE'            })  -- #warning
  highlight(0, 'csError',               { fg = colors.pink,       bg = 'NONE'            })  -- #error
  highlight(0, 'csLine',                { fg = colors.pink,       bg = 'NONE'            })  -- #line

  -- Comments
  highlight(0, 'csComment',             { link = "Comment" })  -- // and /* */ comments
  highlight(0, 'csLineComment',         { link = "Comment" })  -- // comments
  highlight(0, 'csBlockComment',        { link = "Comment" })  -- /* */ comments
  highlight(0, 'csXmlComment',          { link = "Comment" })  -- /// XML doc comments
  highlight(0, 'csXmlTag',              { fg = colors.green,      bg = 'NONE'            })  -- <summary>, <param>, etc.
  highlight(0, 'csXmlTagName',          { fg = colors.green,      bg = 'NONE'            })  -- Tag names
  highlight(0, 'csXmlAttribute',        { fg = colors.green,      bg = 'NONE'            })  -- name="value"
  highlight(0, 'csTodo',                { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'csErrorHighlight',      { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.c_sharp)

  -- Variables
  highlight(0, '@variable.c_sharp',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.c_sharp',      { link = "Variable" })  -- this, base, value
  highlight(0, '@variable.parameter.c_sharp',    { link = "Variable" })  -- Method parameters
  highlight(0, '@variable.member.c_sharp',       { link = "Variable" })  -- Fields

  -- Constants
  highlight(0, '@constant.c_sharp',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.c_sharp',      { link = "Constant" })  -- true, false, null
  highlight(0, '@constant.macro.c_sharp',        { link = "Constant" })  -- Preprocessor symbols

  -- Functions/Methods
  highlight(0, '@function.c_sharp',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.method.c_sharp',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.c_sharp',  { link = "Function" })  -- Method calls
  highlight(0, '@constructor.c_sharp',           { fg = colors.orange,    bg = 'NONE' })  -- Constructors

  -- Types
  highlight(0, '@type.c_sharp',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.c_sharp',          { link = "Type" })  -- int, string, bool, etc.
  highlight(0, '@type.definition.c_sharp',       { link = "Type" })  -- Type definitions

  -- Modules/Namespaces
  highlight(0, '@module.c_sharp',                { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces

  -- Properties
  highlight(0, '@property.c_sharp',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Keywords
  highlight(0, '@keyword.c_sharp',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.type.c_sharp',          { link = "Keyword" })  -- class, struct, interface, enum
  highlight(0, '@keyword.modifier.c_sharp',      { link = "Keyword" })  -- public, private, static, etc.
  highlight(0, '@keyword.function.c_sharp',      { link = "Keyword" })  -- return
  highlight(0, '@keyword.operator.c_sharp',      { link = "Operator" })  -- is, as, typeof, sizeof, nameof
  highlight(0, '@keyword.return.c_sharp',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.repeat.c_sharp',        { link = "Keyword" })  -- for, foreach, while, do
  highlight(0, '@keyword.conditional.c_sharp',   { link = "Conditional" })  -- if, else, switch, case
  highlight(0, '@keyword.exception.c_sharp',     { link = "Keyword" })  -- try, catch, finally, throw
  highlight(0, '@keyword.import.c_sharp',        { link = "Keyword" })  -- using
  highlight(0, '@keyword.coroutine.c_sharp',     { link = "Keyword" })  -- async, await
  highlight(0, '@keyword.directive.c_sharp',     { link = "Keyword" })  -- Preprocessor directives
  highlight(0, '@keyword.directive.define.c_sharp', { link = "Keyword" })  -- #define, #if

  -- Labels
  highlight(0, '@label.c_sharp',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels

  -- Strings
  highlight(0, '@string.c_sharp',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.c_sharp',         { link = "String" })  -- Escape sequences
  highlight(0, '@character.c_sharp',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals
  highlight(0, '@character.special.c_sharp',     { fg = colors.pink,      bg = 'NONE' })  -- Escape chars

  -- Numbers
  highlight(0, '@number.c_sharp',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.c_sharp',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.c_sharp',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.c_sharp',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.c_sharp', { link = "Comment" })  -- XML doc comments

  -- Attributes
  highlight(0, '@attribute.c_sharp',             { fg = colors.pink,      bg = 'NONE' })  -- [Attribute]
  highlight(0, '@attribute.builtin.c_sharp',     { fg = colors.pink,      bg = 'NONE' })  -- [Serializable], etc.

  -- Operators and Punctuation
  highlight(0, '@operator.c_sharp',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.c_sharp',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.c_sharp', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.c_sharp',   { fg = colors.pink,      bg = 'NONE' })  -- $ in interpolation, # in directives


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.cs)

  highlight(0, '@lsp.type.variable.cs',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.cs',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.cs',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.field.cs',         { fg = colors.purple,    bg = 'NONE' })  -- Fields
  highlight(0, '@lsp.type.function.cs',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.cs',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.cs',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.cs',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.struct.cs',        { fg = colors.turquoise, bg = 'NONE' })  -- Structs
  highlight(0, '@lsp.type.interface.cs',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.enum.cs',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.cs',    { fg = colors.purple,    bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.delegate.cs',      { fg = colors.turquoise, bg = 'NONE' })  -- Delegates
  highlight(0, '@lsp.type.event.cs',         { fg = colors.purple,    bg = 'NONE' })  -- Events
  highlight(0, '@lsp.type.namespace.cs',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.typeParameter.cs', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type parameters
  highlight(0, '@lsp.type.keyword.cs',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.cs',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.cs',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.cs',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.cs',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.cs',       { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.regexp.cs',        { fg = colors.redLight,  bg = 'NONE' })  -- Regex
  highlight(0, '@lsp.type.xmlDocCommentName.cs', { link = "Comment" })  -- XML tag names
  highlight(0, '@lsp.type.xmlDocCommentText.cs', { link = "Comment" })  -- XML doc text

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.cs',    { link = "Variable" })  -- readonly fields
  highlight(0, '@lsp.typemod.variable.static.cs',      { link = "Variable" })  -- static fields
  highlight(0, '@lsp.typemod.method.declaration.cs',   { fg = colors.orange,    bg = 'NONE' })  -- Method declarations
  highlight(0, '@lsp.typemod.method.static.cs',        { fg = colors.orange,    bg = 'NONE' })  -- Static methods
  highlight(0, '@lsp.typemod.method.async.cs',         { fg = colors.orange,    bg = 'NONE' })  -- Async methods
  highlight(0, '@lsp.typemod.type.declaration.cs',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.cs',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@lsp.typemod.class.abstract.cs',       { fg = colors.turquoise, bg = 'NONE' })  -- Abstract classes
  highlight(0, '@lsp.typemod.class.static.cs',         { fg = colors.turquoise, bg = 'NONE' })  -- Static classes
end

return csharp
