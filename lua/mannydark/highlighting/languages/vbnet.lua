-------------------------------------------------------------------------------
-- Visual Basic .NET Files
-- Highlighting for .vb files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local vbnet   = {}


-------------------------------------------------------------------------------
-- Settings

vbnet.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'vbnetStatement',       { fg = colors.blue,       bg = 'NONE'            })  -- General statements
  highlight(0, 'vbnetConditional',     { fg = colors.blue,       bg = 'NONE'            })  -- If, Then, Else, ElseIf, End If, Select, Case
  highlight(0, 'vbnetRepeat',          { fg = colors.blue,       bg = 'NONE'            })  -- For, Next, Each, In, To, Step, While, Do, Loop, Until
  highlight(0, 'vbnetBranch',          { fg = colors.blue,       bg = 'NONE'            })  -- GoTo, Return, Exit, Stop, Continue

  -- Keywords - Exception Handling
  highlight(0, 'vbnetException',       { fg = colors.blue,       bg = 'NONE'            })  -- Try, Catch, Finally, Throw, When, Resume, On Error

  -- Keywords - OOP
  highlight(0, 'vbnetClassWords',      { fg = colors.blue,       bg = 'NONE'            })  -- Class, MustInherit, NotInheritable
  highlight(0, 'vbnetStructureWords',  { fg = colors.blue,       bg = 'NONE'            })  -- Structure
  highlight(0, 'vbnetModuleWords',     { fg = colors.blue,       bg = 'NONE'            })  -- Module
  highlight(0, 'vbnetInterfaceWords',  { fg = colors.blue,       bg = 'NONE'            })  -- Interface, Inherits
  highlight(0, 'vbnetEnumWords',       { fg = colors.blue,       bg = 'NONE'            })  -- Enum
  highlight(0, 'vbnetDelegateWords',   { fg = colors.blue,       bg = 'NONE'            })  -- Delegate
  highlight(0, 'vbnetNamespace',       { fg = colors.blue,       bg = 'NONE'            })  -- Namespace
  highlight(0, 'vbnetNew',             { fg = colors.blue,       bg = 'NONE'            })  -- New
  highlight(0, 'vbnetSelf',            { fg = colors.blue,       bg = 'NONE'            })  -- Me, MyBase, MyClass
  highlight(0, 'vbnetInheritsKeyword', { fg = colors.blue,       bg = 'NONE'            })  -- Inherits
  highlight(0, 'vbnetImplements',      { fg = colors.blue,       bg = 'NONE'            })  -- Implements

  -- Keywords - Access Modifiers
  highlight(0, 'vbnetAccessModifier',  { fg = colors.blue,       bg = 'NONE'            })  -- Public, Private, Protected, Friend, Protected Friend
  highlight(0, 'vbnetTypeAccess',      { fg = colors.blue,       bg = 'NONE'            })  -- Access in type declarations

  -- Keywords - Modifiers
  highlight(0, 'vbnetModifier',        { fg = colors.blue,       bg = 'NONE'            })  -- Shared, Static, ReadOnly, WriteOnly, etc.
  highlight(0, 'vbnetShared',          { fg = colors.blue,       bg = 'NONE'            })  -- Shared
  highlight(0, 'vbnetStatic',          { fg = colors.blue,       bg = 'NONE'            })  -- Static
  highlight(0, 'vbnetReadOnly',        { fg = colors.blue,       bg = 'NONE'            })  -- ReadOnly
  highlight(0, 'vbnetWriteOnly',       { fg = colors.blue,       bg = 'NONE'            })  -- WriteOnly
  highlight(0, 'vbnetWithEvents',      { fg = colors.blue,       bg = 'NONE'            })  -- WithEvents
  highlight(0, 'vbnetShadows',         { fg = colors.blue,       bg = 'NONE'            })  -- Shadows
  highlight(0, 'vbnetOverridable',     { fg = colors.blue,       bg = 'NONE'            })  -- Overridable
  highlight(0, 'vbnetOverrides',       { fg = colors.blue,       bg = 'NONE'            })  -- Overrides
  highlight(0, 'vbnetOverloads',       { fg = colors.blue,       bg = 'NONE'            })  -- Overloads
  highlight(0, 'vbnetMustOverride',    { fg = colors.blue,       bg = 'NONE'            })  -- MustOverride
  highlight(0, 'vbnetNotOverridable',  { fg = colors.blue,       bg = 'NONE'            })  -- NotOverridable
  highlight(0, 'vbnetDefault',         { fg = colors.blue,       bg = 'NONE'            })  -- Default
  highlight(0, 'vbnetPartial',         { fg = colors.blue,       bg = 'NONE'            })  -- Partial
  highlight(0, 'vbnetNarrowing',       { fg = colors.blue,       bg = 'NONE'            })  -- Narrowing
  highlight(0, 'vbnetWidening',        { fg = colors.blue,       bg = 'NONE'            })  -- Widening

  -- Keywords - Procedures
  highlight(0, 'vbnetSubWords',        { fg = colors.blue,       bg = 'NONE'            })  -- Sub, End Sub
  highlight(0, 'vbnetFunctionWords',   { fg = colors.blue,       bg = 'NONE'            })  -- Function, End Function
  highlight(0, 'vbnetPropertyWords',   { fg = colors.blue,       bg = 'NONE'            })  -- Property, End Property, Get, Set
  highlight(0, 'vbnetGetterWords',     { fg = colors.blue,       bg = 'NONE'            })  -- Get
  highlight(0, 'vbnetSetterWords',     { fg = colors.blue,       bg = 'NONE'            })  -- Set
  highlight(0, 'vbnetProcedureWords',  { fg = colors.blue,       bg = 'NONE'            })  -- Procedure-related keywords

  -- Keywords - Parameters
  highlight(0, 'vbnetParameter',       { fg = colors.blue,       bg = 'NONE'            })  -- ByVal, ByRef, Optional, ParamArray

  -- Keywords - Variable Declaration
  highlight(0, 'vbnetDeclare',         { fg = colors.blue,       bg = 'NONE'            })  -- Dim, Const, Declare
  highlight(0, 'vbnetAs',              { fg = colors.blue,       bg = 'NONE'            })  -- As
  highlight(0, 'vbnetGlobal',          { fg = colors.blue,       bg = 'NONE'            })  -- Global

  -- Keywords - Events
  highlight(0, 'vbnetEvent',           { fg = colors.blue,       bg = 'NONE'            })  -- Event
  highlight(0, 'vbnetEventHandler',    { fg = colors.blue,       bg = 'NONE'            })  -- AddHandler, RemoveHandler, RaiseEvent
  highlight(0, 'vbnetHandlesKeyword',  { fg = colors.blue,       bg = 'NONE'            })  -- Handles

  -- Keywords - Array
  highlight(0, 'vbnetArrayHandler',    { fg = colors.blue,       bg = 'NONE'            })  -- ReDim, Erase, Preserve

  -- Keywords - Operators
  highlight(0, 'vbnetOperatorKeyword', { fg = colors.blue,       bg = 'NONE'            })  -- And, Or, Not, Xor, Mod, Is, IsNot, Like, AndAlso, OrElse
  highlight(0, 'vbnetGetType',         { fg = colors.blue,       bg = 'NONE'            })  -- GetType, TypeOf...Is
  highlight(0, 'vbnetAddressOf',       { fg = colors.blue,       bg = 'NONE'            })  -- AddressOf
  highlight(0, 'vbnetNameOf',          { fg = colors.blue,       bg = 'NONE'            })  -- NameOf

  -- Keywords - Type Casting
  highlight(0, 'vbnetCast',            { fg = colors.blue,       bg = 'NONE'            })  -- CType, DirectCast, TryCast
  highlight(0, 'vbnetConversion',      { fg = colors.orange,     bg = 'NONE'            })  -- CBool, CByte, CChar, CDate, CDec, CDbl, CInt, CLng, CObj, CSByte, CShort, CSng, CStr, CUInt, CULng, CUShort

  -- Keywords - With
  highlight(0, 'vbnetWith',            { fg = colors.blue,       bg = 'NONE'            })  -- With, End With

  -- Keywords - Using
  highlight(0, 'vbnetUsing',           { fg = colors.blue,       bg = 'NONE'            })  -- Using, End Using

  -- Keywords - SyncLock
  highlight(0, 'vbnetSyncLock',        { fg = colors.blue,       bg = 'NONE'            })  -- SyncLock, End SyncLock

  -- Keywords - Async
  highlight(0, 'vbnetAsync',           { fg = colors.blue,       bg = 'NONE'            })  -- Async
  highlight(0, 'vbnetAwait',           { fg = colors.blue,       bg = 'NONE'            })  -- Await
  highlight(0, 'vbnetIterator',        { fg = colors.blue,       bg = 'NONE'            })  -- Iterator
  highlight(0, 'vbnetYield',           { fg = colors.blue,       bg = 'NONE'            })  -- Yield

  -- Keywords - Generic Modifiers
  highlight(0, 'vbnetGenericIn',       { fg = colors.blue,       bg = 'NONE'            })  -- In (generic covariance)
  highlight(0, 'vbnetGenericOut',      { fg = colors.blue,       bg = 'NONE'            })  -- Out (generic contravariance)
  highlight(0, 'vbnetOf',              { fg = colors.blue,       bg = 'NONE'            })  -- Of (generics)

  -- LINQ Keywords
  highlight(0, 'vbnetLinq',            { fg = colors.blue,       bg = 'NONE'            })  -- LINQ query keywords
  highlight(0, 'vbnetFrom',            { fg = colors.blue,       bg = 'NONE'            })  -- From
  highlight(0, 'vbnetWhere',           { fg = colors.blue,       bg = 'NONE'            })  -- Where
  highlight(0, 'vbnetSelect',          { fg = colors.blue,       bg = 'NONE'            })  -- Select
  highlight(0, 'vbnetOrderBy',         { fg = colors.blue,       bg = 'NONE'            })  -- Order By
  highlight(0, 'vbnetAscending',       { fg = colors.blue,       bg = 'NONE'            })  -- Ascending
  highlight(0, 'vbnetDescending',      { fg = colors.blue,       bg = 'NONE'            })  -- Descending
  highlight(0, 'vbnetGroupBy',         { fg = colors.blue,       bg = 'NONE'            })  -- Group By
  highlight(0, 'vbnetInto',            { fg = colors.blue,       bg = 'NONE'            })  -- Into
  highlight(0, 'vbnetJoin',            { fg = colors.blue,       bg = 'NONE'            })  -- Join
  highlight(0, 'vbnetOn',              { fg = colors.blue,       bg = 'NONE'            })  -- On
  highlight(0, 'vbnetEquals',          { fg = colors.blue,       bg = 'NONE'            })  -- Equals
  highlight(0, 'vbnetLet',             { fg = colors.blue,       bg = 'NONE'            })  -- Let
  highlight(0, 'vbnetAggregate',       { fg = colors.blue,       bg = 'NONE'            })  -- Aggregate
  highlight(0, 'vbnetDistinct',        { fg = colors.blue,       bg = 'NONE'            })  -- Distinct
  highlight(0, 'vbnetSkip',            { fg = colors.blue,       bg = 'NONE'            })  -- Skip, Skip While
  highlight(0, 'vbnetTake',            { fg = colors.blue,       bg = 'NONE'            })  -- Take, Take While
  highlight(0, 'vbnetGroup',           { fg = colors.blue,       bg = 'NONE'            })  -- Group

  -- Imports
  highlight(0, 'vbnetImports',         { fg = colors.pink,       bg = 'NONE'            })  -- Imports

  -- Types - Built-in
  highlight(0, 'vbnetBuiltinType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Boolean, Byte, Char, Date, Decimal, Double, Integer, etc.
  highlight(0, 'vbnetSystemType',      { fg = colors.turquoise,  bg = 'NONE'            })  -- System.* types
  highlight(0, 'vbnetType',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'vbnetClassName',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'vbnetGenericType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic type parameters (Of T)
  highlight(0, 'vbnetNullableType',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Nullable types (T?)

  -- Functions/Methods
  highlight(0, 'vbnetFunction',        { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'vbnetMethod',          { fg = colors.orange,     bg = 'NONE'            })  -- Method names
  highlight(0, 'vbnetMethodCall',      { fg = colors.orange,     bg = 'NONE'            })  -- Method calls
  highlight(0, 'vbnetConstructor',     { fg = colors.orange,     bg = 'NONE'            })  -- New
  highlight(0, 'vbnetMSFunction',      { fg = colors.orange,     bg = 'NONE'            })  -- Microsoft.VisualBasic functions

  -- Variables/Identifiers
  highlight(0, 'vbnetIdentifier',      { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'vbnetVariable',        { fg = colors.purple,     bg = 'NONE'            })  -- Variables
  highlight(0, 'vbnetField',           { fg = colors.purple,     bg = 'NONE'            })  -- Fields
  highlight(0, 'vbnetProperty',        { fg = colors.purple,     bg = 'NONE'            })  -- Properties
  highlight(0, 'vbnetLocal',           { fg = colors.purple,     bg = 'NONE'            })  -- Local variables

  -- Constants
  highlight(0, 'vbnetConstant',        { fg = colors.purple,     bg = 'NONE'            })  -- Constants
  highlight(0, 'vbnetBoolean',         { fg = colors.blue,       bg = 'NONE'            })  -- True, False
  highlight(0, 'vbnetNothing',         { fg = colors.blue,       bg = 'NONE'            })  -- Nothing
  highlight(0, 'vbnetEnumMember',      { fg = colors.purple,     bg = 'NONE'            })  -- Enum values

  -- Strings
  highlight(0, 'vbnetString',          { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'vbnetCharacter',       { fg = colors.redLight,   bg = 'NONE'            })  -- Character literals "x"c

  -- Numbers
  highlight(0, 'vbnetNumber',          { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'vbnetInteger',         { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'vbnetFloat',           { fg = colors.greenLight, bg = 'NONE'            })  -- Floating-point
  highlight(0, 'vbnetHex',             { fg = colors.greenLight, bg = 'NONE'            })  -- &HFF hex
  highlight(0, 'vbnetOctal',           { fg = colors.greenLight, bg = 'NONE'            })  -- &O octal
  highlight(0, 'vbnetBinary',          { fg = colors.greenLight, bg = 'NONE'            })  -- &B binary (VB14+)
  highlight(0, 'vbnetTypeSpecifier',   { fg = colors.greenLight, bg = 'NONE'            })  -- Type suffixes !, #, $, %, &, @

  -- Date Literals
  highlight(0, 'vbnetDate',            { fg = colors.greenLight, bg = 'NONE'            })  -- #1/1/2024#

  -- Operators
  highlight(0, 'vbnetOperator',        { fg = colors.white,      bg = 'NONE'            })  -- + - * / \ ^ = < > & etc.
  highlight(0, 'vbnetAssignment',      { fg = colors.white,      bg = 'NONE'            })  -- = += -= *= /= \= ^= &= etc.
  highlight(0, 'vbnetComparison',      { fg = colors.white,      bg = 'NONE'            })  -- = <> < > <= >=
  highlight(0, 'vbnetConcatenation',   { fg = colors.white,      bg = 'NONE'            })  -- & +
  highlight(0, 'vbnetArithmetic',      { fg = colors.white,      bg = 'NONE'            })  -- + - * / \ Mod ^
  highlight(0, 'vbnetBitShift',        { fg = colors.white,      bg = 'NONE'            })  -- << >>
  highlight(0, 'vbnetNullConditional', { fg = colors.white,      bg = 'NONE'            })  -- ?. (VB14+)
  highlight(0, 'vbnetNullCoalescing',  { fg = colors.white,      bg = 'NONE'            })  -- If() null-coalescing pattern

  -- Attributes
  highlight(0, 'vbnetAttribute',       { fg = colors.pink,       bg = 'NONE'            })  -- <Attribute()>
  highlight(0, 'vbnetAttributeName',   { fg = colors.pink,       bg = 'NONE'            })  -- Attribute name
  highlight(0, 'vbnetAttributeTarget', { fg = colors.pink,       bg = 'NONE'            })  -- Assembly:, Module:

  -- Preprocessor Directives
  highlight(0, 'vbnetPreCondit',       { fg = colors.pink,       bg = 'NONE'            })  -- #If, #ElseIf, #Else, #End If
  highlight(0, 'vbnetDefine',          { fg = colors.pink,       bg = 'NONE'            })  -- #Const
  highlight(0, 'vbnetInclude',         { fg = colors.pink,       bg = 'NONE'            })  -- #ExternalSource
  highlight(0, 'vbnetRegion',          { fg = colors.pink,       bg = 'NONE'            })  -- #Region, #End Region
  highlight(0, 'vbnetPreProcValue',    { fg = colors.pink,       bg = 'NONE'            })  -- Preprocessor values

  -- XML Literals (VB.NET unique feature)
  highlight(0, 'vbnetXmlLiteral',      { fg = colors.redLight,   bg = 'NONE'            })  -- XML literal content
  highlight(0, 'vbnetXmlTag',          { fg = colors.blue,       bg = 'NONE'            })  -- <tag>
  highlight(0, 'vbnetXmlTagName',      { fg = colors.blue,       bg = 'NONE'            })  -- Tag name
  highlight(0, 'vbnetXmlAttribute',    { fg = colors.purple,     bg = 'NONE'            })  -- Attribute name
  highlight(0, 'vbnetXmlAttributeValue', { fg = colors.redLight, bg = 'NONE'            })  -- Attribute value
  highlight(0, 'vbnetXmlEmbedded',     { fg = colors.pink,       bg = 'NONE'            })  -- <%= embedded expression %>
  highlight(0, 'vbnetXmlNamespace',    { fg = colors.turquoise,  bg = 'NONE'            })  -- xmlns declarations

  -- Comments
  highlight(0, 'vbnetComment',         { fg = colors.red,        bg = 'NONE'            })  -- ' comments and REM
  highlight(0, 'vbnetXmlComment',      { fg = colors.red,        bg = 'NONE'            })  -- ''' XML doc comments
  highlight(0, 'vbnetXmlDocTag',       { fg = colors.green,      bg = 'NONE'            })  -- <summary>, <param>, etc.
  highlight(0, 'vbnetXmlDocTagName',   { fg = colors.green,      bg = 'NONE'            })  -- Tag names in doc
  highlight(0, 'vbnetXmlDocAttribute', { fg = colors.green,      bg = 'NONE'            })  -- name="value" in doc
  highlight(0, 'vbnetTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Obsolete/Error Keywords
  highlight(0, 'vbnetKeywordError',    { fg = colors.red,        bg = 'NONE'            })  -- EndIf, GoSub, Let, Variant, Wend

  -- Error
  highlight(0, 'vbnetErrorHighlight',  { fg = colors.red,        bg = 'NONE'            })  -- Errors

  -- New Clause
  highlight(0, 'vbnetNewClause',       { fg = colors.blue,       bg = 'NONE'            })  -- New clause with object initializer


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.vb)

  -- Variables
  highlight(0, '@variable.vb',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.vb',      { fg = colors.blue,      bg = 'NONE' })  -- Me, MyBase, MyClass
  highlight(0, '@variable.parameter.vb',    { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@variable.member.vb',       { fg = colors.purple,    bg = 'NONE' })  -- Fields

  -- Constants
  highlight(0, '@constant.vb',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.vb',      { fg = colors.blue,      bg = 'NONE' })  -- True, False, Nothing

  -- Functions/Methods
  highlight(0, '@function.vb',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.vb',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.vb',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.vb',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.vb',      { fg = colors.orange,    bg = 'NONE' })  -- CBool, CInt, etc.
  highlight(0, '@constructor.vb',           { fg = colors.orange,    bg = 'NONE' })  -- Constructors (New)

  -- Types
  highlight(0, '@type.vb',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.vb',          { fg = colors.turquoise, bg = 'NONE' })  -- Integer, String, Boolean, etc.
  highlight(0, '@type.definition.vb',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.vb',        { fg = colors.blue,      bg = 'NONE' })  -- Type modifiers

  -- Modules/Namespaces
  highlight(0, '@module.vb',                { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@module.builtin.vb',        { fg = colors.turquoise, bg = 'NONE' })  -- System namespace

  -- Properties
  highlight(0, '@property.vb',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Keywords
  highlight(0, '@keyword.vb',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.type.vb',          { fg = colors.blue,      bg = 'NONE' })  -- Class, Structure, Module, Interface, Enum
  highlight(0, '@keyword.modifier.vb',      { fg = colors.blue,      bg = 'NONE' })  -- Public, Private, Shared, etc.
  highlight(0, '@keyword.function.vb',      { fg = colors.blue,      bg = 'NONE' })  -- Sub, Function, Property
  highlight(0, '@keyword.operator.vb',      { fg = colors.blue,      bg = 'NONE' })  -- And, Or, Not, Is, IsNot, Like, Mod, TypeOf
  highlight(0, '@keyword.return.vb',        { fg = colors.blue,      bg = 'NONE' })  -- Return
  highlight(0, '@keyword.repeat.vb',        { fg = colors.blue,      bg = 'NONE' })  -- For, While, Do, Loop, Each
  highlight(0, '@keyword.conditional.vb',   { fg = colors.blue,      bg = 'NONE' })  -- If, Then, Else, ElseIf, Select, Case
  highlight(0, '@keyword.exception.vb',     { fg = colors.blue,      bg = 'NONE' })  -- Try, Catch, Finally, Throw
  highlight(0, '@keyword.import.vb',        { fg = colors.pink,      bg = 'NONE' })  -- Imports
  highlight(0, '@keyword.coroutine.vb',     { fg = colors.blue,      bg = 'NONE' })  -- Async, Await
  highlight(0, '@keyword.directive.vb',     { fg = colors.pink,      bg = 'NONE' })  -- Preprocessor directives

  -- Labels
  highlight(0, '@label.vb',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels

  -- Strings
  highlight(0, '@string.vb',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.vb',         { fg = colors.pink,      bg = 'NONE' })  -- "" (escaped quote)
  highlight(0, '@character.vb',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.vb',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.vb',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.vb',               { fg = colors.blue,      bg = 'NONE' })  -- True, False

  -- Comments
  highlight(0, '@comment.vb',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.vb', { fg = colors.red,       bg = 'NONE' })  -- XML doc comments

  -- Attributes
  highlight(0, '@attribute.vb',             { fg = colors.pink,      bg = 'NONE' })  -- <Attribute()>
  highlight(0, '@attribute.builtin.vb',     { fg = colors.pink,      bg = 'NONE' })  -- <Obsolete>, <STAThread>, etc.

  -- Operators and Punctuation
  highlight(0, '@operator.vb',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.vb',   { fg = colors.white,     bg = 'NONE' })  -- (), {}
  highlight(0, '@punctuation.delimiter.vb', { fg = colors.white,     bg = 'NONE' })  -- , :
  highlight(0, '@punctuation.special.vb',   { fg = colors.pink,      bg = 'NONE' })  -- # in directives

  -- XML (VB.NET specific)
  highlight(0, '@tag.vb',                   { fg = colors.blue,      bg = 'NONE' })  -- XML tags
  highlight(0, '@tag.attribute.vb',         { fg = colors.purple,    bg = 'NONE' })  -- XML attributes
  highlight(0, '@tag.delimiter.vb',         { fg = colors.white,     bg = 'NONE' })  -- < > /> in XML


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.vb)

  highlight(0, '@lsp.type.variable.vb',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.vb',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.vb',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.field.vb',         { fg = colors.purple,    bg = 'NONE' })  -- Fields
  highlight(0, '@lsp.type.function.vb',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.vb',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.vb',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.vb',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.struct.vb',        { fg = colors.turquoise, bg = 'NONE' })  -- Structures
  highlight(0, '@lsp.type.interface.vb',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.enum.vb',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.vb',    { fg = colors.purple,    bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.delegate.vb',      { fg = colors.turquoise, bg = 'NONE' })  -- Delegates
  highlight(0, '@lsp.type.event.vb',         { fg = colors.purple,    bg = 'NONE' })  -- Events
  highlight(0, '@lsp.type.module.vb',        { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.namespace.vb',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.typeParameter.vb', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type parameters
  highlight(0, '@lsp.type.keyword.vb',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.vb',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.vb',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.vb',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.vb',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.vb',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.xmlDocCommentName.vb', { fg = colors.green, bg = 'NONE' })  -- XML tag names
  highlight(0, '@lsp.type.xmlDocCommentText.vb', { fg = colors.red,   bg = 'NONE' })  -- XML doc text
  highlight(0, '@lsp.type.xmlLiteralName.vb',    { fg = colors.blue,  bg = 'NONE' })  -- XML literal tag names
  highlight(0, '@lsp.type.xmlLiteralText.vb',    { fg = colors.redLight, bg = 'NONE' }) -- XML literal text

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.vb',    { fg = colors.purple,    bg = 'NONE' })  -- ReadOnly fields
  highlight(0, '@lsp.typemod.variable.static.vb',      { fg = colors.purple,    bg = 'NONE' })  -- Shared fields
  highlight(0, '@lsp.typemod.method.declaration.vb',   { fg = colors.orange,    bg = 'NONE' })  -- Method declarations
  highlight(0, '@lsp.typemod.method.static.vb',        { fg = colors.orange,    bg = 'NONE' })  -- Shared methods
  highlight(0, '@lsp.typemod.method.async.vb',         { fg = colors.orange,    bg = 'NONE' })  -- Async methods
  highlight(0, '@lsp.typemod.type.declaration.vb',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.vb',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
  highlight(0, '@lsp.typemod.class.abstract.vb',       { fg = colors.turquoise, bg = 'NONE' })  -- MustInherit classes
  highlight(0, '@lsp.typemod.class.static.vb',         { fg = colors.turquoise, bg = 'NONE' })  -- Module (static class equivalent)
end

return vbnet

