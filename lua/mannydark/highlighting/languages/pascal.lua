-------------------------------------------------------------------------------
-- Pascal Files
-- Highlighting for .pas, .pp, .inc, .dpr, .lpr files.
-- Supports Standard Pascal, Turbo Pascal, Free Pascal, Object Pascal/Delphi.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local pascal  = {}


-------------------------------------------------------------------------------
-- Settings

pascal.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'pascalKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'pascalStatement',       { fg = colors.blue,       bg = 'NONE'            })  -- goto, exit
  highlight(0, 'pascalConditional',     { fg = colors.blue,       bg = 'NONE'            })  -- if, then, else, case, of
  highlight(0, 'pascalRepeat',          { fg = colors.blue,       bg = 'NONE'            })  -- for, while, repeat, until, do, to, downto
  highlight(0, 'pascalLabel',           { fg = colors.blue,       bg = 'NONE'            })  -- label

  -- Keywords - Block Structure
  highlight(0, 'pascalBegin',           { fg = colors.blue,       bg = 'NONE'            })  -- begin
  highlight(0, 'pascalEnd',             { fg = colors.blue,       bg = 'NONE'            })  -- end
  highlight(0, 'pascalProgram',         { fg = colors.blue,       bg = 'NONE'            })  -- program
  highlight(0, 'pascalUnit',            { fg = colors.blue,       bg = 'NONE'            })  -- unit
  highlight(0, 'pascalLibrary',         { fg = colors.blue,       bg = 'NONE'            })  -- library

  -- Keywords - Declarations
  highlight(0, 'pascalProcedure',       { fg = colors.blue,       bg = 'NONE'            })  -- procedure
  highlight(0, 'pascalFunction',        { fg = colors.blue,       bg = 'NONE'            })  -- function
  highlight(0, 'pascalVar',             { fg = colors.blue,       bg = 'NONE'            })  -- var
  highlight(0, 'pascalConst',           { fg = colors.blue,       bg = 'NONE'            })  -- const
  highlight(0, 'pascalType',            { fg = colors.blue,       bg = 'NONE'            })  -- type keyword
  highlight(0, 'pascalResourcestring',  { fg = colors.blue,       bg = 'NONE'            })  -- resourcestring
  highlight(0, 'pascalThreadvar',       { fg = colors.blue,       bg = 'NONE'            })  -- threadvar

  -- Keywords - Unit Structure
  highlight(0, 'pascalUses',            { fg = colors.pink,       bg = 'NONE'            })  -- uses
  highlight(0, 'pascalInterface',       { fg = colors.blue,       bg = 'NONE'            })  -- interface (unit section)
  highlight(0, 'pascalImplementation',  { fg = colors.blue,       bg = 'NONE'            })  -- implementation
  highlight(0, 'pascalInitialization',  { fg = colors.blue,       bg = 'NONE'            })  -- initialization
  highlight(0, 'pascalFinalization',    { fg = colors.blue,       bg = 'NONE'            })  -- finalization
  highlight(0, 'pascalExports',         { fg = colors.blue,       bg = 'NONE'            })  -- exports

  -- Keywords - Types
  highlight(0, 'pascalRecord',          { fg = colors.blue,       bg = 'NONE'            })  -- record
  highlight(0, 'pascalArray',           { fg = colors.blue,       bg = 'NONE'            })  -- array
  highlight(0, 'pascalSet',             { fg = colors.blue,       bg = 'NONE'            })  -- set
  highlight(0, 'pascalFile',            { fg = colors.blue,       bg = 'NONE'            })  -- file
  highlight(0, 'pascalPacked',          { fg = colors.blue,       bg = 'NONE'            })  -- packed
  highlight(0, 'pascalString',          { fg = colors.blue,       bg = 'NONE'            })  -- string (as type)
  highlight(0, 'pascalOf',              { fg = colors.blue,       bg = 'NONE'            })  -- of

  -- Keywords - Object Pascal/OOP
  highlight(0, 'pascalClass',           { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'pascalObject',          { fg = colors.blue,       bg = 'NONE'            })  -- object
  highlight(0, 'pascalInherited',       { fg = colors.blue,       bg = 'NONE'            })  -- inherited
  highlight(0, 'pascalSelf',            { fg = colors.blue,       bg = 'NONE'            })  -- self
  highlight(0, 'pascalConstructor',     { fg = colors.blue,       bg = 'NONE'            })  -- constructor
  highlight(0, 'pascalDestructor',      { fg = colors.blue,       bg = 'NONE'            })  -- destructor
  highlight(0, 'pascalProperty',        { fg = colors.blue,       bg = 'NONE'            })  -- property
  highlight(0, 'pascalDispinterface',   { fg = colors.blue,       bg = 'NONE'            })  -- dispinterface

  -- Keywords - Exception Handling
  highlight(0, 'pascalTry',             { fg = colors.blue,       bg = 'NONE'            })  -- try
  highlight(0, 'pascalExcept',          { fg = colors.blue,       bg = 'NONE'            })  -- except
  highlight(0, 'pascalFinally',         { fg = colors.blue,       bg = 'NONE'            })  -- finally
  highlight(0, 'pascalRaise',           { fg = colors.blue,       bg = 'NONE'            })  -- raise
  highlight(0, 'pascalOn',              { fg = colors.blue,       bg = 'NONE'            })  -- on (exception filter)

  -- Keywords - Operators (word-based)
  highlight(0, 'pascalOperator',        { fg = colors.blue,       bg = 'NONE'            })  -- and, or, not, xor, div, mod, shl, shr, in, is, as
  highlight(0, 'pascalAnd',             { fg = colors.blue,       bg = 'NONE'            })  -- and
  highlight(0, 'pascalOr',              { fg = colors.blue,       bg = 'NONE'            })  -- or
  highlight(0, 'pascalNot',             { fg = colors.blue,       bg = 'NONE'            })  -- not
  highlight(0, 'pascalXor',             { fg = colors.blue,       bg = 'NONE'            })  -- xor
  highlight(0, 'pascalDiv',             { fg = colors.blue,       bg = 'NONE'            })  -- div
  highlight(0, 'pascalMod',             { fg = colors.blue,       bg = 'NONE'            })  -- mod
  highlight(0, 'pascalShl',             { fg = colors.blue,       bg = 'NONE'            })  -- shl
  highlight(0, 'pascalShr',             { fg = colors.blue,       bg = 'NONE'            })  -- shr
  highlight(0, 'pascalIn',              { fg = colors.blue,       bg = 'NONE'            })  -- in
  highlight(0, 'pascalIs',              { fg = colors.blue,       bg = 'NONE'            })  -- is
  highlight(0, 'pascalAs',              { fg = colors.blue,       bg = 'NONE'            })  -- as

  -- Keywords - Other
  highlight(0, 'pascalWith',            { fg = colors.blue,       bg = 'NONE'            })  -- with
  highlight(0, 'pascalGoto',            { fg = colors.blue,       bg = 'NONE'            })  -- goto
  highlight(0, 'pascalOut',             { fg = colors.blue,       bg = 'NONE'            })  -- out (parameter mode)
  highlight(0, 'pascalInline',          { fg = colors.blue,       bg = 'NONE'            })  -- inline
  highlight(0, 'pascalOperatorKw',      { fg = colors.blue,       bg = 'NONE'            })  -- operator (overloading)
  highlight(0, 'pascalAbsolute',        { fg = colors.blue,       bg = 'NONE'            })  -- absolute
  highlight(0, 'pascalAsm',             { fg = colors.blue,       bg = 'NONE'            })  -- asm

  -- Modifiers - Visibility
  highlight(0, 'pascalPrivate',         { fg = colors.pink,       bg = 'NONE'            })  -- private
  highlight(0, 'pascalProtected',       { fg = colors.pink,       bg = 'NONE'            })  -- protected
  highlight(0, 'pascalPublic',          { fg = colors.pink,       bg = 'NONE'            })  -- public
  highlight(0, 'pascalPublished',       { fg = colors.pink,       bg = 'NONE'            })  -- published
  highlight(0, 'pascalStrict',          { fg = colors.pink,       bg = 'NONE'            })  -- strict

  -- Modifiers - Method
  highlight(0, 'pascalVirtual',         { fg = colors.pink,       bg = 'NONE'            })  -- virtual
  highlight(0, 'pascalDynamic',         { fg = colors.pink,       bg = 'NONE'            })  -- dynamic
  highlight(0, 'pascalOverride',        { fg = colors.pink,       bg = 'NONE'            })  -- override
  highlight(0, 'pascalOverload',        { fg = colors.pink,       bg = 'NONE'            })  -- overload
  highlight(0, 'pascalAbstract',        { fg = colors.pink,       bg = 'NONE'            })  -- abstract
  highlight(0, 'pascalReintroduce',     { fg = colors.pink,       bg = 'NONE'            })  -- reintroduce
  highlight(0, 'pascalStatic',          { fg = colors.pink,       bg = 'NONE'            })  -- static
  highlight(0, 'pascalFinal',           { fg = colors.pink,       bg = 'NONE'            })  -- final
  highlight(0, 'pascalSealed',          { fg = colors.pink,       bg = 'NONE'            })  -- sealed

  -- Modifiers - Calling Convention
  highlight(0, 'pascalCdecl',           { fg = colors.pink,       bg = 'NONE'            })  -- cdecl
  highlight(0, 'pascalStdcall',         { fg = colors.pink,       bg = 'NONE'            })  -- stdcall
  highlight(0, 'pascalSafecall',        { fg = colors.pink,       bg = 'NONE'            })  -- safecall
  highlight(0, 'pascalRegister',        { fg = colors.pink,       bg = 'NONE'            })  -- register
  highlight(0, 'pascalPascalConv',      { fg = colors.pink,       bg = 'NONE'            })  -- pascal (calling convention)

  -- Modifiers - External/Linking
  highlight(0, 'pascalExternal',        { fg = colors.pink,       bg = 'NONE'            })  -- external
  highlight(0, 'pascalExport',          { fg = colors.pink,       bg = 'NONE'            })  -- export
  highlight(0, 'pascalForward',         { fg = colors.pink,       bg = 'NONE'            })  -- forward
  highlight(0, 'pascalName',            { fg = colors.pink,       bg = 'NONE'            })  -- name
  highlight(0, 'pascalIndex',           { fg = colors.pink,       bg = 'NONE'            })  -- index

  -- Modifiers - Property
  highlight(0, 'pascalRead',            { fg = colors.pink,       bg = 'NONE'            })  -- read
  highlight(0, 'pascalWrite',           { fg = colors.pink,       bg = 'NONE'            })  -- write
  highlight(0, 'pascalDefault',         { fg = colors.pink,       bg = 'NONE'            })  -- default
  highlight(0, 'pascalNodefault',       { fg = colors.pink,       bg = 'NONE'            })  -- nodefault
  highlight(0, 'pascalStored',          { fg = colors.pink,       bg = 'NONE'            })  -- stored
  highlight(0, 'pascalImplements',      { fg = colors.pink,       bg = 'NONE'            })  -- implements

  -- Modifiers - Other
  highlight(0, 'pascalDeprecated',      { fg = colors.pink,       bg = 'NONE'            })  -- deprecated
  highlight(0, 'pascalPlatform',        { fg = colors.pink,       bg = 'NONE'            })  -- platform
  highlight(0, 'pascalExperimental',    { fg = colors.pink,       bg = 'NONE'            })  -- experimental
  highlight(0, 'pascalMessage',         { fg = colors.pink,       bg = 'NONE'            })  -- message

  -- Types
  highlight(0, 'pascalTypeName',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'pascalBuiltinType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Integer, Real, Boolean, Char, String, etc.
  highlight(0, 'pascalPointerType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Pointer, ^Type
  highlight(0, 'pascalRangeType',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Subrange types
  highlight(0, 'pascalEnumType',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Enumeration types
  highlight(0, 'pascalClassType',       { fg = colors.turquoise,  bg = 'NONE'            })  -- TObject, TComponent, etc.

  -- Functions
  highlight(0, 'pascalFunctionName',    { fg = colors.orange,     bg = 'NONE'            })  -- Function/procedure names
  highlight(0, 'pascalFunctionCall',    { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'pascalBuiltinFunc',     { fg = colors.orange,     bg = 'NONE'            })  -- WriteLn, ReadLn, Inc, Dec, etc.
  highlight(0, 'pascalMethodName',      { fg = colors.orange,     bg = 'NONE'            })  -- Method names

  -- Variables and Identifiers
  highlight(0, 'pascalIdentifier',      { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'pascalParameter',       { fg = colors.purple,     bg = 'NONE'            })  -- Parameters
  highlight(0, 'pascalField',           { fg = colors.purple,     bg = 'NONE'            })  -- Record/object fields
  highlight(0, 'pascalEnumValue',       { fg = colors.purple,     bg = 'NONE'            })  -- Enumeration values

  -- Constants
  highlight(0, 'pascalConstant',        { fg = colors.purple,     bg = 'NONE'            })  -- Named constants
  highlight(0, 'pascalBoolean',         { fg = colors.blue,       bg = 'NONE'            })  -- True, False
  highlight(0, 'pascalNil',             { fg = colors.blue,       bg = 'NONE'            })  -- nil
  highlight(0, 'pascalResult',          { fg = colors.blue,       bg = 'NONE'            })  -- Result (implicit return)

  -- Strings
  highlight(0, 'pascalStringLiteral',   { fg = colors.redLight,   bg = 'NONE'            })  -- 'strings'
  highlight(0, 'pascalCharLiteral',     { fg = colors.redLight,   bg = 'NONE'            })  -- 'c', #65
  highlight(0, 'pascalCharCode',        { fg = colors.redLight,   bg = 'NONE'            })  -- #65, #$41

  -- Numbers
  highlight(0, 'pascalNumber',          { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'pascalInteger',         { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'pascalFloat',           { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'pascalHex',             { fg = colors.greenLight, bg = 'NONE'            })  -- $FF hex
  highlight(0, 'pascalBinary',          { fg = colors.greenLight, bg = 'NONE'            })  -- %1010 binary
  highlight(0, 'pascalOctal',           { fg = colors.greenLight, bg = 'NONE'            })  -- &77 octal

  -- Operators (symbol-based)
  highlight(0, 'pascalOperatorSign',    { fg = colors.white,      bg = 'NONE'            })  -- + - * / = < > @ ^
  highlight(0, 'pascalAssignment',      { fg = colors.white,      bg = 'NONE'            })  -- :=
  highlight(0, 'pascalComparison',      { fg = colors.white,      bg = 'NONE'            })  -- = <> < > <= >=
  highlight(0, 'pascalRange',           { fg = colors.white,      bg = 'NONE'            })  -- ..
  highlight(0, 'pascalPointerOp',       { fg = colors.white,      bg = 'NONE'            })  -- @ ^
  highlight(0, 'pascalDelimiter',       { fg = colors.white,      bg = 'NONE'            })  -- ; , : .

  -- Compiler Directives
  highlight(0, 'pascalDirective',       { fg = colors.pink,       bg = 'NONE'            })  -- {$...} directives
  highlight(0, 'pascalInclude',         { fg = colors.pink,       bg = 'NONE'            })  -- {$I ...}, {$INCLUDE ...}
  highlight(0, 'pascalDefine',          { fg = colors.pink,       bg = 'NONE'            })  -- {$DEFINE ...}
  highlight(0, 'pascalIfDef',           { fg = colors.pink,       bg = 'NONE'            })  -- {$IFDEF ...}, {$IFNDEF ...}
  highlight(0, 'pascalMode',            { fg = colors.pink,       bg = 'NONE'            })  -- {$MODE ...}

  -- Comments
  highlight(0, 'pascalComment',         { fg = colors.red,        bg = 'NONE'            })  -- { }, (* *), //
  highlight(0, 'pascalBraceComment',    { fg = colors.red,        bg = 'NONE'            })  -- { }
  highlight(0, 'pascalParenComment',    { fg = colors.red,        bg = 'NONE'            })  -- (* *)
  highlight(0, 'pascalLineComment',     { fg = colors.red,        bg = 'NONE'            })  -- //
  highlight(0, 'pascalTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'pascalError',           { fg = colors.red,        bg = 'NONE'            })  -- Syntax errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.pascal)

  -- Variables
  highlight(0, '@variable.pascal',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.pascal',      { fg = colors.blue,      bg = 'NONE' })  -- Self, Result
  highlight(0, '@variable.parameter.pascal',    { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@variable.member.pascal',       { fg = colors.purple,    bg = 'NONE' })  -- Record/object fields

  -- Constants
  highlight(0, '@constant.pascal',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.pascal',      { fg = colors.blue,      bg = 'NONE' })  -- True, False, nil

  -- Functions
  highlight(0, '@function.pascal',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.pascal',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.pascal',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.method.pascal',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@method.pascal',                { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@method.call.pascal',           { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@constructor.pascal',           { fg = colors.orange,    bg = 'NONE' })  -- Constructors

  -- Types
  highlight(0, '@type.pascal',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.pascal',          { fg = colors.turquoise, bg = 'NONE' })  -- Integer, String, Boolean, etc.
  highlight(0, '@type.definition.pascal',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.pascal',        { fg = colors.blue,      bg = 'NONE' })  -- packed

  -- Keywords
  highlight(0, '@keyword.pascal',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.pascal',      { fg = colors.blue,      bg = 'NONE' })  -- procedure, function
  highlight(0, '@keyword.type.pascal',          { fg = colors.blue,      bg = 'NONE' })  -- type, record, class, object
  highlight(0, '@keyword.modifier.pascal',      { fg = colors.pink,      bg = 'NONE' })  -- virtual, override, private, etc.
  highlight(0, '@keyword.return.pascal',        { fg = colors.blue,      bg = 'NONE' })  -- exit
  highlight(0, '@keyword.repeat.pascal',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, repeat
  highlight(0, '@keyword.conditional.pascal',   { fg = colors.blue,      bg = 'NONE' })  -- if, then, else, case
  highlight(0, '@keyword.operator.pascal',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not, xor, div, mod
  highlight(0, '@keyword.exception.pascal',     { fg = colors.blue,      bg = 'NONE' })  -- try, except, finally, raise
  highlight(0, '@keyword.import.pascal',        { fg = colors.pink,      bg = 'NONE' })  -- uses
  highlight(0, '@keyword.directive.pascal',     { fg = colors.pink,      bg = 'NONE' })  -- Compiler directives

  -- Includes
  highlight(0, '@include.pascal',               { fg = colors.pink,      bg = 'NONE' })  -- uses

  -- Conditionals and Loops
  highlight(0, '@conditional.pascal',           { fg = colors.blue,      bg = 'NONE' })  -- if, then, else, case
  highlight(0, '@repeat.pascal',                { fg = colors.blue,      bg = 'NONE' })  -- for, while, repeat
  highlight(0, '@exception.pascal',             { fg = colors.blue,      bg = 'NONE' })  -- try, except, finally, raise

  -- Strings
  highlight(0, '@string.pascal',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.pascal',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@character.pascal',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.pascal',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.pascal',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.pascal',               { fg = colors.blue,      bg = 'NONE' })  -- True, False

  -- Comments
  highlight(0, '@comment.pascal',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.pascal', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Labels
  highlight(0, '@label.pascal',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels

  -- Attributes (directives)
  highlight(0, '@attribute.pascal',             { fg = colors.pink,      bg = 'NONE' })  -- Compiler directives

  -- Properties
  highlight(0, '@property.pascal',              { fg = colors.purple,    bg = 'NONE' })  -- Properties and fields

  -- Operators and Punctuation
  highlight(0, '@operator.pascal',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.pascal',   { fg = colors.white,     bg = 'NONE' })  -- (), []
  highlight(0, '@punctuation.delimiter.pascal', { fg = colors.white,     bg = 'NONE' })  -- , ; : .
  highlight(0, '@punctuation.special.pascal',   { fg = colors.pink,      bg = 'NONE' })  -- {$ } directive brackets


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.pascal)

  highlight(0, '@lsp.type.variable.pascal',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.pascal',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.pascal',      { fg = colors.purple,    bg = 'NONE' })  -- Properties/fields
  highlight(0, '@lsp.type.function.pascal',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.pascal',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.pascal',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.pascal',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.enum.pascal',          { fg = colors.turquoise, bg = 'NONE' })  -- Enumeration types
  highlight(0, '@lsp.type.enumMember.pascal',    { fg = colors.purple,    bg = 'NONE' })  -- Enum values
  highlight(0, '@lsp.type.interface.pascal',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.namespace.pascal',     { fg = colors.turquoise, bg = 'NONE' })  -- Units
  highlight(0, '@lsp.type.typeParameter.pascal', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type parameters
  highlight(0, '@lsp.type.keyword.pascal',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.modifier.pascal',      { fg = colors.pink,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.pascal',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.pascal',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.pascal',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.pascal',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.decorator.pascal',     { fg = colors.pink,      bg = 'NONE' })  -- Attributes/directives

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.pascal',    { fg = colors.purple,    bg = 'NONE' })  -- const variables
  highlight(0, '@lsp.typemod.variable.declaration.pascal', { fg = colors.purple,    bg = 'NONE' })  -- Variable declarations
  highlight(0, '@lsp.typemod.function.declaration.pascal', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.definition.pascal',  { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@lsp.typemod.method.virtual.pascal',       { fg = colors.orange,    bg = 'NONE' })  -- Virtual methods
  highlight(0, '@lsp.typemod.method.abstract.pascal',      { fg = colors.orange,    bg = 'NONE' })  -- Abstract methods
  highlight(0, '@lsp.typemod.type.declaration.pascal',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.pascal',  { fg = colors.turquoise, bg = 'NONE' })  -- Standard types
  highlight(0, '@lsp.typemod.class.abstract.pascal',       { fg = colors.turquoise, bg = 'NONE' })  -- Abstract classes
end

return pascal
