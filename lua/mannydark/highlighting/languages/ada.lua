-------------------------------------------------------------------------------
-- Ada Files
-- Highlighting for .ada, .adb, .ads files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local ada     = {}


-------------------------------------------------------------------------------
-- Settings

ada.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'adaKeyword',            { link = "Keyword" })  -- General keywords
  highlight(0, 'adaStatement',          { fg = colors.blue,       bg = 'NONE'            })  -- return, goto, exit
  highlight(0, 'adaConditional',        { link = "Conditional" })  -- if, then, else, elsif, case, when
  highlight(0, 'adaRepeat',             { fg = colors.blue,       bg = 'NONE'            })  -- loop, for, while, reverse
  highlight(0, 'adaLabel',              { fg = colors.blue,       bg = 'NONE'            })  -- Labels (<<label>>)
  highlight(0, 'adaException',          { fg = colors.blue,       bg = 'NONE'            })  -- exception, raise, others

  -- Keywords - Block Structure
  highlight(0, 'adaBegin',              { fg = colors.blue,       bg = 'NONE'            })  -- begin
  highlight(0, 'adaEnd',                { fg = colors.blue,       bg = 'NONE'            })  -- end
  highlight(0, 'adaDeclare',            { fg = colors.blue,       bg = 'NONE'            })  -- declare
  highlight(0, 'adaIs',                 { fg = colors.blue,       bg = 'NONE'            })  -- is
  highlight(0, 'adaDo',                 { fg = colors.blue,       bg = 'NONE'            })  -- do

  -- Keywords - Declarations
  highlight(0, 'adaProcedure',          { fg = colors.blue,       bg = 'NONE'            })  -- procedure
  highlight(0, 'adaFunction',           { link = "Function" })  -- function
  highlight(0, 'adaPackage',            { fg = colors.blue,       bg = 'NONE'            })  -- package
  highlight(0, 'adaBody',               { fg = colors.blue,       bg = 'NONE'            })  -- body
  highlight(0, 'adaGeneric',            { fg = colors.blue,       bg = 'NONE'            })  -- generic
  highlight(0, 'adaSeparate',           { fg = colors.blue,       bg = 'NONE'            })  -- separate
  highlight(0, 'adaSubtype',            { fg = colors.blue,       bg = 'NONE'            })  -- subtype
  highlight(0, 'adaRenames',            { fg = colors.blue,       bg = 'NONE'            })  -- renames
  highlight(0, 'adaOverriding',         { fg = colors.blue,       bg = 'NONE'            })  -- overriding

  -- Keywords - Types
  highlight(0, 'adaTypeKeyword',        { link = "Keyword" })  -- type keyword
  highlight(0, 'adaRecord',             { fg = colors.blue,       bg = 'NONE'            })  -- record
  highlight(0, 'adaArray',              { fg = colors.blue,       bg = 'NONE'            })  -- array
  highlight(0, 'adaAccess',             { fg = colors.blue,       bg = 'NONE'            })  -- access
  highlight(0, 'adaTagged',             { fg = colors.blue,       bg = 'NONE'            })  -- tagged
  highlight(0, 'adaLimited',            { fg = colors.blue,       bg = 'NONE'            })  -- limited
  highlight(0, 'adaPrivate',            { fg = colors.blue,       bg = 'NONE'            })  -- private
  highlight(0, 'adaAbstract',           { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'adaInterface',          { fg = colors.blue,       bg = 'NONE'            })  -- interface
  highlight(0, 'adaNew',                { fg = colors.blue,       bg = 'NONE'            })  -- new
  highlight(0, 'adaRange',              { fg = colors.blue,       bg = 'NONE'            })  -- range
  highlight(0, 'adaDelta',              { fg = colors.blue,       bg = 'NONE'            })  -- delta
  highlight(0, 'adaDigits',             { fg = colors.blue,       bg = 'NONE'            })  -- digits
  highlight(0, 'adaAliased',            { fg = colors.blue,       bg = 'NONE'            })  -- aliased
  highlight(0, 'adaConstant',           { link = "Constant" })  -- constant

  -- Keywords - Tasking
  highlight(0, 'adaTask',               { fg = colors.blue,       bg = 'NONE'            })  -- task
  highlight(0, 'adaProtected',          { fg = colors.blue,       bg = 'NONE'            })  -- protected
  highlight(0, 'adaEntry',              { fg = colors.blue,       bg = 'NONE'            })  -- entry
  highlight(0, 'adaAccept',             { fg = colors.blue,       bg = 'NONE'            })  -- accept
  highlight(0, 'adaSelect',             { fg = colors.blue,       bg = 'NONE'            })  -- select
  highlight(0, 'adaTerminate',          { fg = colors.blue,       bg = 'NONE'            })  -- terminate
  highlight(0, 'adaAbort',              { fg = colors.blue,       bg = 'NONE'            })  -- abort
  highlight(0, 'adaRequeue',            { fg = colors.blue,       bg = 'NONE'            })  -- requeue
  highlight(0, 'adaDelay',              { fg = colors.blue,       bg = 'NONE'            })  -- delay
  highlight(0, 'adaSynchronized',       { fg = colors.blue,       bg = 'NONE'            })  -- synchronized

  -- Keywords - Modifiers and Modes
  highlight(0, 'adaIn',                 { fg = colors.blue,       bg = 'NONE'            })  -- in (parameter mode)
  highlight(0, 'adaOut',                { fg = colors.blue,       bg = 'NONE'            })  -- out (parameter mode)
  highlight(0, 'adaAll',                { fg = colors.blue,       bg = 'NONE'            })  -- all
  highlight(0, 'adaOf',                 { fg = colors.blue,       bg = 'NONE'            })  -- of
  highlight(0, 'adaAt',                 { fg = colors.blue,       bg = 'NONE'            })  -- at
  highlight(0, 'adaUntil',              { fg = colors.blue,       bg = 'NONE'            })  -- until
  highlight(0, 'adaSome',               { fg = colors.blue,       bg = 'NONE'            })  -- some (Ada 2012)

  -- Keywords - Imports/Context
  highlight(0, 'adaWith',               { fg = colors.pink,       bg = 'NONE'            })  -- with (context clause)
  highlight(0, 'adaUse',                { fg = colors.pink,       bg = 'NONE'            })  -- use

  -- Operators (word-based)
  highlight(0, 'adaOperator',           { link = "Operator" })  -- and, or, not, xor, mod, rem, abs
  highlight(0, 'adaAndThen',            { fg = colors.blue,       bg = 'NONE'            })  -- and then (short-circuit)
  highlight(0, 'adaOrElse',             { fg = colors.blue,       bg = 'NONE'            })  -- or else (short-circuit)
  highlight(0, 'adaInOperator',         { link = "Operator" })  -- in (membership test)
  highlight(0, 'adaNotIn',              { fg = colors.blue,       bg = 'NONE'            })  -- not in

  -- Types
  highlight(0, 'adaType',               { link = "Type" })  -- Type names
  highlight(0, 'adaBuiltinType',        { link = "Type" })  -- Integer, Float, Boolean, Character, String
  highlight(0, 'adaSubtypeName',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Natural, Positive, etc.
  highlight(0, 'adaGenericType',        { link = "Type" })  -- Generic formal types
  highlight(0, 'adaClassWide',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Type'Class

  -- Functions
  highlight(0, 'adaFunctionName',       { link = "Function" })  -- Function/procedure names
  highlight(0, 'adaFunctionCall',       { link = "Function" })  -- Function calls
  highlight(0, 'adaBuiltinFunc',        { link = "Function" })  -- Put, Get, Put_Line, etc.
  highlight(0, 'adaEntryName',          { fg = colors.orange,     bg = 'NONE'            })  -- Entry names

  -- Variables and Identifiers
  highlight(0, 'adaIdentifier',         { link = "Variable"         })  -- Identifiers
  highlight(0, 'adaParameter',          { link = "Variable"         })  -- Parameters
  highlight(0, 'adaComponent',          { link = "Variable"         })  -- Record components
  highlight(0, 'adaEnumLiteral',        { link = "Variable"         })  -- Enumeration literals

  -- Constants
  highlight(0, 'adaConstantValue',      { link = "Constant" })  -- Named constants
  highlight(0, 'adaBoolean',            { link = "Boolean" })  -- True, False
  highlight(0, 'adaNull',               { fg = colors.blue,       bg = 'NONE'            })  -- null

  -- Strings
  highlight(0, 'adaString',             { link = "String" })  -- "strings"
  highlight(0, 'adaCharacter',          { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' character literals
  highlight(0, 'adaStringEscape',       { link = "String" })  -- "" (doubled quotes)

  -- Numbers
  highlight(0, 'adaNumber',             { link = "Number" })  -- Numbers
  highlight(0, 'adaInteger',            { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'adaFloat',              { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'adaBasedNumber',        { link = "Number" })  -- 16#FF#, 2#1010#, etc.
  highlight(0, 'adaExponent',           { fg = colors.greenLight, bg = 'NONE'            })  -- E+10, E-5

  -- Operators (symbol-based)
  highlight(0, 'adaOperatorSign',       { link = "Operator" })  -- + - * / = < > & :=
  highlight(0, 'adaAssignment',         { fg = colors.white,      bg = 'NONE'            })  -- :=
  highlight(0, 'adaArrow',              { fg = colors.white,      bg = 'NONE'            })  -- =>
  highlight(0, 'adaBox',                { fg = colors.white,      bg = 'NONE'            })  -- <>
  highlight(0, 'adaRangeOp',            { fg = colors.white,      bg = 'NONE'            })  -- ..
  highlight(0, 'adaExponentOp',         { fg = colors.white,      bg = 'NONE'            })  -- **
  highlight(0, 'adaConcatenation',      { fg = colors.white,      bg = 'NONE'            })  -- &
  highlight(0, 'adaTickMark',           { fg = colors.white,      bg = 'NONE'            })  -- ' (attribute tick)

  -- Attributes ('First, 'Last, 'Range, etc.)
  highlight(0, 'adaAttribute',          { fg = colors.pink,       bg = 'NONE'            })  -- All attributes
  highlight(0, 'adaAttributeFirst',     { fg = colors.pink,       bg = 'NONE'            })  -- 'First
  highlight(0, 'adaAttributeLast',      { fg = colors.pink,       bg = 'NONE'            })  -- 'Last
  highlight(0, 'adaAttributeRange',     { fg = colors.pink,       bg = 'NONE'            })  -- 'Range
  highlight(0, 'adaAttributeLength',    { fg = colors.pink,       bg = 'NONE'            })  -- 'Length
  highlight(0, 'adaAttributeSize',      { fg = colors.pink,       bg = 'NONE'            })  -- 'Size
  highlight(0, 'adaAttributeImage',     { fg = colors.pink,       bg = 'NONE'            })  -- 'Image
  highlight(0, 'adaAttributeValue',     { fg = colors.pink,       bg = 'NONE'            })  -- 'Value
  highlight(0, 'adaAttributeAccess',    { fg = colors.pink,       bg = 'NONE'            })  -- 'Access
  highlight(0, 'adaAttributeClass',     { fg = colors.pink,       bg = 'NONE'            })  -- 'Class
  highlight(0, 'adaAttributePos',       { fg = colors.pink,       bg = 'NONE'            })  -- 'Pos
  highlight(0, 'adaAttributeVal',       { fg = colors.pink,       bg = 'NONE'            })  -- 'Val
  highlight(0, 'adaAttributeAddress',   { fg = colors.pink,       bg = 'NONE'            })  -- 'Address

  -- Pragmas
  highlight(0, 'adaPragma',             { fg = colors.pink,       bg = 'NONE'            })  -- pragma keyword
  highlight(0, 'adaPragmaName',         { fg = colors.pink,       bg = 'NONE'            })  -- Pragma names (Import, Export, etc.)
  highlight(0, 'adaPragmaArg',          { fg = colors.purple,     bg = 'NONE'            })  -- Pragma arguments

  -- Aspects (Ada 2012+)
  highlight(0, 'adaAspect',             { fg = colors.pink,       bg = 'NONE'            })  -- Aspects (Pre, Post, etc.)
  highlight(0, 'adaAspectPre',          { fg = colors.pink,       bg = 'NONE'            })  -- Pre =>
  highlight(0, 'adaAspectPost',         { fg = colors.pink,       bg = 'NONE'            })  -- Post =>
  highlight(0, 'adaAspectInvariant',    { fg = colors.pink,       bg = 'NONE'            })  -- Type_Invariant
  highlight(0, 'adaAspectContract',     { fg = colors.pink,       bg = 'NONE'            })  -- Contract_Cases

  -- Comments
  highlight(0, 'adaComment',            { link = "Comment" })  -- -- comments
  highlight(0, 'adaDocComment',         { link = "Comment" })  -- Documentation comments
  highlight(0, 'adaTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Packages and Modules
  highlight(0, 'adaPackageName',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Package names
  highlight(0, 'adaModuleRef',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Module references (Ada.Text_IO)

  -- Error
  highlight(0, 'adaError',              { fg = colors.red,        bg = 'NONE'            })  -- Syntax errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.ada)

  -- Variables
  highlight(0, '@variable.ada',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.ada',      { link = "Variable" })  -- Built-in variables
  highlight(0, '@variable.parameter.ada',    { link = "Variable" })  -- Parameters
  highlight(0, '@variable.member.ada',       { link = "Variable" })  -- Record components

  -- Constants
  highlight(0, '@constant.ada',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.ada',      { link = "Constant" })  -- True, False, null

  -- Functions
  highlight(0, '@function.ada',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.ada',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.ada',      { link = "Function" })  -- Built-in functions

  -- Types
  highlight(0, '@type.ada',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.ada',          { link = "Type" })  -- Integer, Float, Boolean, etc.
  highlight(0, '@type.definition.ada',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.ada',        { link = "Type" })  -- aliased, constant, limited, abstract

  -- Keywords
  highlight(0, '@keyword.ada',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.ada',      { link = "Keyword" })  -- procedure, function
  highlight(0, '@keyword.type.ada',          { link = "Keyword" })  -- type, subtype, record, array
  highlight(0, '@keyword.modifier.ada',      { link = "Keyword" })  -- aliased, constant, renames, limited
  highlight(0, '@keyword.return.ada',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.repeat.ada',        { link = "Keyword" })  -- loop, for, while
  highlight(0, '@keyword.conditional.ada',   { link = "Conditional" })  -- if, then, else, elsif, case, when
  highlight(0, '@keyword.operator.ada',      { link = "Operator" })  -- and, or, not, xor, mod, rem, abs
  highlight(0, '@keyword.exception.ada',     { link = "Keyword" })  -- exception, raise
  highlight(0, '@keyword.import.ada',        { link = "Keyword" })  -- with, use
  highlight(0, '@keyword.directive.ada',     { link = "Keyword" })  -- pragma

  -- Includes
  highlight(0, '@include.ada',               { fg = colors.pink,      bg = 'NONE' })  -- with, use

  -- Modules
  highlight(0, '@module.ada',                { fg = colors.turquoise, bg = 'NONE' })  -- Package names

  -- Conditionals and Loops
  highlight(0, '@conditional.ada',           { link = "Conditional" })  -- if, elsif, else, case, when
  highlight(0, '@repeat.ada',                { fg = colors.blue,      bg = 'NONE' })  -- loop, for, while
  highlight(0, '@exception.ada',             { fg = colors.blue,      bg = 'NONE' })  -- exception, raise

  -- Strings
  highlight(0, '@string.ada',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.ada',         { link = "String" })  -- "" escape
  highlight(0, '@character.ada',             { fg = colors.redLight,  bg = 'NONE' })  -- Character literals

  -- Numbers
  highlight(0, '@number.ada',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.ada',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.ada',               { link = "Boolean" })  -- True, False

  -- Comments
  highlight(0, '@comment.ada',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.ada', { link = "Comment" })  -- Doc comments

  -- Labels
  highlight(0, '@label.ada',                 { fg = colors.blue,      bg = 'NONE' })  -- <<labels>>

  -- Attributes
  highlight(0, '@attribute.ada',             { fg = colors.pink,      bg = 'NONE' })  -- 'First, 'Last, 'Range, etc.

  -- Properties
  highlight(0, '@property.ada',              { fg = colors.purple,    bg = 'NONE' })  -- Record fields

  -- Operators and Punctuation
  highlight(0, '@operator.ada',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.ada',   { fg = colors.white,     bg = 'NONE' })  -- (), []
  highlight(0, '@punctuation.delimiter.ada', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.ada',   { fg = colors.pink,      bg = 'NONE' })  -- ' (attribute tick)


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.ada)

  highlight(0, '@lsp.type.variable.ada',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.ada',     { link = "Variable" })  -- Parameters
  highlight(0, '@lsp.type.property.ada',      { link = "Variable" })  -- Record components
  highlight(0, '@lsp.type.function.ada',      { link = "Function" })  -- Functions
  highlight(0, '@lsp.type.method.ada',        { link = "Function" })  -- Procedures (as methods)
  highlight(0, '@lsp.type.type.ada',          { link = "Type" })  -- Types
  highlight(0, '@lsp.type.class.ada',         { link = "Type" })  -- Tagged types (classes)
  highlight(0, '@lsp.type.enum.ada',          { link = "Type" })  -- Enumeration types
  highlight(0, '@lsp.type.enumMember.ada',    { fg = colors.purple,    bg = 'NONE' })  -- Enum literals
  highlight(0, '@lsp.type.interface.ada',     { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.namespace.ada',     { fg = colors.turquoise, bg = 'NONE' })  -- Packages
  highlight(0, '@lsp.type.typeParameter.ada', { fg = colors.turquoise, bg = 'NONE' })  -- Generic formal types
  highlight(0, '@lsp.type.keyword.ada',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.modifier.ada',      { fg = colors.blue,      bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.operator.ada',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.ada',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.ada',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.ada',       { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.decorator.ada',     { fg = colors.pink,      bg = 'NONE' })  -- Aspects/Pragmas

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.ada',    { link = "Variable" })  -- constant variables
  highlight(0, '@lsp.typemod.variable.declaration.ada', { link = "Variable" })  -- Variable declarations
  highlight(0, '@lsp.typemod.function.declaration.ada', { link = "Function" })  -- Function declarations
  highlight(0, '@lsp.typemod.function.definition.ada',  { link = "Function" })  -- Function definitions
  highlight(0, '@lsp.typemod.type.declaration.ada',     { link = "Type" })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.ada',  { link = "Type" })  -- Standard types
  highlight(0, '@lsp.typemod.type.abstract.ada',        { link = "Type" })  -- Abstract types
end

return ada
