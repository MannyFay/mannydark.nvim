-------------------------------------------------------------------------------
-- PureScript
-------------------------------------------------------------------------------

local colors     = require('mannydark.palette')
local highlight  = vim.api.nvim_set_hl
local purescript = {}


-------------------------------------------------------------------------------
-- Settings

purescript.setupHighlighting = function()


  -------------------------------------------------------------------------
  -- Vim PureScript Syntax Groups (purescript-vim)
  -------------------------------------------------------------------------

  -- Keywords and Control Flow
  highlight(0, 'purescriptConditional',       { link = "Conditional" })  -- if, then, else
  highlight(0, 'purescriptStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- do, case, of, in, ado, forall
  highlight(0, 'purescriptLet',               { fg = colors.blue,       bg = 'NONE'            })  -- let
  highlight(0, 'purescriptWhere',             { fg = colors.blue,       bg = 'NONE'            })  -- where
  highlight(0, 'purescriptKeyword',           { link = "Keyword" })  -- Generic keywords
  highlight(0, 'purescriptForall',            { fg = colors.blue,       bg = 'NONE'            })  -- forall, ∀

  -- Module System
  highlight(0, 'purescriptModule',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Module declarations
  highlight(0, 'purescriptModuleKeyword',     { link = "Keyword" })  -- module keyword
  highlight(0, 'purescriptModuleName',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'purescriptModuleParams',      { fg = colors.white,      bg = 'NONE'            })  -- Module export lists

  -- Imports
  highlight(0, 'purescriptImport',            { fg = colors.blue,       bg = 'NONE'            })  -- Import statements
  highlight(0, 'purescriptImportKeyword',     { link = "Keyword" })  -- import, foreign, qualified
  highlight(0, 'purescriptImportAs',          { fg = colors.blue,       bg = 'NONE'            })  -- as clause in imports
  highlight(0, 'purescriptImportHiding',      { fg = colors.blue,       bg = 'NONE'            })  -- hiding clause
  highlight(0, 'purescriptAsKeyword',         { link = "Keyword" })  -- as keyword
  highlight(0, 'purescriptHidingKeyword',     { link = "Keyword" })  -- hiding keyword
  highlight(0, 'purescriptQualified',         { fg = colors.blue,       bg = 'NONE'            })  -- qualified keyword

  -- Type Definitions
  highlight(0, 'purescriptStructure',         { fg = colors.blue,       bg = 'NONE'            })  -- data, newtype, type, kind, derive, instance
  highlight(0, 'purescriptData',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Data type declarations
  highlight(0, 'purescriptDataStart',         { fg = colors.blue,       bg = 'NONE'            })  -- data keyword
  highlight(0, 'purescriptNewtype',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Newtype declarations
  highlight(0, 'purescriptNewtypeStart',      { fg = colors.blue,       bg = 'NONE'            })  -- newtype keyword
  highlight(0, 'purescriptTypeAlias',         { link = "Type" })  -- Type alias declarations
  highlight(0, 'purescriptTypeAliasStart',    { link = "Type" })  -- type keyword
  highlight(0, 'purescriptForeignData',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Foreign data imports
  highlight(0, 'purescriptForeign',           { fg = colors.blue,       bg = 'NONE'            })  -- foreign keyword
  highlight(0, 'purescriptKind',              { fg = colors.blue,       bg = 'NONE'            })  -- kind keyword
  highlight(0, 'purescriptDerive',            { fg = colors.blue,       bg = 'NONE'            })  -- derive keyword
  highlight(0, 'purescriptInstance',          { fg = colors.blue,       bg = 'NONE'            })  -- instance keyword

  -- Types
  highlight(0, 'purescriptType',              { link = "Type" })  -- Type names (capitalized)
  highlight(0, 'purescriptTypeVar',           { link = "Type" })  -- Type variables (lowercase)
  highlight(0, 'purescriptTypeExport',        { link = "Type" })  -- Exported type constructors
  highlight(0, 'purescriptTypeName',          { link = "Type" })  -- Type name in definition
  highlight(0, 'purescriptTypeAnnotation',    { link = "Type" })  -- Type annotations
  highlight(0, 'purescriptConstraint',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Type class constraints
  highlight(0, 'purescriptKindSig',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Kind signatures
  highlight(0, 'purescriptRow',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Row types

  -- Built-in Types
  highlight(0, 'purescriptInt',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Int type
  highlight(0, 'purescriptNumber',            { link = "Number" })  -- Number type
  highlight(0, 'purescriptStringType',        { link = "String" })  -- String type
  highlight(0, 'purescriptBooleanType',       { link = "Boolean" })  -- Boolean type
  highlight(0, 'purescriptCharType',          { link = "Type" })  -- Char type
  highlight(0, 'purescriptArray',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Array type
  highlight(0, 'purescriptMaybe',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Maybe type
  highlight(0, 'purescriptEither',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Either type
  highlight(0, 'purescriptEffect',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Effect type
  highlight(0, 'purescriptAff',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Aff type
  highlight(0, 'purescriptUnit',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Unit type

  -- Constructors
  highlight(0, 'purescriptConstructor',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Data constructors
  highlight(0, 'purescriptConstructorDecl',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructor declarations
  highlight(0, 'purescriptJust',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Just constructor
  highlight(0, 'purescriptNothing',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Nothing constructor
  highlight(0, 'purescriptLeft',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Left constructor
  highlight(0, 'purescriptRight',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Right constructor
  highlight(0, 'purescriptTrue',              { fg = colors.turquoise,  bg = 'NONE'            })  -- true
  highlight(0, 'purescriptFalse',             { fg = colors.turquoise,  bg = 'NONE'            })  -- false

  -- Type Classes
  highlight(0, 'purescriptClass',             { fg = colors.blue,       bg = 'NONE'            })  -- class keyword
  highlight(0, 'purescriptClassName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'purescriptClassDecl',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Class declaration regions
  highlight(0, 'purescriptClassConstraint',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Class constraints

  -- Functions
  highlight(0, 'purescriptFunction',          { link = "Function" })  -- Function names
  highlight(0, 'purescriptFunctionDecl',      { link = "Function" })  -- Function declarations
  highlight(0, 'purescriptFunctionDeclStart', { link = "Function" })  -- Function declaration start
  highlight(0, 'purescriptFunctionCall',      { link = "Function" })  -- Function calls
  highlight(0, 'purescriptLambda',            { fg = colors.blue,       bg = 'NONE'            })  -- Lambda: \x -> ...

  -- Identifiers and Variables
  highlight(0, 'purescriptIdentifier',        { fg = colors.white,      bg = 'NONE'            })  -- Variable/function names
  highlight(0, 'purescriptVariable',          { link = "Variable" })  -- Variables
  highlight(0, 'purescriptParameter',         { fg = colors.white,      bg = 'NONE'            })  -- Function parameters
  highlight(0, 'purescriptWildcard',          { fg = colors.white,      bg = 'NONE'            })  -- _ wildcard

  -- Operators
  highlight(0, 'purescriptOperator',          { link = "Operator" })  -- Symbolic operators
  highlight(0, 'purescriptOperatorType',      { link = "Operator" })  -- Type operators (::, ∷)
  highlight(0, 'purescriptOperatorTypeSig',   { link = "Operator" })  -- Type annotation operators
  highlight(0, 'purescriptOperatorFunction',  { link = "Operator" })  -- Function operators (->. <-, →, ←)
  highlight(0, 'purescriptArrow',             { fg = colors.blue,       bg = 'NONE'            })  -- -> (function arrow)
  highlight(0, 'purescriptLeftArrow',         { fg = colors.blue,       bg = 'NONE'            })  -- <- (bind)
  highlight(0, 'purescriptFatArrow',          { fg = colors.blue,       bg = 'NONE'            })  -- => (constraint arrow)
  highlight(0, 'purescriptPipe',              { fg = colors.white,      bg = 'NONE'            })  -- | (alternatives)
  highlight(0, 'purescriptEquals',            { fg = colors.white,      bg = 'NONE'            })  -- = (definition)
  highlight(0, 'purescriptDoubleColon',       { fg = colors.white,      bg = 'NONE'            })  -- :: (type annotation)
  highlight(0, 'purescriptDot',               { fg = colors.white,      bg = 'NONE'            })  -- . (composition, record access)
  highlight(0, 'purescriptBacktick',          { fg = colors.white,      bg = 'NONE'            })  -- Backtick-quoted operators

  -- Infix Declarations
  highlight(0, 'purescriptInfix',             { fg = colors.blue,       bg = 'NONE'            })  -- Infix operator declarations
  highlight(0, 'purescriptInfixKeyword',      { link = "Keyword" })  -- infix, infixl, infixr

  -- Literals
  highlight(0, 'purescriptBoolean',           { link = "Boolean" })  -- true, false
  highlight(0, 'purescriptNumberLiteral',     { link = "Number" })  -- Integer literals
  highlight(0, 'purescriptFloat',             { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'purescriptHexNumber',         { link = "Number" })  -- Hexadecimal numbers
  highlight(0, 'purescriptOctalNumber',       { link = "Number" })  -- Octal numbers

  -- Strings and Characters
  highlight(0, 'purescriptString',            { link = "String" })  -- Double-quoted strings
  highlight(0, 'purescriptMultilineString',   { link = "String" })  -- Triple-quoted strings
  highlight(0, 'purescriptChar',              { fg = colors.redLight,   bg = 'NONE'            })  -- Character literals
  highlight(0, 'purescriptStringEscape',      { link = "String" })  -- Escape sequences
  highlight(0, 'purescriptCharEscape',        { fg = colors.pink,       bg = 'NONE'            })  -- Character escapes

  -- Comments
  highlight(0, 'purescriptLineComment',       { link = "Comment" })  -- Single-line comments: --
  highlight(0, 'purescriptBlockComment',      { link = "Comment" })  -- Block comments: {- -}
  highlight(0, 'purescriptDocComment',        { link = "Comment" })  -- Documentation comments: -- |
  highlight(0, 'purescriptTodo',              { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Punctuation and Delimiters
  highlight(0, 'purescriptDelimiter',         { link = "Delimiter" })  -- Punctuation: , ; ( ) [ ] { }
  highlight(0, 'purescriptParen',             { fg = colors.white,      bg = 'NONE'            })  -- Parentheses
  highlight(0, 'purescriptBracket',           { fg = colors.white,      bg = 'NONE'            })  -- Square brackets
  highlight(0, 'purescriptBrace',             { fg = colors.white,      bg = 'NONE'            })  -- Curly braces
  highlight(0, 'purescriptComma',             { fg = colors.white,      bg = 'NONE'            })  -- Commas
  highlight(0, 'purescriptSemicolon',         { fg = colors.white,      bg = 'NONE'            })  -- Semicolons

  -- Records
  highlight(0, 'purescriptRecord',            { fg = colors.white,      bg = 'NONE'            })  -- Record literals
  highlight(0, 'purescriptRecordField',       { fg = colors.blue,       bg = 'NONE'            })  -- Record field names
  highlight(0, 'purescriptRecordAccess',      { fg = colors.blue,       bg = 'NONE'            })  -- .field access
  highlight(0, 'purescriptRecordWildcard',    { fg = colors.white,      bg = 'NONE'            })  -- { .. } record wildcard
  highlight(0, 'purescriptRecordUpdate',      { fg = colors.blue,       bg = 'NONE'            })  -- Record update syntax

  -- Do-notation
  highlight(0, 'purescriptDo',                { fg = colors.blue,       bg = 'NONE'            })  -- do keyword
  highlight(0, 'purescriptAdo',               { fg = colors.blue,       bg = 'NONE'            })  -- ado keyword (applicative do)
  highlight(0, 'purescriptBind',              { fg = colors.white,      bg = 'NONE'            })  -- <- in do-notation

  -- Pattern Matching
  highlight(0, 'purescriptCase',              { fg = colors.blue,       bg = 'NONE'            })  -- case keyword
  highlight(0, 'purescriptOf',                { fg = colors.blue,       bg = 'NONE'            })  -- of keyword
  highlight(0, 'purescriptPattern',           { fg = colors.white,      bg = 'NONE'            })  -- Pattern expressions
  highlight(0, 'purescriptGuard',             { fg = colors.white,      bg = 'NONE'            })  -- Guard expressions: |

  -- FFI (Foreign Function Interface)
  highlight(0, 'purescriptFFI',               { fg = colors.orange,     bg = 'NONE'            })  -- FFI declarations
  highlight(0, 'purescriptForeignImport',     { fg = colors.blue,       bg = 'NONE'            })  -- foreign import

  -- Prelude and Common Functions
  highlight(0, 'purescriptPrelude',           { fg = colors.orange,     bg = 'NONE'            })  -- Prelude functions
  highlight(0, 'purescriptMap',               { fg = colors.orange,     bg = 'NONE'            })  -- map
  highlight(0, 'purescriptBind',              { fg = colors.orange,     bg = 'NONE'            })  -- bind (>>=)
  highlight(0, 'purescriptPure',              { fg = colors.orange,     bg = 'NONE'            })  -- pure
  highlight(0, 'purescriptApply',             { fg = colors.orange,     bg = 'NONE'            })  -- apply (<*>)
  highlight(0, 'purescriptShow',              { fg = colors.orange,     bg = 'NONE'            })  -- show
  highlight(0, 'purescriptEq',                { fg = colors.orange,     bg = 'NONE'            })  -- eq (==)
  highlight(0, 'purescriptCompare',           { fg = colors.orange,     bg = 'NONE'            })  -- compare

  -- Debug and Unsafe
  highlight(0, 'purescriptDebug',             { fg = colors.orange,     bg = 'NONE', bold = true })  -- Debug functions
  highlight(0, 'purescriptUnsafe',            { fg = colors.orange,     bg = 'NONE', bold = true })  -- Unsafe operations
  highlight(0, 'purescriptTrace',             { fg = colors.orange,     bg = 'NONE', bold = true })  -- trace, traceShow
  highlight(0, 'purescriptSpy',               { fg = colors.orange,     bg = 'NONE', bold = true })  -- spy


  -------------------------------------------------------------------------
  -- Treesitter PureScript Captures
  -------------------------------------------------------------------------

  -- Literals
  highlight(0, '@number.purescript',               { link = "Number" })  -- Numbers
  highlight(0, '@number.float.purescript',         { link = "Number" })  -- Float numbers
  highlight(0, '@boolean.purescript',              { link = "Boolean" })  -- Booleans
  highlight(0, '@character.purescript',            { fg = colors.redLight,   bg = 'NONE'            })  -- Characters
  highlight(0, '@character.special.purescript',    { fg = colors.pink,       bg = 'NONE'            })  -- Special characters/escapes
  highlight(0, '@string.purescript',               { link = "String" })  -- Strings

  -- Comments
  highlight(0, '@comment.purescript',              { link = "Comment" })  -- Comments
  highlight(0, '@spell.purescript',                { link = '@comment.purescript'                        })  -- Spell check

  -- Keywords
  highlight(0, '@keyword.purescript',              { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.conditional.purescript',  { link = "Conditional" })  -- if, then, else
  highlight(0, '@keyword.import.purescript',       { link = "Keyword" })  -- import, module
  highlight(0, '@keyword.type.purescript',         { link = "Keyword" })  -- type, data, newtype, class
  highlight(0, '@keyword.modifier.purescript',     { link = "Keyword" })  -- forall, derive, instance

  -- Types and Constructors
  highlight(0, '@type.purescript',                 { link = "Type" })  -- Types
  highlight(0, '@constructor.purescript',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Data constructors

  -- Modules
  highlight(0, '@module.purescript',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names

  -- Functions and Variables
  highlight(0, '@function.purescript',             { link = "Function" })  -- Functions
  highlight(0, '@variable.purescript',             { link = "Variable" })  -- Variables
  highlight(0, '@variable.member.purescript',      { link = "Variable" })  -- Record fields

  -- Operators
  highlight(0, '@operator.purescript',             { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.purescript',  { fg = colors.white,      bg = 'NONE'            })  -- Brackets
  highlight(0, '@punctuation.delimiter.purescript', { link = "Delimiter" })  -- Delimiters


  -------------------------------------------------------------------------
  -- LSP Semantic Tokens
  -------------------------------------------------------------------------

  highlight(0, '@lsp.type.type.purescript',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@lsp.type.class.purescript',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Type classes
  highlight(0, '@lsp.type.enum.purescript',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum types
  highlight(0, '@lsp.type.enumMember.purescript',  { fg = colors.turquoise,  bg = 'NONE'            })  -- Data constructors
  highlight(0, '@lsp.type.function.purescript',    { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@lsp.type.method.purescript',      { fg = colors.orange,     bg = 'NONE'            })  -- Methods
  highlight(0, '@lsp.type.property.purescript',    { fg = colors.blue,       bg = 'NONE'            })  -- Record fields
  highlight(0, '@lsp.type.variable.purescript',    { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.purescript',   { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.typeParameter.purescript', { fg = colors.purple,   bg = 'NONE'            })  -- Type variables
  highlight(0, '@lsp.type.namespace.purescript',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Modules
  highlight(0, '@lsp.type.string.purescript',      { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.purescript',      { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.purescript',     { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.purescript',    { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.comment.purescript',     { link = "Comment" })  -- Comments

  highlight(0, '@lsp.mod.declaration.purescript',  { fg = colors.orange,     bg = 'NONE'            })  -- Declarations
  highlight(0, '@lsp.mod.definition.purescript',   { fg = colors.orange,     bg = 'NONE'            })  -- Definitions
  highlight(0, '@lsp.mod.readonly.purescript',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Immutable values
  highlight(0, '@lsp.mod.defaultLibrary.purescript', { fg = colors.turquoise, bg = 'NONE'           })  -- Prelude

end

return purescript
