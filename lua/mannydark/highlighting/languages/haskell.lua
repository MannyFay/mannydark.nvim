-------------------------------------------------------------------------------
-- Haskell
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local haskell   = {}


-------------------------------------------------------------------------------
-- Settings

haskell.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups (haskell-vim / vim runtime)
  ---------------------------------------------------------------------------

  -- Module System
  highlight(0, 'hsModule',              { fg = colors.blue,       bg = 'NONE' })  -- module keyword
  highlight(0, 'hsModuleName',          { fg = colors.turquoise,  bg = 'NONE' })  -- Module names
  highlight(0, 'hsModuleParams',        { fg = colors.white,      bg = 'NONE' })  -- Module export list
  highlight(0, 'haskellModule',         { fg = colors.turquoise,  bg = 'NONE' })  -- Module declarations

  -- Imports
  highlight(0, 'hsImport',              { fg = colors.blue,       bg = 'NONE' })  -- import keyword
  highlight(0, 'hsImportMod',           { fg = colors.blue,       bg = 'NONE' })  -- Import modifiers
  highlight(0, 'hsImportModuleName',    { fg = colors.turquoise,  bg = 'NONE' })  -- Imported module names
  highlight(0, 'hsImportList',          { fg = colors.white,      bg = 'NONE' })  -- Import list
  highlight(0, 'haskellImport',         { fg = colors.turquoise,  bg = 'NONE' })  -- Import declarations
  highlight(0, 'haskellImportKeywords', { link = "Keyword" })  -- import, qualified, as, hiding

  -- Foreign Imports
  highlight(0, 'haskellForeignImport',  { fg = colors.turquoise,  bg = 'NONE' })  -- Foreign import declarations
  highlight(0, 'haskellForeignKeywords', { link = "Keyword" })  -- foreign, import, export

  -- Keywords - Control Flow
  highlight(0, 'hsConditional',         { link = "Conditional" })  -- if, then, else
  highlight(0, 'hsStatement',           { fg = colors.blue,       bg = 'NONE' })  -- do, case, of, in, mdo, rec
  highlight(0, 'haskellConditional',    { link = "Conditional" })  -- if, then, else
  highlight(0, 'haskellKeyword',        { link = "Keyword" })  -- Generic keywords

  -- Keywords - Let/Where
  highlight(0, 'haskellLet',            { fg = colors.blue,       bg = 'NONE' })  -- let keyword
  highlight(0, 'haskellWhere',          { fg = colors.blue,       bg = 'NONE' })  -- where keyword

  -- Keywords - Declarations
  highlight(0, 'hsStructure',           { fg = colors.blue,       bg = 'NONE' })  -- data, newtype, type, class, instance
  highlight(0, 'hsTypedef',             { link = "Type" })  -- type keyword
  highlight(0, 'hsNewtypedef',          { fg = colors.blue,       bg = 'NONE' })  -- newtype keyword
  highlight(0, 'hsTypeFam',             { link = "Type" })  -- type family keyword
  highlight(0, 'haskellDeclKeyword',    { link = "Keyword" })  -- data, newtype, type
  highlight(0, 'haskellDecl',           { fg = colors.turquoise,  bg = 'NONE' })  -- Declaration regions

  -- Keywords - Deriving
  highlight(0, 'haskellDeriveKeyword',  { link = "Keyword" })  -- deriving keyword
  highlight(0, 'haskellDerive',         { fg = colors.turquoise,  bg = 'NONE' })  -- Derived instances
  highlight(0, 'haskellDefault',        { fg = colors.blue,       bg = 'NONE' })  -- default keyword

  -- Keywords - Infix
  highlight(0, 'hsInfix',               { fg = colors.blue,       bg = 'NONE' })  -- infix, infixl, infixr
  highlight(0, 'haskellInfix',          { fg = colors.blue,       bg = 'NONE' })  -- Infix declarations

  -- Keywords - Static
  highlight(0, 'haskellStatic',         { fg = colors.blue,       bg = 'NONE' })  -- static keyword

  -- Keywords - Forall
  highlight(0, 'haskellForall',         { fg = colors.blue,       bg = 'NONE' })  -- forall, ∀

  -- Keywords - Recursive Do
  highlight(0, 'haskellRecursiveDo',    { fg = colors.blue,       bg = 'NONE' })  -- mdo, rec

  -- Keywords - Arrow Syntax
  highlight(0, 'haskellArrowSyntax',    { fg = colors.blue,       bg = 'NONE' })  -- proc, -<, >-, -<<, >>-

  -- Keywords - Pattern
  highlight(0, 'haskellPatternKeyword', { link = "Keyword" })  -- pattern keyword

  -- Types
  highlight(0, 'hsType',                { link = "Type" })  -- Type names
  highlight(0, 'haskellType',           { link = "Type" })  -- Type names (capitalized)
  highlight(0, 'haskellTypeSig',        { link = "Type" })  -- Type signatures

  -- Type Roles
  highlight(0, 'haskellTypeRoles',      { link = "Type" })  -- type role keyword
  highlight(0, 'haskellTypeRoleBlock',  { link = "Type" })  -- Role annotations

  -- Associated Types
  highlight(0, 'haskellAssocType',      { link = "Type" })  -- Associated type definitions

  -- Built-in Types
  highlight(0, 'hsMaybe',               { fg = colors.turquoise,  bg = 'NONE' })  -- Maybe type
  highlight(0, 'hsExitCode',            { fg = colors.turquoise,  bg = 'NONE' })  -- ExitCode type
  highlight(0, 'hsOrdering',            { fg = colors.turquoise,  bg = 'NONE' })  -- Ordering type

  -- Constructors
  highlight(0, 'hsConstructor',         { fg = colors.turquoise,  bg = 'NONE' })  -- Data constructors
  highlight(0, 'hsConSym',              { fg = colors.turquoise,  bg = 'NONE' })  -- Symbolic constructors (:)
  highlight(0, 'hsEnumConst',           { fg = colors.turquoise,  bg = 'NONE' })  -- Enum constructors

  -- Functions
  highlight(0, 'hsFunction',            { link = "Function" })  -- Function names
  highlight(0, 'hsVarSym',              { fg = colors.white,      bg = 'NONE' })  -- Symbolic variables (operators)

  -- Variables
  highlight(0, 'hsIdentifier',          { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, 'haskellIdentifier',     { fg = colors.white,      bg = 'NONE' })  -- Identifiers

  -- Record Fields
  highlight(0, 'hsLabel',               { fg = colors.orange,     bg = 'NONE' })  -- Record field labels
  highlight(0, 'haskellRecordField',    { fg = colors.orange,     bg = 'NONE' })  -- Record fields

  -- Booleans
  highlight(0, 'hsBoolean',             { link = "Boolean" })  -- True, False
  highlight(0, 'haskellBoolean',        { link = "Boolean" })  -- True, False

  -- Numbers
  highlight(0, 'hsNumber',              { link = "Number" })  -- Integer literals
  highlight(0, 'hsFloat',               { fg = colors.greenLight, bg = 'NONE' })  -- Float literals
  highlight(0, 'haskellNumber',         { link = "Number" })  -- Numbers
  highlight(0, 'haskellFloat',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Strings
  highlight(0, 'hsString',              { link = "String" })  -- String literals
  highlight(0, 'haskellString',         { link = "String" })  -- Strings

  -- Characters
  highlight(0, 'hsCharacter',           { fg = colors.redLight,   bg = 'NONE' })  -- Character literals
  highlight(0, 'haskellChar',           { fg = colors.redLight,   bg = 'NONE' })  -- Characters

  -- Special Characters (escape sequences)
  highlight(0, 'hsSpecialChar',         { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, etc.
  highlight(0, 'hsSpecialCharError',    { fg = colors.red,        bg = 'NONE' })  -- Invalid escape

  -- Operators
  highlight(0, 'hsOperator',            { link = "Operator" })  -- Operators
  highlight(0, 'haskellOperators',      { link = "Operator" })  -- Operators

  -- Delimiters/Separators
  highlight(0, 'hsDelimiter',           { link = "Delimiter" })  -- Delimiters
  highlight(0, 'haskellSeparator',      { fg = colors.white,      bg = 'NONE' })  -- Separators (,)
  highlight(0, 'haskellDelimiter',      { link = "Delimiter" })  -- Delimiters

  -- Brackets/Parens
  highlight(0, 'haskellParens',         { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'haskellBrackets',       { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'haskellBlock',          { fg = colors.white,      bg = 'NONE' })  -- { }

  -- Backticks
  highlight(0, 'haskellBacktick',       { fg = colors.white,      bg = 'NONE' })  -- `function`

  -- Bottom
  highlight(0, 'haskellBottom',         { fg = colors.red,        bg = 'NONE' })  -- undefined, error

  -- Comments
  highlight(0, 'hsComment',             { link = "Comment" })  -- General comments
  highlight(0, 'hsLineComment',         { link = "Comment" })  -- -- comments
  highlight(0, 'hsBlockComment',        { link = "Comment" })  -- {- -} comments
  highlight(0, 'haskellLineComment',    { link = "Comment" })  -- Line comments
  highlight(0, 'haskellBlockComment',   { link = "Comment" })  -- Block comments
  highlight(0, 'hsTodo',                { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME
  highlight(0, 'haskellTodo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME

  -- Pragmas
  highlight(0, 'hsPragma',              { fg = colors.pink,       bg = 'NONE' })  -- {-# LANGUAGE ... #-}
  highlight(0, 'haskellPragma',         { fg = colors.pink,       bg = 'NONE' })  -- Pragmas

  -- Preprocessor
  highlight(0, 'haskellPreProc',        { fg = colors.pink,       bg = 'NONE' })  -- CPP preprocessor

  -- Shebang
  highlight(0, 'haskellShebang',        { fg = colors.green,      bg = 'NONE' })  -- #!/usr/bin/env runhaskell

  -- Liquid Haskell
  highlight(0, 'haskellLiquid',         { fg = colors.pink,       bg = 'NONE' })  -- Liquid Haskell annotations

  -- Template Haskell
  highlight(0, 'haskellTH',             { fg = colors.pink,       bg = 'NONE' })  -- Template Haskell
  highlight(0, 'haskellTHBlock',        { fg = colors.pink,       bg = 'NONE' })  -- [| |] TH blocks
  highlight(0, 'haskellTHDoubleBlock',  { fg = colors.pink,       bg = 'NONE' })  -- [|| ||] typed TH

  -- QuasiQuotes
  highlight(0, 'haskellQuote',          { fg = colors.pink,       bg = 'NONE' })  -- Quote syntax
  highlight(0, 'haskellQuoted',         { fg = colors.pink,       bg = 'NONE' })  -- Quoted expressions
  highlight(0, 'haskellQuotedType',     { link = "Type" })  -- Quoted types
  highlight(0, 'haskellQuasiQuote',     { fg = colors.pink,       bg = 'NONE' })  -- [quoter| ... |]
  highlight(0, 'haskellQuasiQuoted',    { fg = colors.redLight,   bg = 'NONE' })  -- QuasiQuote content

  -- Backpack
  highlight(0, 'haskellBackpackStructure',  { fg = colors.blue,      bg = 'NONE' })  -- unit, signature
  highlight(0, 'haskellBackpackDependency', { fg = colors.turquoise, bg = 'NONE' })  -- Backpack dependencies

  -- Errors
  highlight(0, 'hsError',               { fg = colors.red,        bg = 'NONE' })  -- Errors
  highlight(0, 'hsDebug',               { fg = colors.red,        bg = 'NONE' })  -- Debug


  ---------------------------------------------------------------------------
  -- Additional Legacy Groups (neovimhaskell/haskell-vim)
  ---------------------------------------------------------------------------

  -- More Keywords
  highlight(0, 'haskellImportAs',       { fg = colors.blue,       bg = 'NONE' })  -- as in imports
  highlight(0, 'haskellImportHiding',   { fg = colors.blue,       bg = 'NONE' })  -- hiding in imports
  highlight(0, 'haskellImportQualified', { fg = colors.blue,      bg = 'NONE' })  -- qualified

  -- Pattern Guards
  highlight(0, 'haskellPatternGuard',   { fg = colors.white,      bg = 'NONE' })  -- Pattern guards

  -- Type Applications
  highlight(0, 'haskellTypeApp',        { link = "Type" })  -- @Type applications

  -- Promoted Types (DataKinds)
  highlight(0, 'haskellPromoted',       { fg = colors.turquoise,  bg = 'NONE' })  -- 'Constructor, '[]

  -- Kind Signatures
  highlight(0, 'haskellKind',           { fg = colors.turquoise,  bg = 'NONE' })  -- Kind annotations

  -- Constraint Kinds
  highlight(0, 'haskellConstraint',     { fg = colors.turquoise,  bg = 'NONE' })  -- Constraints

  -- Type Operators
  highlight(0, 'haskellTypeOperator',   { link = "Operator" })  -- Type-level operators

  -- Pattern Synonyms
  highlight(0, 'haskellPatternSynonym', { fg = colors.turquoise,  bg = 'NONE' })  -- pattern keyword
  highlight(0, 'haskellPatternSynonymDef', { fg = colors.turquoise, bg = 'NONE' })  -- Pattern synonym defs

  -- Standalone Deriving
  highlight(0, 'haskellDeriving',       { fg = colors.blue,       bg = 'NONE' })  -- deriving
  highlight(0, 'haskellDerivingStandalone', { fg = colors.blue,   bg = 'NONE' })  -- Standalone deriving
  highlight(0, 'haskellDerivingVia',    { fg = colors.blue,       bg = 'NONE' })  -- deriving via
  highlight(0, 'haskellDerivingStrategies', { fg = colors.blue,   bg = 'NONE' })  -- stock, anyclass, newtype

  -- GADTs
  highlight(0, 'haskellGADT',           { fg = colors.turquoise,  bg = 'NONE' })  -- GADT constructors

  -- Linear Types
  highlight(0, 'haskellLinearArrow',    { fg = colors.white,      bg = 'NONE' })  -- %1 ->

  -- Qualified Do
  highlight(0, 'haskellQualifiedDo',    { fg = colors.blue,       bg = 'NONE' })  -- Module.do


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.haskell)
  ---------------------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.haskell',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.haskell',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.haskell',       { link = "Variable" })  -- Record fields

  -- Types
  highlight(0, '@type.haskell',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.haskell',          { link = "Type" })  -- Built-in types
  highlight(0, '@type.definition.haskell',       { link = "Type" })  -- Type definitions
  highlight(0, '@type.qualifier.haskell',        { link = "Type" })  -- Module qualifiers

  -- Constructors
  highlight(0, '@constructor.haskell',           { fg = colors.turquoise,  bg = 'NONE' })  -- Data constructors

  -- Functions
  highlight(0, '@function.haskell',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.haskell',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.haskell',      { link = "Function" })  -- Built-in functions

  -- Keywords
  highlight(0, '@keyword.haskell',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.haskell',      { link = "Keyword" })  -- where, let
  highlight(0, '@keyword.operator.haskell',      { link = "Operator" })  -- Operator keywords
  highlight(0, '@keyword.return.haskell',        { link = "Keyword" })  -- return (in do)
  highlight(0, '@keyword.repeat.haskell',        { link = "Keyword" })  -- Repetition (rarely used)
  highlight(0, '@keyword.conditional.haskell',   { link = "Conditional" })  -- if, then, else, case, of
  highlight(0, '@keyword.import.haskell',        { link = "Keyword" })  -- import, qualified, as, hiding
  highlight(0, '@keyword.exception.haskell',     { link = "Keyword" })  -- catch, throw
  highlight(0, '@keyword.directive.haskell',     { link = "Keyword" })  -- Pragmas

  -- Modules
  highlight(0, '@module.haskell',                { fg = colors.turquoise,  bg = 'NONE' })  -- Module names
  highlight(0, '@module.builtin.haskell',        { fg = colors.turquoise,  bg = 'NONE' })  -- Prelude, etc.

  -- Strings
  highlight(0, '@string.haskell',                { link = "String" })  -- String literals
  highlight(0, '@string.escape.haskell',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.special.haskell',        { link = "String" })  -- Special strings
  highlight(0, '@string.special.symbol.haskell', { link = "String" })  -- Symbolic names

  -- Characters
  highlight(0, '@character.haskell',             { fg = colors.redLight,   bg = 'NONE' })  -- Character literals
  highlight(0, '@character.special.haskell',     { fg = colors.pink,       bg = 'NONE' })  -- Special chars

  -- Numbers
  highlight(0, '@number.haskell',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.haskell',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.haskell',               { link = "Boolean" })  -- True, False

  -- Operators
  highlight(0, '@operator.haskell',              { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.haskell',   { fg = colors.white,      bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.haskell', { link = "Delimiter" })  -- , ; ::
  highlight(0, '@punctuation.special.haskell',   { fg = colors.pink,       bg = 'NONE' })  -- @ (as-pattern), ' (promoted)

  -- Comments
  highlight(0, '@comment.haskell',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.haskell', { link = "Comment" })  -- Haddock comments

  -- Labels
  highlight(0, '@label.haskell',                 { fg = colors.pink,       bg = 'NONE' })  -- OverloadedLabels #label

  -- Debug
  highlight(0, '@keyword.debug.haskell',         { link = "Keyword" })  -- trace, undefined


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.haskell)
  -- Based on haskell-language-server HsSemanticTokenType
  ---------------------------------------------------------------------------

  -- Standard LSP Types
  highlight(0, '@lsp.type.variable.haskell',     { link = "Variable" })  -- TVariable
  highlight(0, '@lsp.type.function.haskell',     { fg = colors.orange,     bg = 'NONE' })  -- TFunction
  highlight(0, '@lsp.type.parameter.haskell',    { fg = colors.white,      bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.type.haskell',         { fg = colors.turquoise,  bg = 'NONE' })  -- TTypeConstructor
  highlight(0, '@lsp.type.class.haskell',        { fg = colors.turquoise,  bg = 'NONE' })  -- TClass
  highlight(0, '@lsp.type.enum.haskell',         { fg = colors.turquoise,  bg = 'NONE' })  -- Data types
  highlight(0, '@lsp.type.enumMember.haskell',   { fg = colors.turquoise,  bg = 'NONE' })  -- TDataConstructor
  highlight(0, '@lsp.type.property.haskell',     { fg = colors.orange,     bg = 'NONE' })  -- TRecordField
  highlight(0, '@lsp.type.method.haskell',       { fg = colors.orange,     bg = 'NONE' })  -- TClassMethod
  highlight(0, '@lsp.type.namespace.haskell',    { fg = colors.turquoise,  bg = 'NONE' })  -- TModule
  highlight(0, '@lsp.type.typeParameter.haskell', { fg = colors.purple,    bg = 'NONE' })  -- TTypeVariable
  highlight(0, '@lsp.type.operator.haskell',     { link = "Operator" })  -- TOperator
  highlight(0, '@lsp.type.keyword.haskell',      { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.string.haskell',       { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.haskell',       { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.comment.haskell',      { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.decorator.haskell',    { fg = colors.pink,       bg = 'NONE' })  -- Pragmas
  highlight(0, '@lsp.type.macro.haskell',        { fg = colors.pink,       bg = 'NONE' })  -- Template Haskell

  -- HLS-Specific Token Types (mapped to standard LSP tokens)
  -- TVariable -> variable
  -- TFunction -> function
  -- TDataConstructor -> enumMember
  -- TTypeVariable -> typeParameter
  -- TClassMethod -> method
  -- TPatternSynonym -> enumMember (pattern synonyms act like constructors)
  -- TTypeConstructor -> type
  -- TClass -> class
  -- TTypeSynonym -> type
  -- TTypeFamily -> type
  -- TRecordField -> property
  -- TOperator -> operator
  -- TModule -> namespace

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.haskell',    { link = "Variable" })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.haskell', { fg = colors.orange,    bg = 'NONE' })  -- Function defs
  highlight(0, '@lsp.typemod.class.declaration.haskell',    { fg = colors.turquoise, bg = 'NONE' })  -- Class defs
  highlight(0, '@lsp.typemod.type.declaration.haskell',     { fg = colors.turquoise, bg = 'NONE' })  -- Type defs
  highlight(0, '@lsp.typemod.function.defaultLibrary.haskell', { fg = colors.orange, bg = 'NONE' })  -- Prelude funcs
  highlight(0, '@lsp.typemod.variable.global.haskell',      { link = "Variable" })  -- Top-level bindings
  highlight(0, '@lsp.typemod.variable.local.haskell',       { link = "Variable" })  -- Local bindings


  ---------------------------------------------------------------------------
  -- Haddock Documentation
  ---------------------------------------------------------------------------

  highlight(0, 'haddockHeading',        { fg = colors.red,        bg = 'NONE', bold = true })  -- = Section headers
  highlight(0, 'haddockChunk',          { fg = colors.red,        bg = 'NONE' })  -- Documentation chunks
  highlight(0, 'haddockModuleHeading',  { fg = colors.red,        bg = 'NONE', bold = true })  -- Module headers
  highlight(0, 'haddockKeep',           { fg = colors.red,        bg = 'NONE' })  -- @since annotations
  highlight(0, 'haddockLink',           { fg = colors.blue,       bg = 'NONE', underline = true })  -- Links
  highlight(0, 'haddockAnchor',         { fg = colors.pink,       bg = 'NONE' })  -- #anchor references
  highlight(0, 'haddockEmphasis',       { fg = colors.red,        bg = 'NONE', italic = true })  -- /emphasis/
  highlight(0, 'haddockBold',           { fg = colors.red,        bg = 'NONE', bold = true })  -- __bold__
  highlight(0, 'haddockMonospace',      { fg = colors.redLight,   bg = 'NONE' })  -- @code@
  highlight(0, 'haddockCodeBlock',      { fg = colors.redLight,   bg = 'NONE' })  -- @@ code blocks @@
  highlight(0, 'haddockURL',            { fg = colors.blue,       bg = 'NONE', underline = true })  -- <url>
  highlight(0, 'haddockMath',           { fg = colors.greenLight, bg = 'NONE' })  -- \(math\)
  highlight(0, 'haddockExample',        { fg = colors.redLight,   bg = 'NONE' })  -- >>> examples
  highlight(0, 'haddockProperty',       { fg = colors.pink,       bg = 'NONE' })  -- prop> properties


  ---------------------------------------------------------------------------
  -- Literate Haskell
  ---------------------------------------------------------------------------

  highlight(0, 'hsLiterateComment',     { link = "Comment" })  -- Text in .lhs files
  highlight(0, 'lhsComment',            { link = "Comment" })  -- Literate comments
  highlight(0, 'lhsCodeBlock',          { fg = colors.white,      bg = 'NONE' })  -- Code blocks in .lhs
end

return haskell
