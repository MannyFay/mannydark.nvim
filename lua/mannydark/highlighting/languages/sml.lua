-------------------------------------------------------------------------------
-- Standard ML (SML)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local sml       = {}


-------------------------------------------------------------------------------
-- Settings

sml.setupHighlighting = function()


  ---------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups (vim runtime sml.vim)
  ---------------------------------------------------------------------------

  -- Keywords - Control Flow
  highlight(0, 'smlCond',              { fg = colors.blue,       bg = 'NONE' })  -- if, then, else, case, of
  highlight(0, 'smlRepeat',            { fg = colors.blue,       bg = 'NONE' })  -- while, do
  highlight(0, 'smlConditional',       { fg = colors.blue,       bg = 'NONE' })  -- if, then, else
  highlight(0, 'smlStatement',         { fg = colors.blue,       bg = 'NONE' })  -- Generic statements

  -- Keywords - Exception Handling
  highlight(0, 'smlRaise',             { fg = colors.blue,       bg = 'NONE' })  -- raise
  highlight(0, 'smlHandle',            { fg = colors.blue,       bg = 'NONE' })  -- handle
  highlight(0, 'smlException',         { fg = colors.blue,       bg = 'NONE' })  -- exception keyword

  -- Keywords - Boolean Operators
  highlight(0, 'smlBoolOp',            { fg = colors.blue,       bg = 'NONE' })  -- andalso, orelse

  -- Keywords - Declarations
  highlight(0, 'smlDecl',              { fg = colors.blue,       bg = 'NONE' })  -- and, datatype, exception, fun, infix, etc.
  highlight(0, 'smlKeyword',           { fg = colors.blue,       bg = 'NONE' })  -- General keywords
  highlight(0, 'smlKeyChar',           { fg = colors.white,      bg = 'NONE' })  -- => | = etc.

  -- Keywords - val/fun
  highlight(0, 'smlVal',               { fg = colors.blue,       bg = 'NONE' })  -- val keyword
  highlight(0, 'smlFun',               { fg = colors.blue,       bg = 'NONE' })  -- fun keyword
  highlight(0, 'smlFunDef',            { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, 'smlFunction',          { fg = colors.orange,     bg = 'NONE' })  -- Function names

  -- Keywords - Blocks/Scope
  highlight(0, 'smlBlock',             { fg = colors.blue,       bg = 'NONE' })  -- local, let, struct, sig, abstype
  highlight(0, 'smlLet',               { fg = colors.blue,       bg = 'NONE' })  -- let keyword
  highlight(0, 'smlIn',                { fg = colors.blue,       bg = 'NONE' })  -- in keyword
  highlight(0, 'smlEnd',               { fg = colors.blue,       bg = 'NONE' })  -- end keyword
  highlight(0, 'smlLocal',             { fg = colors.blue,       bg = 'NONE' })  -- local keyword

  -- Keywords - Expressions
  highlight(0, 'smlExpr',              { fg = colors.blue,       bg = 'NONE' })  -- fn, op
  highlight(0, 'smlFn',                { fg = colors.blue,       bg = 'NONE' })  -- fn (lambda)
  highlight(0, 'smlOp',                { fg = colors.blue,       bg = 'NONE' })  -- op keyword

  -- Keywords - Pattern
  highlight(0, 'smlPat',               { fg = colors.blue,       bg = 'NONE' })  -- as
  highlight(0, 'smlAs',                { fg = colors.blue,       bg = 'NONE' })  -- as keyword
  highlight(0, 'smlWild',              { fg = colors.white,      bg = 'NONE' })  -- _ (wildcard)

  -- Keywords - Infix
  highlight(0, 'smlInfix',             { fg = colors.blue,       bg = 'NONE' })  -- infix, infixr, nonfix

  -- Keywords - Type Declarations
  highlight(0, 'smlType',              { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, 'smlTypedef',           { fg = colors.blue,       bg = 'NONE' })  -- type keyword
  highlight(0, 'smlDatatype',          { fg = colors.blue,       bg = 'NONE' })  -- datatype keyword
  highlight(0, 'smlAbstype',           { fg = colors.blue,       bg = 'NONE' })  -- abstype keyword
  highlight(0, 'smlWithtype',          { fg = colors.blue,       bg = 'NONE' })  -- withtype keyword
  highlight(0, 'smlEqtype',            { fg = colors.blue,       bg = 'NONE' })  -- eqtype keyword

  -- Type Variables
  highlight(0, 'smlTyvar',             { fg = colors.purple,     bg = 'NONE' })  -- 'a, ''a type variables


  ---------------------------------------------------------------------------
  -- Module System
  ---------------------------------------------------------------------------

  -- Structure
  highlight(0, 'smlStructure',         { fg = colors.blue,       bg = 'NONE' })  -- structure keyword
  highlight(0, 'smlStruct',            { fg = colors.blue,       bg = 'NONE' })  -- struct keyword
  highlight(0, 'smlStrid',             { fg = colors.turquoise,  bg = 'NONE' })  -- Structure identifiers
  highlight(0, 'smlStridBare',         { fg = colors.turquoise,  bg = 'NONE' })  -- Structure bare identifier
  highlight(0, 'smlStridBareMany',     { fg = colors.turquoise,  bg = 'NONE' })  -- Multiple structure identifiers

  -- Signature
  highlight(0, 'smlSignature',         { fg = colors.blue,       bg = 'NONE' })  -- signature keyword
  highlight(0, 'smlSig',               { fg = colors.blue,       bg = 'NONE' })  -- sig keyword
  highlight(0, 'smlSigid',             { fg = colors.turquoise,  bg = 'NONE' })  -- Signature identifiers

  -- Functor
  highlight(0, 'smlFunctor',           { fg = colors.blue,       bg = 'NONE' })  -- functor keyword
  highlight(0, 'smlFunctid',           { fg = colors.turquoise,  bg = 'NONE' })  -- Functor identifiers
  highlight(0, 'smlFunctArgSimple',    { fg = colors.turquoise,  bg = 'NONE' })  -- Simple functor arguments
  highlight(0, 'smlFunctAppSimple',    { fg = colors.turquoise,  bg = 'NONE' })  -- Simple functor application

  -- Module Operations
  highlight(0, 'smlOpen',              { fg = colors.blue,       bg = 'NONE' })  -- open keyword
  highlight(0, 'smlInclude',           { fg = colors.blue,       bg = 'NONE' })  -- include keyword
  highlight(0, 'smlSharing',           { fg = colors.blue,       bg = 'NONE' })  -- sharing keyword
  highlight(0, 'smlWhere',             { fg = colors.blue,       bg = 'NONE' })  -- where keyword

  -- Module Path/Qualification
  highlight(0, 'smlModPath',           { fg = colors.turquoise,  bg = 'NONE' })  -- Module paths (A.B.C)
  highlight(0, 'smlModule',            { fg = colors.turquoise,  bg = 'NONE' })  -- Module names
  highlight(0, 'smlModParam1',         { fg = colors.turquoise,  bg = 'NONE' })  -- Module parameters
  highlight(0, 'smlModType',           { fg = colors.turquoise,  bg = 'NONE' })  -- Module types
  highlight(0, 'smlMPRestr3',          { fg = colors.turquoise,  bg = 'NONE' })  -- Module restrictions
  highlight(0, 'smlFullMod',           { fg = colors.turquoise,  bg = 'NONE' })  -- Full module expressions
  highlight(0, 'smlModTypeRestr',      { fg = colors.turquoise,  bg = 'NONE' })  -- Module type restrictions
  highlight(0, 'smlWith',              { fg = colors.blue,       bg = 'NONE' })  -- with keyword
  highlight(0, 'smlMTDef',             { fg = colors.turquoise,  bg = 'NONE' })  -- Module type definitions
  highlight(0, 'smlModPreRHS',         { fg = colors.turquoise,  bg = 'NONE' })  -- Module pre-RHS
  highlight(0, 'smlMPRestr2',          { fg = colors.turquoise,  bg = 'NONE' })  -- Module restrictions


  ---------------------------------------------------------------------------
  -- Constructors & Built-ins
  ---------------------------------------------------------------------------

  -- Data Constructors
  highlight(0, 'smlConstructor',       { fg = colors.turquoise,  bg = 'NONE' })  -- Data constructors (capitalized)
  highlight(0, 'smlCon',               { fg = colors.turquoise,  bg = 'NONE' })  -- Constructors

  -- Built-in Constants
  highlight(0, 'smlBoolean',           { fg = colors.blue,       bg = 'NONE' })  -- true, false
  highlight(0, 'smlNil',               { fg = colors.turquoise,  bg = 'NONE' })  -- nil
  highlight(0, 'smlRef',               { fg = colors.blue,       bg = 'NONE' })  -- ref
  highlight(0, 'smlCons',              { fg = colors.white,      bg = 'NONE' })  -- :: (cons operator)

  -- Unit
  highlight(0, 'smlUnit',              { fg = colors.turquoise,  bg = 'NONE' })  -- ()


  ---------------------------------------------------------------------------
  -- Identifiers & Variables
  ---------------------------------------------------------------------------

  highlight(0, 'smlIdentifier',        { fg = colors.white,      bg = 'NONE' })  -- General identifiers
  highlight(0, 'smlAnyVar',            { fg = colors.white,      bg = 'NONE' })  -- Any variable
  highlight(0, 'smlVariable',          { fg = colors.white,      bg = 'NONE' })  -- Variables


  ---------------------------------------------------------------------------
  -- Record Fields
  ---------------------------------------------------------------------------

  highlight(0, 'smlRecordField',       { fg = colors.orange,     bg = 'NONE' })  -- Record field names
  highlight(0, 'smlLabel',             { fg = colors.orange,     bg = 'NONE' })  -- Record labels
  highlight(0, 'smlRecordSelector',    { fg = colors.orange,     bg = 'NONE' })  -- #field selector


  ---------------------------------------------------------------------------
  -- Literals
  ---------------------------------------------------------------------------

  -- Numbers
  highlight(0, 'smlNumber',            { fg = colors.greenLight, bg = 'NONE' })  -- General numbers
  highlight(0, 'smlInt',               { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, 'smlWord',              { fg = colors.greenLight, bg = 'NONE' })  -- Word literals (0w...)
  highlight(0, 'smlReal',              { fg = colors.greenLight, bg = 'NONE' })  -- Real numbers (floats)
  highlight(0, 'smlHex',               { fg = colors.greenLight, bg = 'NONE' })  -- Hexadecimal

  -- Strings
  highlight(0, 'smlString',            { fg = colors.redLight,   bg = 'NONE' })  -- String literals
  highlight(0, 'smlStrGap',            { fg = colors.redLight,   bg = 'NONE' })  -- String gaps (\  \)

  -- Characters
  highlight(0, 'smlCharacter',         { fg = colors.redLight,   bg = 'NONE' })  -- Character literals
  highlight(0, 'smlChar',              { fg = colors.redLight,   bg = 'NONE' })  -- Character literals
  highlight(0, 'smlCharEsc',           { fg = colors.pink,       bg = 'NONE' })  -- Character escapes
  highlight(0, 'smlCharEscErr',        { fg = colors.red,        bg = 'NONE' })  -- Invalid char escapes

  -- Escape Sequences
  highlight(0, 'smlEscape',            { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, etc.
  highlight(0, 'smlSpecial',           { fg = colors.pink,       bg = 'NONE' })  -- Special characters


  ---------------------------------------------------------------------------
  -- Operators & Punctuation
  ---------------------------------------------------------------------------

  -- Operators
  highlight(0, 'smlOperator',          { fg = colors.white,      bg = 'NONE' })  -- General operators
  highlight(0, 'smlRefAssign',         { fg = colors.white,      bg = 'NONE' })  -- := (ref assignment)
  highlight(0, 'smlDeref',             { fg = colors.white,      bg = 'NONE' })  -- ! (dereference)

  -- Arrows
  highlight(0, 'smlArrow',             { fg = colors.white,      bg = 'NONE' })  -- -> (function type)
  highlight(0, 'smlFatArrow',          { fg = colors.white,      bg = 'NONE' })  -- => (pattern match)

  -- Punctuation
  highlight(0, 'smlPunc',              { fg = colors.white,      bg = 'NONE' })  -- General punctuation
  highlight(0, 'smlDelim',             { fg = colors.white,      bg = 'NONE' })  -- Delimiters
  highlight(0, 'smlDelimiter',         { fg = colors.white,      bg = 'NONE' })  -- Delimiters

  -- Enclosures
  highlight(0, 'smlEncl',              { fg = colors.white,      bg = 'NONE' })  -- Enclosures
  highlight(0, 'smlParens',            { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'smlBrackets',          { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'smlBraces',            { fg = colors.white,      bg = 'NONE' })  -- { }

  -- Top Level
  highlight(0, 'smlTopStop',           { fg = colors.white,      bg = 'NONE' })  -- ; at top level


  ---------------------------------------------------------------------------
  -- Comments
  ---------------------------------------------------------------------------

  highlight(0, 'smlComment',           { fg = colors.red,        bg = 'NONE' })  -- (* *) comments
  highlight(0, 'smlLineComment',       { fg = colors.red,        bg = 'NONE' })  -- Line comments (Successor ML)
  highlight(0, 'smlCommentErr',        { fg = colors.red,        bg = 'NONE' })  -- Unclosed comments
  highlight(0, 'smlTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME


  ---------------------------------------------------------------------------
  -- Errors
  ---------------------------------------------------------------------------

  highlight(0, 'smlError',             { fg = colors.red,        bg = 'NONE' })  -- General errors
  highlight(0, 'smlBraceErr',          { fg = colors.red,        bg = 'NONE' })  -- Brace errors
  highlight(0, 'smlBrackErr',          { fg = colors.red,        bg = 'NONE' })  -- Bracket errors
  highlight(0, 'smlParenErr',          { fg = colors.red,        bg = 'NONE' })  -- Parenthesis errors
  highlight(0, 'smlEndErr',            { fg = colors.red,        bg = 'NONE' })  -- end without matching begin
  highlight(0, 'smlThenErr',           { fg = colors.red,        bg = 'NONE' })  -- then without if


  ---------------------------------------------------------------------------
  -- SML/NJ Extensions
  ---------------------------------------------------------------------------

  highlight(0, 'smlLazy',              { fg = colors.blue,       bg = 'NONE' })  -- lazy keyword (SML/NJ)
  highlight(0, 'smlSuccML',            { fg = colors.blue,       bg = 'NONE' })  -- Successor ML features
  highlight(0, 'smlOverload',          { fg = colors.blue,       bg = 'NONE' })  -- _overload


  ---------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.sml)
  ---------------------------------------------------------------------------

  -- Comments
  highlight(0, '@comment.sml',                   { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Keywords
  highlight(0, '@keyword.sml',                   { fg = colors.blue,       bg = 'NONE' })  -- Reserved words
  highlight(0, '@keyword.function.sml',          { fg = colors.blue,       bg = 'NONE' })  -- fun, fn
  highlight(0, '@keyword.operator.sml',          { fg = colors.white,      bg = 'NONE' })  -- Operator keywords
  highlight(0, '@keyword.conditional.sml',       { fg = colors.blue,       bg = 'NONE' })  -- if, then, else, case, of
  highlight(0, '@keyword.repeat.sml',            { fg = colors.blue,       bg = 'NONE' })  -- while, do
  highlight(0, '@keyword.exception.sml',         { fg = colors.blue,       bg = 'NONE' })  -- raise, handle, exception
  highlight(0, '@keyword.import.sml',            { fg = colors.blue,       bg = 'NONE' })  -- open
  highlight(0, '@keyword.type.sml',              { fg = colors.blue,       bg = 'NONE' })  -- type, datatype
  highlight(0, '@keyword.modifier.sml',          { fg = colors.blue,       bg = 'NONE' })  -- infix, infixr, nonfix

  -- Variables
  highlight(0, '@variable.sml',                  { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@variable.parameter.sml',        { fg = colors.white,      bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.sml',           { fg = colors.orange,     bg = 'NONE' })  -- Record fields

  -- Types
  highlight(0, '@type.sml',                      { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.sml',              { fg = colors.turquoise,  bg = 'NONE' })  -- int, string, bool, etc.
  highlight(0, '@type.definition.sml',           { fg = colors.turquoise,  bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier.sml',            { fg = colors.turquoise,  bg = 'NONE' })  -- Module qualifiers

  -- Constructors
  highlight(0, '@constructor.sml',               { fg = colors.turquoise,  bg = 'NONE' })  -- Data constructors

  -- Constants
  highlight(0, '@constant.sml',                  { fg = colors.turquoise,  bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.sml',          { fg = colors.turquoise,  bg = 'NONE' })  -- true, false, nil, ::, ref

  -- Functions
  highlight(0, '@function.sml',                  { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.sml',             { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.sml',          { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Modules
  highlight(0, '@module.sml',                    { fg = colors.turquoise,  bg = 'NONE' })  -- Module names

  -- Numbers
  highlight(0, '@number.sml',                    { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.sml',              { fg = colors.greenLight, bg = 'NONE' })  -- Reals

  -- Strings
  highlight(0, '@string.sml',                    { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.sml',             { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences

  -- Characters
  highlight(0, '@character.sml',                 { fg = colors.redLight,   bg = 'NONE' })  -- Characters
  highlight(0, '@character.special.sml',         { fg = colors.pink,       bg = 'NONE' })  -- Special chars

  -- Booleans
  highlight(0, '@boolean.sml',                   { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Operators
  highlight(0, '@operator.sml',                  { fg = colors.white,      bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.sml',       { fg = colors.white,      bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.sml',     { fg = colors.white,      bg = 'NONE' })  -- , ; : :: -> => |

  -- Labels
  highlight(0, '@label.sml',                     { fg = colors.orange,     bg = 'NONE' })  -- Record labels

  -- Error
  highlight(0, '@error.sml',                     { fg = colors.red,        bg = 'NONE' })  -- Misinterpreted identifiers


  ---------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.sml)
  -- Based on Millet language server
  ---------------------------------------------------------------------------

  -- Standard LSP Types
  highlight(0, '@lsp.type.variable.sml',         { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.function.sml',         { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.parameter.sml',        { fg = colors.white,      bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.type.sml',             { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.sml',            { fg = colors.turquoise,  bg = 'NONE' })  -- Signatures
  highlight(0, '@lsp.type.enum.sml',             { fg = colors.turquoise,  bg = 'NONE' })  -- Datatypes
  highlight(0, '@lsp.type.enumMember.sml',       { fg = colors.turquoise,  bg = 'NONE' })  -- Constructors
  highlight(0, '@lsp.type.property.sml',         { fg = colors.orange,     bg = 'NONE' })  -- Record fields
  highlight(0, '@lsp.type.namespace.sml',        { fg = colors.turquoise,  bg = 'NONE' })  -- Structures
  highlight(0, '@lsp.type.typeParameter.sml',    { fg = colors.purple,     bg = 'NONE' })  -- Type variables
  highlight(0, '@lsp.type.operator.sml',         { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.keyword.sml',          { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.string.sml',           { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.sml',           { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.comment.sml',          { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.sml',    { fg = colors.pink,      bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.sml', { fg = colors.orange,    bg = 'NONE' })  -- Function defs
  highlight(0, '@lsp.typemod.type.declaration.sml',     { fg = colors.turquoise, bg = 'NONE' })  -- Type defs


  ---------------------------------------------------------------------------
  -- ML Basis / MLB Files
  ---------------------------------------------------------------------------

  highlight(0, 'mlbBasis',             { fg = colors.blue,       bg = 'NONE' })  -- basis keyword
  highlight(0, 'mlbOpen',              { fg = colors.blue,       bg = 'NONE' })  -- open keyword
  highlight(0, 'mlbLocal',             { fg = colors.blue,       bg = 'NONE' })  -- local keyword
  highlight(0, 'mlbIn',                { fg = colors.blue,       bg = 'NONE' })  -- in keyword
  highlight(0, 'mlbEnd',               { fg = colors.blue,       bg = 'NONE' })  -- end keyword
  highlight(0, 'mlbAnn',               { fg = colors.pink,       bg = 'NONE' })  -- ann keyword (annotations)
  highlight(0, 'mlbAnnotation',        { fg = colors.pink,       bg = 'NONE' })  -- Annotation content
  highlight(0, 'mlbPath',              { fg = colors.redLight,   bg = 'NONE' })  -- File paths
  highlight(0, 'mlbComment',           { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, 'mlbString',            { fg = colors.redLight,   bg = 'NONE' })  -- String paths


  ---------------------------------------------------------------------------
  -- SML/NJ Compilation Manager (CM) Files
  ---------------------------------------------------------------------------

  highlight(0, 'cmKeyword',            { fg = colors.blue,       bg = 'NONE' })  -- CM keywords
  highlight(0, 'cmGroup',              { fg = colors.blue,       bg = 'NONE' })  -- group keyword
  highlight(0, 'cmLibrary',            { fg = colors.blue,       bg = 'NONE' })  -- library keyword
  highlight(0, 'cmIs',                 { fg = colors.blue,       bg = 'NONE' })  -- is keyword
  highlight(0, 'cmSource',             { fg = colors.turquoise,  bg = 'NONE' })  -- Source paths
  highlight(0, 'cmPath',               { fg = colors.redLight,   bg = 'NONE' })  -- File paths
  highlight(0, 'cmComment',            { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, 'cmString',             { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, 'cmVariable',           { fg = colors.white,      bg = 'NONE' })  -- CM variables


  ---------------------------------------------------------------------------
  -- ML-Lex (.lex) Files
  ---------------------------------------------------------------------------

  highlight(0, 'mllexKeyword',         { fg = colors.blue,       bg = 'NONE' })  -- ML-Lex keywords
  highlight(0, 'mllexDelimiter',       { fg = colors.white,      bg = 'NONE' })  -- %% delimiters
  highlight(0, 'mllexRegexp',          { fg = colors.pink,       bg = 'NONE' })  -- Regular expressions
  highlight(0, 'mllexCharClass',       { fg = colors.pink,       bg = 'NONE' })  -- Character classes
  highlight(0, 'mllexAction',          { fg = colors.white,      bg = 'NONE' })  -- Lex actions
  highlight(0, 'mllexComment',         { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, 'mllexState',           { fg = colors.turquoise,  bg = 'NONE' })  -- Lexer states


  ---------------------------------------------------------------------------
  -- ML-Yacc (.grm) Files
  ---------------------------------------------------------------------------

  highlight(0, 'mlyaccKeyword',        { fg = colors.blue,       bg = 'NONE' })  -- ML-Yacc keywords
  highlight(0, 'mlyaccDelimiter',      { fg = colors.white,      bg = 'NONE' })  -- %% delimiters
  highlight(0, 'mlyaccToken',          { fg = colors.turquoise,  bg = 'NONE' })  -- Token definitions
  highlight(0, 'mlyaccNonterminal',    { fg = colors.orange,     bg = 'NONE' })  -- Nonterminal symbols
  highlight(0, 'mlyaccAction',         { fg = colors.white,      bg = 'NONE' })  -- Yacc actions
  highlight(0, 'mlyaccComment',        { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, 'mlyaccPrecAssoc',      { fg = colors.blue,       bg = 'NONE' })  -- %left, %right, etc.


  ---------------------------------------------------------------------------
  -- Basis Library Types
  ---------------------------------------------------------------------------

  -- Basic Types
  highlight(0, 'smlTypeInt',           { fg = colors.turquoise,  bg = 'NONE' })  -- int
  highlight(0, 'smlTypeReal',          { fg = colors.turquoise,  bg = 'NONE' })  -- real
  highlight(0, 'smlTypeBool',          { fg = colors.turquoise,  bg = 'NONE' })  -- bool
  highlight(0, 'smlTypeString',        { fg = colors.turquoise,  bg = 'NONE' })  -- string
  highlight(0, 'smlTypeChar',          { fg = colors.turquoise,  bg = 'NONE' })  -- char
  highlight(0, 'smlTypeUnit',          { fg = colors.turquoise,  bg = 'NONE' })  -- unit
  highlight(0, 'smlTypeWord',          { fg = colors.turquoise,  bg = 'NONE' })  -- word

  -- Container Types
  highlight(0, 'smlTypeList',          { fg = colors.turquoise,  bg = 'NONE' })  -- list
  highlight(0, 'smlTypeArray',         { fg = colors.turquoise,  bg = 'NONE' })  -- array
  highlight(0, 'smlTypeVector',        { fg = colors.turquoise,  bg = 'NONE' })  -- vector
  highlight(0, 'smlTypeOption',        { fg = colors.turquoise,  bg = 'NONE' })  -- option
  highlight(0, 'smlTypeRef',           { fg = colors.turquoise,  bg = 'NONE' })  -- ref

  -- IO Types
  highlight(0, 'smlTypeInstream',      { fg = colors.turquoise,  bg = 'NONE' })  -- TextIO.instream
  highlight(0, 'smlTypeOutstream',     { fg = colors.turquoise,  bg = 'NONE' })  -- TextIO.outstream

  -- Exception Types
  highlight(0, 'smlTypeExn',           { fg = colors.turquoise,  bg = 'NONE' })  -- exn

  -- Order Type
  highlight(0, 'smlTypeOrder',         { fg = colors.turquoise,  bg = 'NONE' })  -- order (LESS, EQUAL, GREATER)


  ---------------------------------------------------------------------------
  -- Standard Exceptions
  ---------------------------------------------------------------------------

  highlight(0, 'smlExnBind',           { fg = colors.turquoise,  bg = 'NONE' })  -- Bind
  highlight(0, 'smlExnMatch',          { fg = colors.turquoise,  bg = 'NONE' })  -- Match
  highlight(0, 'smlExnOverflow',       { fg = colors.turquoise,  bg = 'NONE' })  -- Overflow
  highlight(0, 'smlExnDiv',            { fg = colors.turquoise,  bg = 'NONE' })  -- Div
  highlight(0, 'smlExnDomain',         { fg = colors.turquoise,  bg = 'NONE' })  -- Domain
  highlight(0, 'smlExnSize',           { fg = colors.turquoise,  bg = 'NONE' })  -- Size
  highlight(0, 'smlExnSubscript',      { fg = colors.turquoise,  bg = 'NONE' })  -- Subscript
  highlight(0, 'smlExnFail',           { fg = colors.turquoise,  bg = 'NONE' })  -- Fail
  highlight(0, 'smlExnEmpty',          { fg = colors.turquoise,  bg = 'NONE' })  -- Empty
  highlight(0, 'smlExnOption',         { fg = colors.turquoise,  bg = 'NONE' })  -- Option


  ---------------------------------------------------------------------------
  -- Basis Library Structures
  ---------------------------------------------------------------------------

  highlight(0, 'smlBasisList',         { fg = colors.turquoise,  bg = 'NONE' })  -- List structure
  highlight(0, 'smlBasisArray',        { fg = colors.turquoise,  bg = 'NONE' })  -- Array structure
  highlight(0, 'smlBasisVector',       { fg = colors.turquoise,  bg = 'NONE' })  -- Vector structure
  highlight(0, 'smlBasisString',       { fg = colors.turquoise,  bg = 'NONE' })  -- String structure
  highlight(0, 'smlBasisChar',         { fg = colors.turquoise,  bg = 'NONE' })  -- Char structure
  highlight(0, 'smlBasisInt',          { fg = colors.turquoise,  bg = 'NONE' })  -- Int structure
  highlight(0, 'smlBasisReal',         { fg = colors.turquoise,  bg = 'NONE' })  -- Real structure
  highlight(0, 'smlBasisWord',         { fg = colors.turquoise,  bg = 'NONE' })  -- Word structure
  highlight(0, 'smlBasisBool',         { fg = colors.turquoise,  bg = 'NONE' })  -- Bool structure
  highlight(0, 'smlBasisOption',       { fg = colors.turquoise,  bg = 'NONE' })  -- Option structure
  highlight(0, 'smlBasisTextIO',       { fg = colors.turquoise,  bg = 'NONE' })  -- TextIO structure
  highlight(0, 'smlBasisBinIO',        { fg = colors.turquoise,  bg = 'NONE' })  -- BinIO structure
  highlight(0, 'smlBasisOS',           { fg = colors.turquoise,  bg = 'NONE' })  -- OS structure
  highlight(0, 'smlBasisGeneral',      { fg = colors.turquoise,  bg = 'NONE' })  -- General structure
  highlight(0, 'smlBasisMath',         { fg = colors.turquoise,  bg = 'NONE' })  -- Math structure


  ---------------------------------------------------------------------------
  -- Conceal Characters (vim-better-sml style)
  ---------------------------------------------------------------------------

  -- These are for visual substitutions when conceallevel > 0
  highlight(0, 'smlGreek',             { fg = colors.purple,     bg = 'NONE' })  -- Greek letters for 'a -> α
  highlight(0, 'smlConceal',           { fg = colors.white,      bg = 'NONE' })  -- Concealed text
  highlight(0, 'smlArrowConceal',      { fg = colors.white,      bg = 'NONE' })  -- -> concealed as →
  highlight(0, 'smlFatArrowConceal',   { fg = colors.white,      bg = 'NONE' })  -- => concealed as ⇒

end

return sml
