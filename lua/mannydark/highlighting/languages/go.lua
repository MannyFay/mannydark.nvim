-------------------------------------------------------------------------------
-- Go Files
-- Highlighting for .go files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local go      = {}


-------------------------------------------------------------------------------
-- Settings

go.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'goPackage',       { fg = colors.blue,       bg = 'NONE'            })  -- package keyword
  highlight(0, 'goImport',        { fg = colors.blue,       bg = 'NONE'            })  -- import keyword
  highlight(0, 'goVar',           { link = "Variable" })  -- var keyword
  highlight(0, 'goConst',         { fg = colors.blue,       bg = 'NONE'            })  -- const keyword
  highlight(0, 'goDeclaration',   { fg = colors.blue,       bg = 'NONE'            })  -- type, func keywords
  highlight(0, 'goStatement',     { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, goto, fallthrough, defer
  highlight(0, 'goConditional',   { link = "Conditional" })  -- if, else, switch, select
  highlight(0, 'goLabel',         { fg = colors.blue,       bg = 'NONE'            })  -- Labels for goto
  highlight(0, 'goRepeat',        { fg = colors.blue,       bg = 'NONE'            })  -- for, range
  highlight(0, 'goTypeDecl',      { link = "Type" })  -- type keyword in declarations
  highlight(0, 'goDeclType',      { link = "Type" })  -- struct, interface, map, chan

  -- Types
  highlight(0, 'goType',          { link = "Type" })  -- Basic types (int, string, etc.)
  highlight(0, 'goSignedInts',    { fg = colors.turquoise,  bg = 'NONE'            })  -- int8, int16, int32, int64
  highlight(0, 'goUnsignedInts',  { fg = colors.turquoise,  bg = 'NONE'            })  -- uint8, uint16, uint32, uint64, byte
  highlight(0, 'goFloats',        { fg = colors.turquoise,  bg = 'NONE'            })  -- float32, float64
  highlight(0, 'goComplexes',     { fg = colors.turquoise,  bg = 'NONE'            })  -- complex64, complex128
  highlight(0, 'goExtraType',     { link = "Type" })  -- error, rune
  highlight(0, 'goTypeName',      { link = "Type" })  -- Custom type names
  highlight(0, 'goReceiverType',  { link = "Type" })  -- Receiver type in methods
  highlight(0, 'goTypeConstructor', { link = "Type" })  -- Type constructors
  highlight(0, 'goFunctionCall',  { link = "Function" })  -- Called type constructors

  -- Functions
  highlight(0, 'goFunction',      { link = "Function" })  -- Function names
  highlight(0, 'goBuiltins',      { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions (make, new, len, cap, append, etc.)

  -- Variables and Parameters
  highlight(0, 'goParamName',     { fg = colors.purple,     bg = 'NONE'            })  -- Parameter names
  highlight(0, 'goReceiverVar',   { link = "Variable" })  -- Receiver variable (self-like)
  highlight(0, 'goField',         { fg = colors.purple,     bg = 'NONE'            })  -- Struct field access
  highlight(0, 'goVarAssign',     { link = "Variable" })  -- Variable assignments
  highlight(0, 'goVarDefs',       { link = "Variable" })  -- Variable definitions

  -- Constants
  highlight(0, 'goPredefinedIdentifiers', { fg = colors.blue, bg = 'NONE'          })  -- nil, true, false, iota
  highlight(0, 'goBoolean',       { link = "Boolean" })  -- true, false

  -- Strings
  highlight(0, 'goString',        { link = "String" })  -- "strings"
  highlight(0, 'goRawString',     { link = "String" })  -- `raw strings`
  highlight(0, 'goImportString',  { link = "String" })  -- Import paths
  highlight(0, 'goCharacter',     { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' rune literals
  highlight(0, 'goSpecialString', { link = "String" })  -- Escape sequences
  highlight(0, 'goEscapeOctal',   { fg = colors.pink,       bg = 'NONE'            })  -- \NNN
  highlight(0, 'goEscapeC',       { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'goEscapeX',       { fg = colors.pink,       bg = 'NONE'            })  -- \xNN
  highlight(0, 'goEscapeU',       { fg = colors.pink,       bg = 'NONE'            })  -- \uNNNN
  highlight(0, 'goEscapeBigU',    { fg = colors.pink,       bg = 'NONE'            })  -- \UNNNNNNNN
  highlight(0, 'goFormatSpecifier', { fg = colors.pink,     bg = 'NONE'            })  -- %v, %s, %d, etc.

  -- Operators
  highlight(0, 'goOperator',        { link = "Operator" })  -- Operators
  highlight(0, 'goPointerOperator', { link = "Operator" })  -- * and & operators
  highlight(0, 'goVarArgs',         { fg = colors.white,    bg = 'NONE'            })  -- ... variadic

  -- Comments
  highlight(0, 'goComment',       { link = "Comment" })  -- // and /* */ comments
  highlight(0, 'goTodo',          { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.
  highlight(0, 'goPackageComment', { link = "Comment" })  -- Package documentation
  highlight(0, 'goBuildCommentStart', { link = "Comment" })  -- //go:build

  -- Build tags and directives
  highlight(0, 'goGenerate',      { fg = colors.green,      bg = 'NONE'            })  -- //go:generate
  highlight(0, 'goGenerateVariables', { link = "Variable" })  -- Variables in generate
  highlight(0, 'goBuildKeyword',  { link = "Keyword" })  -- Build constraint keywords
  highlight(0, 'goBuildDirectives', { fg = colors.green,    bg = 'NONE'            })  -- Build directives

  -- Errors
  highlight(0, 'goSpaceError',    { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Space errors
  highlight(0, 'goEscapeError',   { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Invalid escapes


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.go)

  -- Variables
  highlight(0, '@variable.go',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.go',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.go',       { link = "Variable" })  -- Struct fields

  -- Constants
  highlight(0, '@constant.go',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.go',      { link = "Constant" })  -- nil, true, false, iota

  -- Functions
  highlight(0, '@function.go',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.go',         { link = "Function" })  -- Function calls
  highlight(0, '@function.method.go',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.go',  { link = "Function" })  -- Method calls
  highlight(0, '@function.builtin.go',      { link = "Function" })  -- make, new, len, cap, append, etc.
  highlight(0, '@constructor.go',           { fg = colors.turquoise, bg = 'NONE' })  -- Type constructors

  -- Types
  highlight(0, '@type.go',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.go',          { link = "Type" })  -- Built-in types (int, string, error)
  highlight(0, '@type.definition.go',       { link = "Type" })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.go',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.type.go',          { link = "Keyword" })  -- type, struct, interface
  highlight(0, '@keyword.function.go',      { link = "Keyword" })  -- func
  highlight(0, '@keyword.return.go',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.coroutine.go',     { link = "Keyword" })  -- go (goroutine)
  highlight(0, '@keyword.repeat.go',        { link = "Keyword" })  -- for, range
  highlight(0, '@keyword.conditional.go',   { link = "Conditional" })  -- if, else, switch, case, default
  highlight(0, '@keyword.import.go',        { link = "Keyword" })  -- import, package

  -- Strings
  highlight(0, '@string.go',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.go',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.regexp.go',         { link = "String" })  -- Regex patterns

  -- Numbers
  highlight(0, '@number.go',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.go',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.go',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.go',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.go', { link = "Comment" })  -- Doc comments

  -- Modules and Labels
  highlight(0, '@module.go',                { fg = colors.turquoise, bg = 'NONE' })  -- Package names
  highlight(0, '@label.go',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels for goto/break/continue
  highlight(0, '@property.go',              { fg = colors.purple,    bg = 'NONE' })  -- Struct fields

  -- Operators and Punctuation
  highlight(0, '@operator.go',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.go',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.go', { link = "Delimiter" })  -- , ; :


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.go)

  highlight(0, '@lsp.type.variable.go',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.go',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.go',      { fg = colors.purple,    bg = 'NONE' })  -- Struct fields
  highlight(0, '@lsp.type.function.go',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.go',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.go',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.struct.go',        { fg = colors.turquoise, bg = 'NONE' })  -- Struct types
  highlight(0, '@lsp.type.interface.go',     { fg = colors.turquoise, bg = 'NONE' })  -- Interface types
  highlight(0, '@lsp.type.namespace.go',     { fg = colors.turquoise, bg = 'NONE' })  -- Package names
  highlight(0, '@lsp.type.typeParameter.go', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type params
  highlight(0, '@lsp.type.keyword.go',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.go',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.go',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.go',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.go',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.go',    { link = "Variable" })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.go', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.type.declaration.go',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.go',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return go
