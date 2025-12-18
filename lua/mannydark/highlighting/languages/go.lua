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
  highlight(0, 'goVar',           { fg = colors.blue,       bg = 'NONE'            })  -- var keyword
  highlight(0, 'goConst',         { fg = colors.blue,       bg = 'NONE'            })  -- const keyword
  highlight(0, 'goDeclaration',   { fg = colors.blue,       bg = 'NONE'            })  -- type, func keywords
  highlight(0, 'goStatement',     { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, goto, fallthrough, defer
  highlight(0, 'goConditional',   { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch, select
  highlight(0, 'goLabel',         { fg = colors.blue,       bg = 'NONE'            })  -- Labels for goto
  highlight(0, 'goRepeat',        { fg = colors.blue,       bg = 'NONE'            })  -- for, range
  highlight(0, 'goTypeDecl',      { fg = colors.blue,       bg = 'NONE'            })  -- type keyword in declarations
  highlight(0, 'goDeclType',      { fg = colors.blue,       bg = 'NONE'            })  -- struct, interface, map, chan

  -- Types
  highlight(0, 'goType',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Basic types (int, string, etc.)
  highlight(0, 'goSignedInts',    { fg = colors.turquoise,  bg = 'NONE'            })  -- int8, int16, int32, int64
  highlight(0, 'goUnsignedInts',  { fg = colors.turquoise,  bg = 'NONE'            })  -- uint8, uint16, uint32, uint64, byte
  highlight(0, 'goFloats',        { fg = colors.turquoise,  bg = 'NONE'            })  -- float32, float64
  highlight(0, 'goComplexes',     { fg = colors.turquoise,  bg = 'NONE'            })  -- complex64, complex128
  highlight(0, 'goExtraType',     { fg = colors.turquoise,  bg = 'NONE'            })  -- error, rune
  highlight(0, 'goTypeName',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Custom type names
  highlight(0, 'goReceiverType',  { fg = colors.turquoise,  bg = 'NONE'            })  -- Receiver type in methods
  highlight(0, 'goTypeConstructor', { fg = colors.turquoise, bg = 'NONE'           })  -- Type constructors
  highlight(0, 'goFunctionCall',  { fg = colors.turquoise,  bg = 'NONE'            })  -- Called type constructors

  -- Functions
  highlight(0, 'goFunction',      { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'goBuiltins',      { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions (make, new, len, cap, append, etc.)

  -- Variables and Parameters
  highlight(0, 'goParamName',     { fg = colors.purple,     bg = 'NONE'            })  -- Parameter names
  highlight(0, 'goReceiverVar',   { fg = colors.purple,     bg = 'NONE'            })  -- Receiver variable (self-like)
  highlight(0, 'goField',         { fg = colors.purple,     bg = 'NONE'            })  -- Struct field access
  highlight(0, 'goVarAssign',     { fg = colors.purple,     bg = 'NONE'            })  -- Variable assignments
  highlight(0, 'goVarDefs',       { fg = colors.purple,     bg = 'NONE'            })  -- Variable definitions

  -- Constants
  highlight(0, 'goPredefinedIdentifiers', { fg = colors.blue, bg = 'NONE'          })  -- nil, true, false, iota
  highlight(0, 'goBoolean',       { fg = colors.blue,       bg = 'NONE'            })  -- true, false

  -- Strings
  highlight(0, 'goString',        { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'goRawString',     { fg = colors.redLight,   bg = 'NONE'            })  -- `raw strings`
  highlight(0, 'goImportString',  { fg = colors.redLight,   bg = 'NONE'            })  -- Import paths
  highlight(0, 'goCharacter',     { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' rune literals
  highlight(0, 'goSpecialString', { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, 'goEscapeOctal',   { fg = colors.pink,       bg = 'NONE'            })  -- \NNN
  highlight(0, 'goEscapeC',       { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'goEscapeX',       { fg = colors.pink,       bg = 'NONE'            })  -- \xNN
  highlight(0, 'goEscapeU',       { fg = colors.pink,       bg = 'NONE'            })  -- \uNNNN
  highlight(0, 'goEscapeBigU',    { fg = colors.pink,       bg = 'NONE'            })  -- \UNNNNNNNN
  highlight(0, 'goFormatSpecifier', { fg = colors.pink,     bg = 'NONE'            })  -- %v, %s, %d, etc.

  -- Operators
  highlight(0, 'goOperator',        { fg = colors.white,    bg = 'NONE'            })  -- Operators
  highlight(0, 'goPointerOperator', { fg = colors.white,    bg = 'NONE'            })  -- * and & operators
  highlight(0, 'goVarArgs',         { fg = colors.white,    bg = 'NONE'            })  -- ... variadic

  -- Comments
  highlight(0, 'goComment',       { fg = colors.red,        bg = 'NONE'            })  -- // and /* */ comments
  highlight(0, 'goTodo',          { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.
  highlight(0, 'goPackageComment', { fg = colors.red,       bg = 'NONE'            })  -- Package documentation
  highlight(0, 'goBuildCommentStart', { fg = colors.red,    bg = 'NONE'            })  -- //go:build

  -- Build tags and directives
  highlight(0, 'goGenerate',      { fg = colors.green,      bg = 'NONE'            })  -- //go:generate
  highlight(0, 'goGenerateVariables', { fg = colors.green,  bg = 'NONE'            })  -- Variables in generate
  highlight(0, 'goBuildKeyword',  { fg = colors.green,      bg = 'NONE'            })  -- Build constraint keywords
  highlight(0, 'goBuildDirectives', { fg = colors.green,    bg = 'NONE'            })  -- Build directives

  -- Errors
  highlight(0, 'goSpaceError',    { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Space errors
  highlight(0, 'goEscapeError',   { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Invalid escapes


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.go)

  -- Variables
  highlight(0, '@variable.go',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.parameter.go',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.go',       { fg = colors.purple,    bg = 'NONE' })  -- Struct fields

  -- Constants
  highlight(0, '@constant.go',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.go',      { fg = colors.blue,      bg = 'NONE' })  -- nil, true, false, iota

  -- Functions
  highlight(0, '@function.go',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.go',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.go',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.go',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.go',      { fg = colors.orange,    bg = 'NONE' })  -- make, new, len, cap, append, etc.
  highlight(0, '@constructor.go',           { fg = colors.turquoise, bg = 'NONE' })  -- Type constructors

  -- Types
  highlight(0, '@type.go',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.go',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types (int, string, error)
  highlight(0, '@type.definition.go',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.go',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.type.go',          { fg = colors.blue,      bg = 'NONE' })  -- type, struct, interface
  highlight(0, '@keyword.function.go',      { fg = colors.blue,      bg = 'NONE' })  -- func
  highlight(0, '@keyword.return.go',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.coroutine.go',     { fg = colors.blue,      bg = 'NONE'            })  -- go (goroutine)
  highlight(0, '@keyword.repeat.go',        { fg = colors.blue,      bg = 'NONE' })  -- for, range
  highlight(0, '@keyword.conditional.go',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch, case, default
  highlight(0, '@keyword.import.go',        { fg = colors.blue,      bg = 'NONE' })  -- import, package

  -- Strings
  highlight(0, '@string.go',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.go',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.regexp.go',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex patterns

  -- Numbers
  highlight(0, '@number.go',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.go',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.go',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.go',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.go', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Modules and Labels
  highlight(0, '@module.go',                { fg = colors.turquoise, bg = 'NONE' })  -- Package names
  highlight(0, '@label.go',                 { fg = colors.blue,      bg = 'NONE' })  -- Labels for goto/break/continue
  highlight(0, '@property.go',              { fg = colors.purple,    bg = 'NONE' })  -- Struct fields

  -- Operators and Punctuation
  highlight(0, '@operator.go',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.go',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.go', { fg = colors.white,     bg = 'NONE' })  -- , ; :


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.go)

  highlight(0, '@lsp.type.variable.go',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.go',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.go',      { fg = colors.purple,    bg = 'NONE' })  -- Struct fields
  highlight(0, '@lsp.type.function.go',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.go',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.go',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.struct.go',        { fg = colors.turquoise, bg = 'NONE' })  -- Struct types
  highlight(0, '@lsp.type.interface.go',     { fg = colors.turquoise, bg = 'NONE' })  -- Interface types
  highlight(0, '@lsp.type.namespace.go',     { fg = colors.turquoise, bg = 'NONE' })  -- Package names
  highlight(0, '@lsp.type.typeParameter.go', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type params
  highlight(0, '@lsp.type.keyword.go',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.go',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.go',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.go',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.go',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.go',    { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.function.declaration.go', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.type.declaration.go',     { fg = colors.turquoise, bg = 'NONE' })  -- Type declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.go',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return go
