-------------------------------------------------------------------------------
-- Python Files
-- Highlighting for .py files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local python  = {}


-------------------------------------------------------------------------------
-- Settings

python.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'pythonStatement',     { fg = colors.blue,       bg = 'NONE'            })  -- def, class, return, pass, etc.
  highlight(0, 'pythonConditional',   { fg = colors.blue,       bg = 'NONE'            })  -- if, elif, else
  highlight(0, 'pythonRepeat',        { fg = colors.blue,       bg = 'NONE'            })  -- for, while
  highlight(0, 'pythonOperator',      { fg = colors.blue,       bg = 'NONE'            })  -- and, or, not, in, is
  highlight(0, 'pythonException',     { fg = colors.blue,       bg = 'NONE'            })  -- try, except, finally, raise, with
  highlight(0, 'pythonInclude',       { fg = colors.blue,       bg = 'NONE'            })  -- import, from
  highlight(0, 'pythonAsync',         { fg = colors.blue,       bg = 'NONE'            })  -- async, await

  -- Classes and Types
  highlight(0, 'pythonClass',         { fg = colors.turquoise,  bg = 'NONE'            })  -- class keyword
  highlight(0, 'pythonType',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Type hints
  highlight(0, 'pythonExceptions',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in exceptions (ValueError, etc.)

  -- Functions
  highlight(0, 'pythonFunction',      { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'pythonBuiltin',       { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions (print, len, etc.)
  highlight(0, 'pythonEllipsis',      { fg = colors.orange,     bg = 'NONE'            })  -- ... ellipsis

  -- Variables
  highlight(0, 'pythonClassVar',      { fg = colors.purple,     bg = 'NONE'            })  -- cls, self

  -- Decorators
  highlight(0, 'pythonDecorator',     { fg = colors.pink,       bg = 'NONE'            })  -- @ symbol
  highlight(0, 'pythonDecoratorName', { fg = colors.pink,       bg = 'NONE'            })  -- Decorator name

  -- Strings
  highlight(0, 'pythonString',        { fg = colors.redLight,   bg = 'NONE'            })  -- Regular strings
  highlight(0, 'pythonRawString',     { fg = colors.redLight,   bg = 'NONE'            })  -- r"raw strings"
  highlight(0, 'pythonFString',       { fg = colors.redLight,   bg = 'NONE'            })  -- f"formatted strings"
  highlight(0, 'pythonRawFString',    { fg = colors.redLight,   bg = 'NONE'            })  -- rf"raw formatted"
  highlight(0, 'pythonBytes',         { fg = colors.redLight,   bg = 'NONE'            })  -- b"bytes"
  highlight(0, 'pythonRawBytes',      { fg = colors.redLight,   bg = 'NONE'            })  -- rb"raw bytes"
  highlight(0, 'pythonQuotes',        { fg = colors.redLight,   bg = 'NONE'            })  -- Quote characters
  highlight(0, 'pythonTripleQuotes',  { fg = colors.redLight,   bg = 'NONE'            })  -- Triple quote characters
  highlight(0, 'pythonEscape',        { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'pythonUnicodeEscape', { fg = colors.pink,       bg = 'NONE'            })  -- \uXXXX escapes
  highlight(0, 'pythonFStringDelimiter', { fg = colors.redLight, bg = 'NONE'           })  -- {} in f-strings

  -- Numbers
  highlight(0, 'pythonNumber',        { fg = colors.greenLight, bg = 'NONE'            })  -- All numbers

  -- Comments
  highlight(0, 'pythonComment',       { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'pythonTodo',          { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- Doctest
  highlight(0, 'pythonDoctest',       { fg = colors.gray,       bg = 'NONE'            })  -- >>> in docstrings
  highlight(0, 'pythonDoctestValue',  { fg = colors.gray,       bg = 'NONE'            })  -- Expected values

  -- Errors
  highlight(0, 'pythonSpaceError',    { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Mixed tabs/spaces


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.python)

  -- Variables
  highlight(0, '@variable.python',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.python',      { fg = colors.blue,      bg = 'NONE' })  -- self, cls
  highlight(0, '@variable.parameter.python',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.python',       { fg = colors.purple,    bg = 'NONE' })  -- Instance attributes

  -- Constants
  highlight(0, '@constant.python',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.python',      { fg = colors.blue,      bg = 'NONE' })  -- None, True, False, Ellipsis

  -- Functions
  highlight(0, '@function.python',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.python',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.python',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.method.python',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.python',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.macro.python',        { fg = colors.orange,    bg = 'NONE' })  -- Type conversions (int(), str())
  highlight(0, '@constructor.python',           { fg = colors.turquoise, bg = 'NONE' })  -- Class instantiation

  -- Types
  highlight(0, '@type.python',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type annotations
  highlight(0, '@type.builtin.python',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types (int, str, list, dict)
  highlight(0, '@type.definition.python',       { fg = colors.turquoise, bg = 'NONE' })  -- TypeAlias definitions

  -- Attributes (Decorators)
  highlight(0, '@attribute.python',             { fg = colors.pink,      bg = 'NONE' })  -- Decorator names
  highlight(0, '@attribute.builtin.python',     { fg = colors.pink,      bg = 'NONE' })  -- Built-in decorators (@property, @staticmethod)

  -- Keywords
  highlight(0, '@keyword.python',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.python',      { fg = colors.blue,      bg = 'NONE' })  -- def, lambda
  highlight(0, '@keyword.type.python',          { fg = colors.blue,      bg = 'NONE' })  -- class
  highlight(0, '@keyword.operator.python',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not, in, is
  highlight(0, '@keyword.return.python',        { fg = colors.blue,      bg = 'NONE' })  -- return, yield
  highlight(0, '@keyword.import.python',        { fg = colors.blue,      bg = 'NONE' })  -- import, from
  highlight(0, '@keyword.conditional.python',   { fg = colors.blue,      bg = 'NONE' })  -- if, elif, else
  highlight(0, '@keyword.repeat.python',        { fg = colors.blue,      bg = 'NONE' })  -- for, while
  highlight(0, '@keyword.exception.python',     { fg = colors.blue,      bg = 'NONE' })  -- try, except, finally, raise, with
  highlight(0, '@keyword.coroutine.python',     { fg = colors.blue,      bg = 'NONE' })  -- async, await

  -- Strings
  highlight(0, '@string.python',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.documentation.python',  { fg = colors.redLight,  bg = 'NONE' })  -- Docstrings
  highlight(0, '@string.escape.python',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.regexp.python',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex patterns
  highlight(0, '@string.special.python',        { fg = colors.pink,      bg = 'NONE' })  -- Format specs in f-strings

  -- Numbers
  highlight(0, '@number.python',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.python',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.python',               { fg = colors.blue,      bg = 'NONE' })  -- True, False

  -- Comments
  highlight(0, '@comment.python',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.python', { fg = colors.red,       bg = 'NONE' })  -- Docstring comments

  -- Modules
  highlight(0, '@module.python',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@module.builtin.python',        { fg = colors.turquoise, bg = 'NONE' })  -- Built-in modules

  -- Operators and Punctuation
  highlight(0, '@operator.python',              { fg = colors.white,     bg = 'NONE' })  -- Operators (+, -, *, /, =)
  highlight(0, '@punctuation.bracket.python',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.python', { fg = colors.white,     bg = 'NONE' })  -- , : ;
  highlight(0, '@punctuation.special.python',   { fg = colors.redLight,  bg = 'NONE' })  -- {} in f-strings

  -- Special
  highlight(0, '@character.special.python',     { fg = colors.blue,      bg = 'NONE' })  -- Wildcard patterns


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.python)

  highlight(0, '@lsp.type.variable.python',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.python',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.python',      { fg = colors.purple,    bg = 'NONE' })  -- Properties/Attributes
  highlight(0, '@lsp.type.function.python',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.python',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.python',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.type.python',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.namespace.python',     { fg = colors.turquoise, bg = 'NONE' })  -- Modules/Namespaces
  highlight(0, '@lsp.type.enum.python',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.python',    { fg = colors.purple,    bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.decorator.python',     { fg = colors.pink,      bg = 'NONE' })  -- Decorators
  highlight(0, '@lsp.type.typeParameter.python', { fg = colors.turquoise, bg = 'NONE' })  -- Generic type params
  highlight(0, '@lsp.type.comment.python',       { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.string.python',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.python',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.keyword.python',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.python',      { fg = colors.white,     bg = 'NONE' })  -- Operators

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.python',    { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.function.builtin.python',     { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@lsp.typemod.class.builtin.python',        { fg = colors.turquoise, bg = 'NONE' })  -- Built-in classes
  highlight(0, '@lsp.typemod.variable.defaultLibrary.python', { fg = colors.blue,   bg = 'NONE' })  -- self, cls
end

return python
