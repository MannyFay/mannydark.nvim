-------------------------------------------------------------------------------
-- Ruby Files
-- Highlighting for .rb, .rake, .gemspec, Gemfile, Rakefile files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local ruby    = {}


-------------------------------------------------------------------------------
-- Settings

ruby.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'rubyKeyword',           { link = "Keyword" })  -- General keywords
  highlight(0, 'rubyStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- return, break, next, redo, retry
  highlight(0, 'rubyConditional',       { link = "Conditional" })  -- if, unless, elsif, else, case, when, then
  highlight(0, 'rubyRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- while, until, for, loop
  highlight(0, 'rubyException',         { fg = colors.blue,       bg = 'NONE'            })  -- begin, rescue, ensure, raise, fail
  highlight(0, 'rubyExceptionHandler',  { fg = colors.blue,       bg = 'NONE'            })  -- rescue, ensure
  highlight(0, 'rubyControl',           { fg = colors.blue,       bg = 'NONE'            })  -- do, end, yield
  highlight(0, 'rubyAccess',            { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected
  highlight(0, 'rubyAttribute',         { fg = colors.blue,       bg = 'NONE'            })  -- attr_reader, attr_writer, attr_accessor
  highlight(0, 'rubyInclude',           { fg = colors.blue,       bg = 'NONE'            })  -- require, require_relative, load, include, extend, prepend
  highlight(0, 'rubyDefine',            { fg = colors.blue,       bg = 'NONE'            })  -- def, undef, alias
  highlight(0, 'rubyClass',             { fg = colors.blue,       bg = 'NONE'            })  -- class keyword
  highlight(0, 'rubyModule',            { fg = colors.blue,       bg = 'NONE'            })  -- module keyword
  highlight(0, 'rubyBlock',             { fg = colors.blue,       bg = 'NONE'            })  -- do, end
  highlight(0, 'rubyBlockParameter',    { fg = colors.purple,     bg = 'NONE'            })  -- Block parameters |x, y|
  highlight(0, 'rubyBeginEnd',          { fg = colors.blue,       bg = 'NONE'            })  -- BEGIN, END

  -- Special Keywords
  highlight(0, 'rubyPseudoVariable',    { link = "Variable" })  -- self, nil, true, false, __FILE__, __LINE__, __ENCODING__
  highlight(0, 'rubyNil',               { fg = colors.blue,       bg = 'NONE'            })  -- nil
  highlight(0, 'rubyBoolean',           { link = "Boolean" })  -- true, false
  highlight(0, 'rubySelf',              { fg = colors.blue,       bg = 'NONE'            })  -- self
  highlight(0, 'rubySuper',             { fg = colors.blue,       bg = 'NONE'            })  -- super
  highlight(0, 'rubyLambda',            { fg = colors.blue,       bg = 'NONE'            })  -- lambda, ->
  highlight(0, 'rubyProc',              { fg = colors.blue,       bg = 'NONE'            })  -- proc

  -- Types and Classes
  highlight(0, 'rubyConstant',          { link = "Constant" })  -- Constants, Class names
  highlight(0, 'rubyClassName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names in definitions
  highlight(0, 'rubyModuleName',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'rubyException',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Exception classes

  -- Functions and Methods
  highlight(0, 'rubyFunction',          { link = "Function" })  -- Method definitions
  highlight(0, 'rubyMethodName',        { link = "Function" })  -- Method names
  highlight(0, 'rubyMethodBlock',       { link = "Function" })  -- Method calls with blocks
  highlight(0, 'rubyDoBlock',           { fg = colors.blue,       bg = 'NONE'            })  -- do...end blocks

  -- Variables
  highlight(0, 'rubyIdentifier',        { fg = colors.purple,     bg = 'NONE'            })  -- Local variables
  highlight(0, 'rubyLocalVariable',     { link = "Variable" })  -- Local variables
  highlight(0, 'rubyInstanceVariable',  { link = "Variable" })  -- @instance_variables
  highlight(0, 'rubyClassVariable',     { link = "Variable" })  -- @@class_variables
  highlight(0, 'rubyGlobalVariable',    { link = "Variable" })  -- $global_variables
  highlight(0, 'rubyPredefinedVariable', { link = "Variable" })  -- $!, $@, $&, $`, $', $+, etc.
  highlight(0, 'rubyBlockArgument',     { fg = colors.purple,     bg = 'NONE'            })  -- &block arguments

  -- Symbols
  highlight(0, 'rubySymbol',            { fg = colors.pink,       bg = 'NONE'            })  -- :symbols
  highlight(0, 'rubySymbolDelimiter',   { link = "Delimiter" })  -- Symbol delimiters

  -- Strings
  highlight(0, 'rubyString',            { link = "String" })  -- "strings" and 'strings'
  highlight(0, 'rubyStringDelimiter',   { link = "Delimiter" })  -- String delimiters
  highlight(0, 'rubyInterpolation',     { fg = colors.pink,       bg = 'NONE'            })  -- #{interpolation}
  highlight(0, 'rubyInterpolationDelimiter', { link = "Delimiter" })  -- #{ and }
  highlight(0, 'rubyStringEscape',      { link = "String" })  -- \n, \t, etc.
  highlight(0, 'rubyQuoteEscape',       { fg = colors.pink,       bg = 'NONE'            })  -- \" and \'
  highlight(0, 'rubyHeredoc',           { fg = colors.redLight,   bg = 'NONE'            })  -- <<~HEREDOC
  highlight(0, 'rubyHeredocDelimiter',  { link = "Delimiter" })  -- HEREDOC delimiters
  highlight(0, 'rubyPercentStringDelimiter', { link = "Delimiter" })  -- %w, %i, %q, %Q, %x

  -- Regular Expressions
  highlight(0, 'rubyRegexp',            { fg = colors.redLight,   bg = 'NONE'            })  -- /regex/
  highlight(0, 'rubyRegexpDelimiter',   { link = "Delimiter" })  -- Regex delimiters
  highlight(0, 'rubyRegexpSpecial',     { fg = colors.pink,       bg = 'NONE'            })  -- Regex special chars
  highlight(0, 'rubyRegexpEscape',      { fg = colors.pink,       bg = 'NONE'            })  -- Regex escapes
  highlight(0, 'rubyRegexpCharClass',   { fg = colors.pink,       bg = 'NONE'            })  -- \d, \w, \s, etc.
  highlight(0, 'rubyRegexpQuantifier',  { fg = colors.pink,       bg = 'NONE'            })  -- *, +, ?, {n,m}
  highlight(0, 'rubyRegexpAnchor',      { fg = colors.pink,       bg = 'NONE'            })  -- ^, $, \b, \A, \Z

  -- Numbers
  highlight(0, 'rubyInteger',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'rubyFloat',             { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'rubyNumber',            { link = "Number" })  -- All numbers

  -- Operators
  highlight(0, 'rubyOperator',          { link = "Operator" })  -- Operators
  highlight(0, 'rubyPseudoOperator',    { link = "Operator" })  -- ::, .., ...
  highlight(0, 'rubyRangeOperator',     { link = "Operator" })  -- .., ...
  highlight(0, 'rubySplatOperator',     { link = "Operator" })  -- *, **
  highlight(0, 'rubySafeNavigation',    { fg = colors.white,      bg = 'NONE'            })  -- &. safe navigation

  -- Comments
  highlight(0, 'rubyComment',           { link = "Comment" })  -- # comments
  highlight(0, 'rubyMultilineComment',  { link = "Comment" })  -- =begin...=end
  highlight(0, 'rubySharpBang',         { fg = colors.red,        bg = 'NONE'            })  -- Shebang
  highlight(0, 'rubyTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- RDoc/YARD
  highlight(0, 'rubyDocumentation',     { fg = colors.red,        bg = 'NONE'            })  -- Documentation comments
  highlight(0, 'rubyData',              { fg = colors.gray,       bg = 'NONE'            })  -- __END__ data section

  -- DSL Keywords (Rails, RSpec, etc.)
  highlight(0, 'rubyRailsMethod',       { link = "Function" })  -- Rails methods
  highlight(0, 'rubyRailsRenderMethod', { link = "Function" })  -- render, redirect_to
  highlight(0, 'rubyRailsARMethod',     { link = "Function" })  -- ActiveRecord methods

  -- Errors
  highlight(0, 'rubySpaceError',        { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Space errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.ruby)

  -- Variables
  highlight(0, '@variable.ruby',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.ruby',      { link = "Variable" })  -- self, __FILE__, __LINE__
  highlight(0, '@variable.parameter.ruby',    { link = "Variable" })  -- Method parameters
  highlight(0, '@variable.member.ruby',       { link = "Variable" })  -- Instance variables

  -- Constants
  highlight(0, '@constant.ruby',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.ruby',      { link = "Constant" })  -- nil, true, false

  -- Functions
  highlight(0, '@function.ruby',              { link = "Function" })  -- Method definitions
  highlight(0, '@function.call.ruby',         { link = "Function" })  -- Method calls
  highlight(0, '@function.method.ruby',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.ruby',  { link = "Function" })  -- Method calls
  highlight(0, '@function.builtin.ruby',      { link = "Function" })  -- puts, print, p, etc.
  highlight(0, '@constructor.ruby',           { fg = colors.turquoise, bg = 'NONE' })  -- Class.new

  -- Types
  highlight(0, '@type.ruby',                  { link = "Type" })  -- Class/Module names
  highlight(0, '@type.builtin.ruby',          { link = "Type" })  -- Built-in classes (String, Array, Hash)
  highlight(0, '@type.definition.ruby',       { link = "Type" })  -- Class/Module definitions

  -- Symbols
  highlight(0, '@string.special.symbol.ruby', { link = "String" })  -- :symbols

  -- Keywords
  highlight(0, '@keyword.ruby',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.ruby',      { link = "Keyword" })  -- def, lambda
  highlight(0, '@keyword.type.ruby',          { link = "Keyword" })  -- class, module
  highlight(0, '@keyword.modifier.ruby',      { link = "Keyword" })  -- public, private, protected
  highlight(0, '@keyword.return.ruby',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.import.ruby',        { link = "Keyword" })  -- require, require_relative, load
  highlight(0, '@keyword.repeat.ruby',        { link = "Keyword" })  -- while, until, for, loop
  highlight(0, '@keyword.conditional.ruby',   { link = "Conditional" })  -- if, unless, elsif, else, case, when
  highlight(0, '@keyword.exception.ruby',     { link = "Keyword" })  -- begin, rescue, ensure, raise
  highlight(0, '@keyword.operator.ruby',      { link = "Operator" })  -- and, or, not, defined?

  -- Strings
  highlight(0, '@string.ruby',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.ruby',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.regexp.ruby',         { link = "String" })  -- Regex patterns
  highlight(0, '@string.special.ruby',        { link = "String" })  -- Interpolation

  -- Numbers
  highlight(0, '@number.ruby',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.ruby',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.ruby',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.ruby',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.ruby', { link = "Comment" })  -- RDoc/YARD comments

  -- Modules
  highlight(0, '@module.ruby',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@label.ruby',                 { fg = colors.pink,      bg = 'NONE' })  -- Hash keys as labels (key:)
  highlight(0, '@property.ruby',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.ruby',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.ruby',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, ||
  highlight(0, '@punctuation.delimiter.ruby', { link = "Delimiter" })  -- , ; :
  highlight(0, '@punctuation.special.ruby',   { fg = colors.pink,      bg = 'NONE' })  -- #{} interpolation brackets


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.ruby)

  highlight(0, '@lsp.type.variable.ruby',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.ruby',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.ruby',      { fg = colors.purple,    bg = 'NONE' })  -- Instance variables
  highlight(0, '@lsp.type.function.ruby',      { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.method.ruby',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.ruby',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.module.ruby',        { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.type.ruby',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.namespace.ruby',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.symbol.ruby',        { fg = colors.pink,      bg = 'NONE' })  -- Symbols
  highlight(0, '@lsp.type.keyword.ruby',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.ruby',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.ruby',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.ruby',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.ruby',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.ruby',   { link = "Variable" })  -- Constants
  highlight(0, '@lsp.typemod.method.declaration.ruby',  { fg = colors.orange,    bg = 'NONE' })  -- Method declarations
  highlight(0, '@lsp.typemod.class.declaration.ruby',   { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.module.declaration.ruby',  { fg = colors.turquoise, bg = 'NONE' })  -- Module declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.ruby', { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return ruby
