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
  highlight(0, 'rubyKeyword',           { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'rubyStatement',         { fg = colors.blue,       bg = 'NONE'            })  -- return, break, next, redo, retry
  highlight(0, 'rubyConditional',       { fg = colors.blue,       bg = 'NONE'            })  -- if, unless, elsif, else, case, when, then
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
  highlight(0, 'rubyPseudoVariable',    { fg = colors.blue,       bg = 'NONE'            })  -- self, nil, true, false, __FILE__, __LINE__, __ENCODING__
  highlight(0, 'rubyNil',               { fg = colors.blue,       bg = 'NONE'            })  -- nil
  highlight(0, 'rubyBoolean',           { fg = colors.blue,       bg = 'NONE'            })  -- true, false
  highlight(0, 'rubySelf',              { fg = colors.blue,       bg = 'NONE'            })  -- self
  highlight(0, 'rubySuper',             { fg = colors.blue,       bg = 'NONE'            })  -- super
  highlight(0, 'rubyLambda',            { fg = colors.blue,       bg = 'NONE'            })  -- lambda, ->
  highlight(0, 'rubyProc',              { fg = colors.blue,       bg = 'NONE'            })  -- proc

  -- Types and Classes
  highlight(0, 'rubyConstant',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Constants, Class names
  highlight(0, 'rubyClassName',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names in definitions
  highlight(0, 'rubyModuleName',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'rubyException',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Exception classes

  -- Functions and Methods
  highlight(0, 'rubyFunction',          { fg = colors.orange,     bg = 'NONE'            })  -- Method definitions
  highlight(0, 'rubyMethodName',        { fg = colors.orange,     bg = 'NONE'            })  -- Method names
  highlight(0, 'rubyMethodBlock',       { fg = colors.orange,     bg = 'NONE'            })  -- Method calls with blocks
  highlight(0, 'rubyDoBlock',           { fg = colors.blue,       bg = 'NONE'            })  -- do...end blocks

  -- Variables
  highlight(0, 'rubyIdentifier',        { fg = colors.purple,     bg = 'NONE'            })  -- Local variables
  highlight(0, 'rubyLocalVariable',     { fg = colors.purple,     bg = 'NONE'            })  -- Local variables
  highlight(0, 'rubyInstanceVariable',  { fg = colors.purple,     bg = 'NONE'            })  -- @instance_variables
  highlight(0, 'rubyClassVariable',     { fg = colors.purple,     bg = 'NONE'            })  -- @@class_variables
  highlight(0, 'rubyGlobalVariable',    { fg = colors.purple,     bg = 'NONE'            })  -- $global_variables
  highlight(0, 'rubyPredefinedVariable', { fg = colors.purple,    bg = 'NONE'            })  -- $!, $@, $&, $`, $', $+, etc.
  highlight(0, 'rubyBlockArgument',     { fg = colors.purple,     bg = 'NONE'            })  -- &block arguments

  -- Symbols
  highlight(0, 'rubySymbol',            { fg = colors.pink,       bg = 'NONE'            })  -- :symbols
  highlight(0, 'rubySymbolDelimiter',   { fg = colors.pink,       bg = 'NONE'            })  -- Symbol delimiters

  -- Strings
  highlight(0, 'rubyString',            { fg = colors.redLight,   bg = 'NONE'            })  -- "strings" and 'strings'
  highlight(0, 'rubyStringDelimiter',   { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters
  highlight(0, 'rubyInterpolation',     { fg = colors.pink,       bg = 'NONE'            })  -- #{interpolation}
  highlight(0, 'rubyInterpolationDelimiter', { fg = colors.pink,  bg = 'NONE'            })  -- #{ and }
  highlight(0, 'rubyStringEscape',      { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'rubyQuoteEscape',       { fg = colors.pink,       bg = 'NONE'            })  -- \" and \'
  highlight(0, 'rubyHeredoc',           { fg = colors.redLight,   bg = 'NONE'            })  -- <<~HEREDOC
  highlight(0, 'rubyHeredocDelimiter',  { fg = colors.redLight,   bg = 'NONE'            })  -- HEREDOC delimiters
  highlight(0, 'rubyPercentStringDelimiter', { fg = colors.redLight, bg = 'NONE'         })  -- %w, %i, %q, %Q, %x

  -- Regular Expressions
  highlight(0, 'rubyRegexp',            { fg = colors.redLight,   bg = 'NONE'            })  -- /regex/
  highlight(0, 'rubyRegexpDelimiter',   { fg = colors.redLight,   bg = 'NONE'            })  -- Regex delimiters
  highlight(0, 'rubyRegexpSpecial',     { fg = colors.pink,       bg = 'NONE'            })  -- Regex special chars
  highlight(0, 'rubyRegexpEscape',      { fg = colors.pink,       bg = 'NONE'            })  -- Regex escapes
  highlight(0, 'rubyRegexpCharClass',   { fg = colors.pink,       bg = 'NONE'            })  -- \d, \w, \s, etc.
  highlight(0, 'rubyRegexpQuantifier',  { fg = colors.pink,       bg = 'NONE'            })  -- *, +, ?, {n,m}
  highlight(0, 'rubyRegexpAnchor',      { fg = colors.pink,       bg = 'NONE'            })  -- ^, $, \b, \A, \Z

  -- Numbers
  highlight(0, 'rubyInteger',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'rubyFloat',             { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'rubyNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- All numbers

  -- Operators
  highlight(0, 'rubyOperator',          { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'rubyPseudoOperator',    { fg = colors.white,      bg = 'NONE'            })  -- ::, .., ...
  highlight(0, 'rubyRangeOperator',     { fg = colors.white,      bg = 'NONE'            })  -- .., ...
  highlight(0, 'rubySplatOperator',     { fg = colors.white,      bg = 'NONE'            })  -- *, **
  highlight(0, 'rubySafeNavigation',    { fg = colors.white,      bg = 'NONE'            })  -- &. safe navigation

  -- Comments
  highlight(0, 'rubyComment',           { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'rubyMultilineComment',  { fg = colors.red,        bg = 'NONE'            })  -- =begin...=end
  highlight(0, 'rubySharpBang',         { fg = colors.red,        bg = 'NONE'            })  -- Shebang
  highlight(0, 'rubyTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, etc.

  -- RDoc/YARD
  highlight(0, 'rubyDocumentation',     { fg = colors.red,        bg = 'NONE'            })  -- Documentation comments
  highlight(0, 'rubyData',              { fg = colors.gray,       bg = 'NONE'            })  -- __END__ data section

  -- DSL Keywords (Rails, RSpec, etc.)
  highlight(0, 'rubyRailsMethod',       { fg = colors.orange,     bg = 'NONE'            })  -- Rails methods
  highlight(0, 'rubyRailsRenderMethod', { fg = colors.orange,     bg = 'NONE'            })  -- render, redirect_to
  highlight(0, 'rubyRailsARMethod',     { fg = colors.orange,     bg = 'NONE'            })  -- ActiveRecord methods

  -- Errors
  highlight(0, 'rubySpaceError',        { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })  -- Space errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.ruby)

  -- Variables
  highlight(0, '@variable.ruby',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.ruby',      { fg = colors.blue,      bg = 'NONE' })  -- self, __FILE__, __LINE__
  highlight(0, '@variable.parameter.ruby',    { fg = colors.purple,    bg = 'NONE' })  -- Method parameters
  highlight(0, '@variable.member.ruby',       { fg = colors.purple,    bg = 'NONE' })  -- Instance variables

  -- Constants
  highlight(0, '@constant.ruby',              { fg = colors.turquoise, bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.ruby',      { fg = colors.blue,      bg = 'NONE' })  -- nil, true, false

  -- Functions
  highlight(0, '@function.ruby',              { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.call.ruby',         { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.method.ruby',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.ruby',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.ruby',      { fg = colors.orange,    bg = 'NONE' })  -- puts, print, p, etc.
  highlight(0, '@constructor.ruby',           { fg = colors.turquoise, bg = 'NONE' })  -- Class.new

  -- Types
  highlight(0, '@type.ruby',                  { fg = colors.turquoise, bg = 'NONE' })  -- Class/Module names
  highlight(0, '@type.builtin.ruby',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in classes (String, Array, Hash)
  highlight(0, '@type.definition.ruby',       { fg = colors.turquoise, bg = 'NONE' })  -- Class/Module definitions

  -- Symbols
  highlight(0, '@string.special.symbol.ruby', { fg = colors.pink,      bg = 'NONE' })  -- :symbols

  -- Keywords
  highlight(0, '@keyword.ruby',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.ruby',      { fg = colors.blue,      bg = 'NONE' })  -- def, lambda
  highlight(0, '@keyword.type.ruby',          { fg = colors.blue,      bg = 'NONE' })  -- class, module
  highlight(0, '@keyword.modifier.ruby',      { fg = colors.blue,      bg = 'NONE' })  -- public, private, protected
  highlight(0, '@keyword.return.ruby',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.import.ruby',        { fg = colors.blue,      bg = 'NONE' })  -- require, require_relative, load
  highlight(0, '@keyword.repeat.ruby',        { fg = colors.blue,      bg = 'NONE' })  -- while, until, for, loop
  highlight(0, '@keyword.conditional.ruby',   { fg = colors.blue,      bg = 'NONE' })  -- if, unless, elsif, else, case, when
  highlight(0, '@keyword.exception.ruby',     { fg = colors.blue,      bg = 'NONE' })  -- begin, rescue, ensure, raise
  highlight(0, '@keyword.operator.ruby',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not, defined?

  -- Strings
  highlight(0, '@string.ruby',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.ruby',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.regexp.ruby',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex patterns
  highlight(0, '@string.special.ruby',        { fg = colors.pink,      bg = 'NONE' })  -- Interpolation

  -- Numbers
  highlight(0, '@number.ruby',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.ruby',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.ruby',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.ruby',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.ruby', { fg = colors.red,       bg = 'NONE' })  -- RDoc/YARD comments

  -- Modules
  highlight(0, '@module.ruby',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@label.ruby',                 { fg = colors.pink,      bg = 'NONE' })  -- Hash keys as labels (key:)
  highlight(0, '@property.ruby',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Operators and Punctuation
  highlight(0, '@operator.ruby',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.ruby',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, ||
  highlight(0, '@punctuation.delimiter.ruby', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.ruby',   { fg = colors.pink,      bg = 'NONE' })  -- #{} interpolation brackets


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.ruby)

  highlight(0, '@lsp.type.variable.ruby',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.ruby',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.ruby',      { fg = colors.purple,    bg = 'NONE' })  -- Instance variables
  highlight(0, '@lsp.type.function.ruby',      { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.method.ruby',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.class.ruby',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.module.ruby',        { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.type.ruby',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.namespace.ruby',     { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.symbol.ruby',        { fg = colors.pink,      bg = 'NONE' })  -- Symbols
  highlight(0, '@lsp.type.keyword.ruby',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.ruby',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.ruby',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.ruby',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.ruby',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.ruby',   { fg = colors.turquoise, bg = 'NONE' })  -- Constants
  highlight(0, '@lsp.typemod.method.declaration.ruby',  { fg = colors.orange,    bg = 'NONE' })  -- Method declarations
  highlight(0, '@lsp.typemod.class.declaration.ruby',   { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.module.declaration.ruby',  { fg = colors.turquoise, bg = 'NONE' })  -- Module declarations
  highlight(0, '@lsp.typemod.type.defaultLibrary.ruby', { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return ruby
