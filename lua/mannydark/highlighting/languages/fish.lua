-------------------------------------------------------------------------------
-- Fish Shell Files
-- Highlighting for .fish, config.fish files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local fish    = {}


-------------------------------------------------------------------------------
-- Settings

fish.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'fishKeyword',          { fg = colors.blue,       bg = 'NONE'            })  -- end, else, switch, case, while, for, begin, break, continue, return
  highlight(0, 'fishKeywordIf',        { fg = colors.blue,       bg = 'NONE'            })  -- if
  highlight(0, 'fishKeywordAndOr',     { fg = colors.blue,       bg = 'NONE'            })  -- and, or
  highlight(0, 'fishNot',              { fg = colors.blue,       bg = 'NONE'            })  -- not, !
  highlight(0, 'fishConditional',      { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch, case
  highlight(0, 'fishRepeat',           { fg = colors.blue,       bg = 'NONE'            })  -- while, for
  highlight(0, 'fishException',        { fg = colors.blue,       bg = 'NONE'            })  -- begin, end

  -- Keywords - Command Selection
  highlight(0, 'fishSelectStatement',  { fg = colors.blue,       bg = 'NONE'            })  -- command, builtin
  highlight(0, 'fishExec',             { fg = colors.blue,       bg = 'NONE'            })  -- exec
  highlight(0, 'fishTime',             { fg = colors.blue,       bg = 'NONE'            })  -- time

  -- Keywords - Function
  highlight(0, 'fishFunctionKeyword',  { fg = colors.blue,       bg = 'NONE'            })  -- function
  highlight(0, 'fishFunction',         { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'fishFunctionName',     { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions

  -- Built-in Commands
  highlight(0, 'fishCommand',          { fg = colors.orange,     bg = 'NONE'            })  -- Commands
  highlight(0, 'fishBuiltin',          { fg = colors.orange,     bg = 'NONE'            })  -- Built-in commands

  -- Core Commands
  highlight(0, 'fishSet',              { fg = colors.orange,     bg = 'NONE'            })  -- set
  highlight(0, 'fishEcho',             { fg = colors.orange,     bg = 'NONE'            })  -- echo
  highlight(0, 'fishPrintf',           { fg = colors.orange,     bg = 'NONE'            })  -- printf
  highlight(0, 'fishRead',             { fg = colors.orange,     bg = 'NONE'            })  -- read
  highlight(0, 'fishTest',             { fg = colors.orange,     bg = 'NONE'            })  -- test
  highlight(0, 'fishContains',         { fg = colors.orange,     bg = 'NONE'            })  -- contains
  highlight(0, 'fishCount',            { fg = colors.orange,     bg = 'NONE'            })  -- count
  highlight(0, 'fishString',           { fg = colors.orange,     bg = 'NONE'            })  -- string (command)
  highlight(0, 'fishMath',             { fg = colors.orange,     bg = 'NONE'            })  -- math
  highlight(0, 'fishPath',             { fg = colors.orange,     bg = 'NONE'            })  -- path
  highlight(0, 'fishType',             { fg = colors.orange,     bg = 'NONE'            })  -- type
  highlight(0, 'fishStatus',           { fg = colors.orange,     bg = 'NONE'            })  -- status
  highlight(0, 'fishSource',           { fg = colors.orange,     bg = 'NONE'            })  -- source
  highlight(0, 'fishEval',             { fg = colors.orange,     bg = 'NONE'            })  -- eval
  highlight(0, 'fishArgparse',         { fg = colors.orange,     bg = 'NONE'            })  -- argparse

  -- Completion/Binding Commands
  highlight(0, 'fishComplete',         { fg = colors.orange,     bg = 'NONE'            })  -- complete
  highlight(0, 'fishBind',             { fg = colors.orange,     bg = 'NONE'            })  -- bind
  highlight(0, 'fishAbbr',             { fg = colors.orange,     bg = 'NONE'            })  -- abbr (abbreviations - Fish unique)

  -- Fish-specific Commands
  highlight(0, 'fishSetColor',         { fg = colors.orange,     bg = 'NONE'            })  -- set_color
  highlight(0, 'fishFunctions',        { fg = colors.orange,     bg = 'NONE'            })  -- functions
  highlight(0, 'fishFunced',           { fg = colors.orange,     bg = 'NONE'            })  -- funced
  highlight(0, 'fishFuncsave',         { fg = colors.orange,     bg = 'NONE'            })  -- funcsave
  highlight(0, 'fishFishConfig',       { fg = colors.orange,     bg = 'NONE'            })  -- fish_config

  -- Variables
  highlight(0, 'fishVariable',         { fg = colors.purple,     bg = 'NONE'            })  -- $variable
  highlight(0, 'fishInnerVariable',    { fg = colors.purple,     bg = 'NONE'            })  -- Variables within braces
  highlight(0, 'fishVariableDef',      { fg = colors.purple,     bg = 'NONE'            })  -- Variable definitions
  highlight(0, 'fishDeref',            { fg = colors.purple,     bg = 'NONE'            })  -- Variable dereferencing

  -- Special Variables
  highlight(0, 'fishSpecialVar',       { fg = colors.pink,       bg = 'NONE'            })  -- $status, $pipestatus, $argv, $history
  highlight(0, 'fishBuiltinVar',       { fg = colors.pink,       bg = 'NONE'            })  -- PATH, HOME, PWD, FISH_VERSION, fish_color_*, fish_user_paths

  -- Subscripts/Arrays
  highlight(0, 'fishSubscript',        { fg = colors.white,      bg = 'NONE'            })  -- [...] subscript
  highlight(0, 'fishInnerSubscript',   { fg = colors.greenLight, bg = 'NONE'            })  -- Index values

  -- Parameters/Arguments
  highlight(0, 'fishParameter',        { fg = colors.purple,     bg = 'NONE'            })  -- Positional parameters
  highlight(0, 'fishOption',           { fg = colors.blue,       bg = 'NONE'            })  -- -option, --long-option flags

  -- Strings
  highlight(0, 'fishQuote',            { fg = colors.redLight,   bg = 'NONE'            })  -- 'single' and "double" quoted strings
  highlight(0, 'fishSingleQuote',      { fg = colors.redLight,   bg = 'NONE'            })  -- 'single quoted'
  highlight(0, 'fishDoubleQuote',      { fg = colors.redLight,   bg = 'NONE'            })  -- "double quoted"
  highlight(0, 'fishStringLiteral',    { fg = colors.redLight,   bg = 'NONE'            })  -- String literals

  -- Escape Sequences
  highlight(0, 'fishCharacter',        { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences (\n, \t, etc.)
  highlight(0, 'fishEscape',           { fg = colors.pink,       bg = 'NONE'            })  -- Escape characters

  -- Numbers
  highlight(0, 'fishNumber',           { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'fishInteger',          { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'fishFloat',            { fg = colors.greenLight, bg = 'NONE'            })  -- Floats

  -- Booleans
  highlight(0, 'fishBoolean',          { fg = colors.blue,       bg = 'NONE'            })  -- true, false

  -- Operators
  highlight(0, 'fishOperator',         { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'fishLogicalOp',        { fg = colors.white,      bg = 'NONE'            })  -- &&, ||
  highlight(0, 'fishRangeOp',          { fg = colors.white,      bg = 'NONE'            })  -- .. (range operator)
  highlight(0, 'fishTestOp',           { fg = colors.blue,       bg = 'NONE'            })  -- -eq, -ne, -f, -d, etc.

  -- Redirection
  highlight(0, 'fishRedirection',      { fg = colors.pink,       bg = 'NONE'            })  -- <, >, >>, &>, 2>&1, etc.
  highlight(0, 'fishRedirOp',          { fg = colors.pink,       bg = 'NONE'            })  -- Redirection operators

  -- Pipe
  highlight(0, 'fishPipe',             { fg = colors.white,      bg = 'NONE'            })  -- |
  highlight(0, 'fishPipeNoclobber',    { fg = colors.white,      bg = 'NONE'            })  -- |& (stderr pipe)

  -- Background Job
  highlight(0, 'fishBackgroundJob',    { fg = colors.pink,       bg = 'NONE'            })  -- &

  -- Command Substitution
  highlight(0, 'fishCommandSub',       { fg = colors.purple,     bg = 'NONE'            })  -- (command) or $(command)
  highlight(0, 'fishSubstitution',     { fg = colors.purple,     bg = 'NONE'            })  -- Command substitution

  -- Brace Expansion
  highlight(0, 'fishBraceExpansion',   { fg = colors.pink,       bg = 'NONE'            })  -- {a,b,c}
  highlight(0, 'fishBraces',           { fg = colors.white,      bg = 'NONE'            })  -- { }

  -- Glob Patterns
  highlight(0, 'fishPathGlob',         { fg = colors.pink,       bg = 'NONE'            })  -- *, ?, ~
  highlight(0, 'fishGlob',             { fg = colors.pink,       bg = 'NONE'            })  -- Glob patterns
  highlight(0, 'fishWildcard',         { fg = colors.pink,       bg = 'NONE'            })  -- Wildcards

  -- Brackets
  highlight(0, 'fishBrackets',         { fg = colors.white,      bg = 'NONE'            })  -- [ ]
  highlight(0, 'fishParens',           { fg = colors.white,      bg = 'NONE'            })  -- ( )

  -- Punctuation
  highlight(0, 'fishSemicolon',        { fg = colors.white,      bg = 'NONE'            })  -- ;
  highlight(0, 'fishDelimiter',        { fg = colors.white,      bg = 'NONE'            })  -- Delimiters

  -- Special Functions (Fish-specific)
  highlight(0, 'fishPromptFunc',       { fg = colors.orange,     bg = 'NONE'            })  -- fish_prompt, fish_right_prompt, fish_mode_prompt
  highlight(0, 'fishHookFunc',         { fg = colors.orange,     bg = 'NONE'            })  -- fish_greeting, fish_title, fish_command_not_found

  -- Comments
  highlight(0, 'fishComment',          { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'fishTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Shebang
  highlight(0, 'fishShebang',          { fg = colors.pink,       bg = 'NONE'            })  -- #!/usr/bin/env fish

  -- Error
  highlight(0, 'fishError',            { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.fish)

  -- Variables
  highlight(0, '@variable.fish',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.fish',      { fg = colors.pink,      bg = 'NONE' })  -- status, argv, PATH, HOME, FISH_VERSION, fish_color_*
  highlight(0, '@variable.parameter.fish',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters

  -- Constants
  highlight(0, '@constant.fish',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.fish',      { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Functions
  highlight(0, '@function.fish',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.fish',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.fish',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in commands (echo, set, string, math, etc.)

  -- Keywords
  highlight(0, '@keyword.fish',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.fish',      { fg = colors.blue,      bg = 'NONE' })  -- function
  highlight(0, '@keyword.operator.fish',      { fg = colors.blue,      bg = 'NONE' })  -- and, or, not
  highlight(0, '@keyword.return.fish',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.repeat.fish',        { fg = colors.blue,      bg = 'NONE' })  -- for, while
  highlight(0, '@keyword.conditional.fish',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch, case
  highlight(0, '@keyword.import.fish',        { fg = colors.pink,      bg = 'NONE' })  -- source
  highlight(0, '@keyword.directive.fish',     { fg = colors.pink,      bg = 'NONE' })  -- Shebang

  -- Strings
  highlight(0, '@string.fish',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.fish',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.fish',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.fish',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.fish',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.fish',               { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.fish',              { fg = colors.white,     bg = 'NONE' })  -- Operators (&&, ||, |, ..)
  highlight(0, '@punctuation.bracket.fish',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.fish', { fg = colors.white,     bg = 'NONE' })  -- , ;
  highlight(0, '@punctuation.special.fish',   { fg = colors.pink,      bg = 'NONE' })  -- $ in variables


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.fish)

  highlight(0, '@lsp.type.variable.fish',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.fish',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.function.fish',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.keyword.fish',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.fish',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.fish',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.fish',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.fish',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.fish',    { fg = colors.pink,   bg = 'NONE' })  -- Built-in variables
  highlight(0, '@lsp.typemod.function.declaration.fish', { fg = colors.orange, bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.fish', { fg = colors.orange, bg = 'NONE' })  -- Built-in commands
end

return fish

