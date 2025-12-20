-------------------------------------------------------------------------------
-- Zsh Files
-- Highlighting for .zsh, .zshrc, .zshenv, .zprofile, .zlogin, .zlogout files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local zsh     = {}


-------------------------------------------------------------------------------
-- Settings

zsh.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'zshConditional',       { link = "Conditional" })  -- if, then, elif, else, fi, esac, select
  highlight(0, 'zshRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- while, until, repeat, for, foreach
  highlight(0, 'zshDelimiter',         { link = "Delimiter" })  -- do, done, end
  highlight(0, 'zshException',         { fg = colors.blue,       bg = 'NONE'            })  -- always

  -- Keywords - Case
  highlight(0, 'zshCase',              { fg = colors.blue,       bg = 'NONE'            })  -- case
  highlight(0, 'zshCaseWord',          { fg = colors.blue,       bg = 'NONE'            })  -- case word
  highlight(0, 'zshCaseIn',            { fg = colors.blue,       bg = 'NONE'            })  -- in
  highlight(0, 'zshCasePattern',       { fg = colors.redLight,   bg = 'NONE'            })  -- Case patterns

  -- Keywords - Function
  highlight(0, 'zshKeyword',           { link = "Keyword" })  -- function
  highlight(0, 'zshFunction',          { link = "Function" })  -- Function names
  highlight(0, 'zshKSHFunction',       { link = "Function" })  -- Ksh-style function names

  -- Keywords - Precommand Modifiers
  highlight(0, 'zshPrecommand',        { fg = colors.blue,       bg = 'NONE'            })  -- noglob, nocorrect, exec, command, builtin, -, time

  -- Keywords - Types
  highlight(0, 'zshTypes',             { link = "Type" })  -- float, integer, local, typeset, declare, private, readonly

  -- Built-in Commands
  highlight(0, 'zshCommands',          { fg = colors.orange,     bg = 'NONE'            })  -- alias, autoload, bg, bindkey, break, cd, chdir, echo, eval, exec, exit, export, fc, fg, functions, hash, history, jobs, kill, let, logout, print, printf, pwd, read, return, set, shift, source, suspend, test, trap, type, ulimit, umask, unalias, unfunction, unhash, unset, wait, whence, where, which, zcompile, zformat, zle, zmodload, zparseopts, zstyle, ztcp

  -- Zsh-specific Commands
  highlight(0, 'zshZleCommand',        { fg = colors.orange,     bg = 'NONE'            })  -- zle (Zsh Line Editor)
  highlight(0, 'zshZmodload',          { fg = colors.orange,     bg = 'NONE'            })  -- zmodload
  highlight(0, 'zshAutoload',          { fg = colors.orange,     bg = 'NONE'            })  -- autoload
  highlight(0, 'zshZstyle',            { fg = colors.orange,     bg = 'NONE'            })  -- zstyle
  highlight(0, 'zshZparseopts',        { fg = colors.orange,     bg = 'NONE'            })  -- zparseopts
  highlight(0, 'zshCompctl',           { fg = colors.orange,     bg = 'NONE'            })  -- compctl (completion control)
  highlight(0, 'zshCompadd',           { fg = colors.orange,     bg = 'NONE'            })  -- compadd
  highlight(0, 'zshCompdef',           { fg = colors.orange,     bg = 'NONE'            })  -- compdef

  -- Options
  highlight(0, 'zshOptStart',          { fg = colors.blue,       bg = 'NONE'            })  -- setopt, unsetopt
  highlight(0, 'zshOption',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Shell options (auto_cd, autopushd, extended_glob, etc.)

  -- Variables - Definition
  highlight(0, 'zshVariableDef',       { link = "Variable" })  -- Variable names in assignments
  highlight(0, 'zshVariable',          { link = "Variable" })  -- Variable names

  -- Variables - Dereferencing
  highlight(0, 'zshDeref',             { link = "Variable" })  -- $variable
  highlight(0, 'zshDereferencing',     { link = "Variable" })  -- Variable dereferencing
  highlight(0, 'zshShortDeref',        { link = "Variable" })  -- $v short dereference
  highlight(0, 'zshLongDeref',         { link = "Variable" })  -- ${variable}
  highlight(0, 'zshDollarVar',         { link = "Variable" })  -- $variable

  -- Variables - Substitution
  highlight(0, 'zshSubst',             { link = "Variable" })  -- Parameter substitution
  highlight(0, 'zshSubstQuoted',       { link = "Variable" })  -- Quoted substitution
  highlight(0, 'zshSubstDelim',        { link = "Delimiter" })  -- ${} delimiters
  highlight(0, 'zshMathSubst',         { link = "Variable" })  -- $((...)) arithmetic
  highlight(0, 'zshOldSubst',          { link = "Variable" })  -- $[...] old arithmetic

  -- Variables - Special/Built-in
  highlight(0, 'zshSpecialVar',        { link = "Variable" })  -- Special variables ($?, $$, $!, etc.)
  highlight(0, 'zshBuiltinVar',        { link = "Variable" })  -- ZDOTDIR, ZSH_VERSION, HISTFILE, etc.
  highlight(0, 'zshHookFunc',          { link = "Function" })  -- precmd, preexec, chpwd, etc.

  -- Parameter Expansion Flags
  highlight(0, 'zshParamFlag',         { fg = colors.pink,       bg = 'NONE'            })  -- ${(flags)var}
  highlight(0, 'zshExpansionFlag',     { fg = colors.pink,       bg = 'NONE'            })  -- Expansion flags

  -- Strings
  highlight(0, 'zshString',            { link = "String" })  -- "strings"
  highlight(0, 'zshStringDelimiter',   { link = "Delimiter" })  -- Quotes
  highlight(0, 'zshQuoted',            { fg = colors.redLight,   bg = 'NONE'            })  -- Quoted text

  -- Here Documents
  highlight(0, 'zshHereDoc',           { fg = colors.redLight,   bg = 'NONE'            })  -- <<EOF heredocs

  -- Numbers
  highlight(0, 'zshNumber',            { link = "Number" })  -- Numbers

  -- Operators
  highlight(0, 'zshOperator',          { link = "Operator" })  -- Operators (=, ==, !=, &&, ||, etc.)
  highlight(0, 'zshRedir',             { fg = colors.pink,       bg = 'NONE'            })  -- Redirection (>, >>, <, <<, &>, etc.)
  highlight(0, 'zshPipe',              { fg = colors.white,      bg = 'NONE'            })  -- | pipe
  highlight(0, 'zshTestOp',            { fg = colors.blue,       bg = 'NONE'            })  -- Test operators (-eq, -ne, -f, -d, etc.)

  -- Glob Qualifiers (Zsh unique)
  highlight(0, 'zshGlob',              { fg = colors.pink,       bg = 'NONE'            })  -- Glob patterns
  highlight(0, 'zshGlobQual',          { fg = colors.pink,       bg = 'NONE'            })  -- Glob qualifiers (.), (/), (@), etc.
  highlight(0, 'zshExtendedGlob',      { fg = colors.pink,       bg = 'NONE'            })  -- Extended glob (#, ~, ^, etc.)

  -- Brackets/Delimiters
  highlight(0, 'zshDelim',             { link = "Delimiter" })  -- Delimiters
  highlight(0, 'zshBraces',            { fg = colors.white,      bg = 'NONE'            })  -- { }
  highlight(0, 'zshBrackets',          { fg = colors.white,      bg = 'NONE'            })  -- [ ]
  highlight(0, 'zshParens',            { fg = colors.white,      bg = 'NONE'            })  -- ( )
  highlight(0, 'zshDblBracket',        { fg = colors.blue,       bg = 'NONE'            })  -- [[ ]]
  highlight(0, 'zshDblParen',          { fg = colors.white,      bg = 'NONE'            })  -- (( ))

  -- Command Substitution
  highlight(0, 'zshCmdSubst',          { link = "Variable" })  -- $(command)
  highlight(0, 'zshBacktick',          { fg = colors.purple,     bg = 'NONE'            })  -- `command`

  -- Process Substitution
  highlight(0, 'zshProcSubst',         { link = "Variable" })  -- <(cmd) or >(cmd)

  -- Flags/Switches
  highlight(0, 'zshFlag',              { fg = colors.blue,       bg = 'NONE'            })  -- -flag options
  highlight(0, 'zshSwitches',          { fg = colors.blue,       bg = 'NONE'            })  -- Command switches

  -- History Expansion
  highlight(0, 'zshHistory',           { fg = colors.pink,       bg = 'NONE'            })  -- !!, !$, !*, etc.
  highlight(0, 'zshHistoryMod',        { fg = colors.pink,       bg = 'NONE'            })  -- History modifiers :h, :t, :r, etc.

  -- Prompt Escapes
  highlight(0, 'zshPrompt',            { fg = colors.pink,       bg = 'NONE'            })  -- Prompt escape sequences %~, %n, etc.

  -- Comments
  highlight(0, 'zshComment',           { link = "Comment" })  -- # comments
  highlight(0, 'zshTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'zshError',             { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.zsh)
  -- Note: Zsh uses bash treesitter parser, so most highlights come from @xxx.bash
  -- These are additional zsh-specific captures if a dedicated parser is used

  -- Variables
  highlight(0, '@variable.zsh',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.zsh',      { link = "Variable" })  -- ZDOTDIR, ZSH_VERSION, etc.
  highlight(0, '@variable.parameter.zsh',    { link = "Variable" })  -- Function parameters

  -- Constants
  highlight(0, '@constant.zsh',              { link = "Constant" })  -- UPPERCASE variables
  highlight(0, '@constant.builtin.zsh',      { link = "Constant" })  -- Built-in constants

  -- Functions
  highlight(0, '@function.zsh',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.zsh',         { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.zsh',      { link = "Function" })  -- Built-in commands

  -- Keywords
  highlight(0, '@keyword.zsh',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.function.zsh',      { link = "Keyword" })  -- function
  highlight(0, '@keyword.operator.zsh',      { link = "Operator" })  -- Operators
  highlight(0, '@keyword.return.zsh',        { link = "Keyword" })  -- return, exit
  highlight(0, '@keyword.repeat.zsh',        { link = "Keyword" })  -- for, while, until, repeat
  highlight(0, '@keyword.conditional.zsh',   { link = "Conditional" })  -- if, then, else, elif, fi, case, esac
  highlight(0, '@keyword.import.zsh',        { link = "Keyword" })  -- source, autoload
  highlight(0, '@keyword.directive.zsh',     { link = "Keyword" })  -- setopt, unsetopt

  -- Strings
  highlight(0, '@string.zsh',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.zsh',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.regexp.zsh',         { link = "String" })  -- Regex patterns
  highlight(0, '@string.special.zsh',        { link = "String" })  -- Special strings

  -- Numbers
  highlight(0, '@number.zsh',                { link = "Number" })  -- Numbers

  -- Booleans
  highlight(0, '@boolean.zsh',               { link = "Boolean" })  -- true, false

  -- Comments
  highlight(0, '@comment.zsh',               { link = "Comment" })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.zsh',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.zsh',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, [[]]
  highlight(0, '@punctuation.delimiter.zsh', { link = "Delimiter" })  -- ; ;;
  highlight(0, '@punctuation.special.zsh',   { fg = colors.pink,      bg = 'NONE' })  -- $ in expansions


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.zsh)

  highlight(0, '@lsp.type.variable.zsh',      { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.zsh',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.function.zsh',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.keyword.zsh',       { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.zsh',      { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.zsh',        { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.zsh',        { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.zsh',       { link = "Comment" })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.zsh',    { link = "Variable" })  -- Built-in variables
  highlight(0, '@lsp.typemod.function.declaration.zsh', { fg = colors.orange, bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.zsh', { fg = colors.orange, bg = 'NONE' })  -- Built-in commands
end

return zsh

