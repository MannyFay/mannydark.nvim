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
  highlight(0, 'zshConditional',       { fg = colors.blue,       bg = 'NONE'            })  -- if, then, elif, else, fi, esac, select
  highlight(0, 'zshRepeat',            { fg = colors.blue,       bg = 'NONE'            })  -- while, until, repeat, for, foreach
  highlight(0, 'zshDelimiter',         { fg = colors.blue,       bg = 'NONE'            })  -- do, done, end
  highlight(0, 'zshException',         { fg = colors.blue,       bg = 'NONE'            })  -- always

  -- Keywords - Case
  highlight(0, 'zshCase',              { fg = colors.blue,       bg = 'NONE'            })  -- case
  highlight(0, 'zshCaseWord',          { fg = colors.blue,       bg = 'NONE'            })  -- case word
  highlight(0, 'zshCaseIn',            { fg = colors.blue,       bg = 'NONE'            })  -- in
  highlight(0, 'zshCasePattern',       { fg = colors.redLight,   bg = 'NONE'            })  -- Case patterns

  -- Keywords - Function
  highlight(0, 'zshKeyword',           { fg = colors.blue,       bg = 'NONE'            })  -- function
  highlight(0, 'zshFunction',          { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'zshKSHFunction',       { fg = colors.orange,     bg = 'NONE'            })  -- Ksh-style function names

  -- Keywords - Precommand Modifiers
  highlight(0, 'zshPrecommand',        { fg = colors.blue,       bg = 'NONE'            })  -- noglob, nocorrect, exec, command, builtin, -, time

  -- Keywords - Types
  highlight(0, 'zshTypes',             { fg = colors.blue,       bg = 'NONE'            })  -- float, integer, local, typeset, declare, private, readonly

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
  highlight(0, 'zshVariableDef',       { fg = colors.purple,     bg = 'NONE'            })  -- Variable names in assignments
  highlight(0, 'zshVariable',          { fg = colors.purple,     bg = 'NONE'            })  -- Variable names

  -- Variables - Dereferencing
  highlight(0, 'zshDeref',             { fg = colors.purple,     bg = 'NONE'            })  -- $variable
  highlight(0, 'zshDereferencing',     { fg = colors.purple,     bg = 'NONE'            })  -- Variable dereferencing
  highlight(0, 'zshShortDeref',        { fg = colors.purple,     bg = 'NONE'            })  -- $v short dereference
  highlight(0, 'zshLongDeref',         { fg = colors.purple,     bg = 'NONE'            })  -- ${variable}
  highlight(0, 'zshDollarVar',         { fg = colors.purple,     bg = 'NONE'            })  -- $variable

  -- Variables - Substitution
  highlight(0, 'zshSubst',             { fg = colors.purple,     bg = 'NONE'            })  -- Parameter substitution
  highlight(0, 'zshSubstQuoted',       { fg = colors.purple,     bg = 'NONE'            })  -- Quoted substitution
  highlight(0, 'zshSubstDelim',        { fg = colors.white,      bg = 'NONE'            })  -- ${} delimiters
  highlight(0, 'zshMathSubst',         { fg = colors.greenLight, bg = 'NONE'            })  -- $((...)) arithmetic
  highlight(0, 'zshOldSubst',          { fg = colors.purple,     bg = 'NONE'            })  -- $[...] old arithmetic

  -- Variables - Special/Built-in
  highlight(0, 'zshSpecialVar',        { fg = colors.pink,       bg = 'NONE'            })  -- Special variables ($?, $$, $!, etc.)
  highlight(0, 'zshBuiltinVar',        { fg = colors.pink,       bg = 'NONE'            })  -- ZDOTDIR, ZSH_VERSION, HISTFILE, etc.
  highlight(0, 'zshHookFunc',          { fg = colors.pink,       bg = 'NONE'            })  -- precmd, preexec, chpwd, etc.

  -- Parameter Expansion Flags
  highlight(0, 'zshParamFlag',         { fg = colors.pink,       bg = 'NONE'            })  -- ${(flags)var}
  highlight(0, 'zshExpansionFlag',     { fg = colors.pink,       bg = 'NONE'            })  -- Expansion flags

  -- Strings
  highlight(0, 'zshString',            { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'zshStringDelimiter',   { fg = colors.redLight,   bg = 'NONE'            })  -- Quotes
  highlight(0, 'zshQuoted',            { fg = colors.redLight,   bg = 'NONE'            })  -- Quoted text

  -- Here Documents
  highlight(0, 'zshHereDoc',           { fg = colors.redLight,   bg = 'NONE'            })  -- <<EOF heredocs

  -- Numbers
  highlight(0, 'zshNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers

  -- Operators
  highlight(0, 'zshOperator',          { fg = colors.white,      bg = 'NONE'            })  -- Operators (=, ==, !=, &&, ||, etc.)
  highlight(0, 'zshRedir',             { fg = colors.pink,       bg = 'NONE'            })  -- Redirection (>, >>, <, <<, &>, etc.)
  highlight(0, 'zshPipe',              { fg = colors.white,      bg = 'NONE'            })  -- | pipe
  highlight(0, 'zshTestOp',            { fg = colors.blue,       bg = 'NONE'            })  -- Test operators (-eq, -ne, -f, -d, etc.)

  -- Glob Qualifiers (Zsh unique)
  highlight(0, 'zshGlob',              { fg = colors.pink,       bg = 'NONE'            })  -- Glob patterns
  highlight(0, 'zshGlobQual',          { fg = colors.pink,       bg = 'NONE'            })  -- Glob qualifiers (.), (/), (@), etc.
  highlight(0, 'zshExtendedGlob',      { fg = colors.pink,       bg = 'NONE'            })  -- Extended glob (#, ~, ^, etc.)

  -- Brackets/Delimiters
  highlight(0, 'zshDelim',             { fg = colors.white,      bg = 'NONE'            })  -- Delimiters
  highlight(0, 'zshBraces',            { fg = colors.white,      bg = 'NONE'            })  -- { }
  highlight(0, 'zshBrackets',          { fg = colors.white,      bg = 'NONE'            })  -- [ ]
  highlight(0, 'zshParens',            { fg = colors.white,      bg = 'NONE'            })  -- ( )
  highlight(0, 'zshDblBracket',        { fg = colors.blue,       bg = 'NONE'            })  -- [[ ]]
  highlight(0, 'zshDblParen',          { fg = colors.white,      bg = 'NONE'            })  -- (( ))

  -- Command Substitution
  highlight(0, 'zshCmdSubst',          { fg = colors.purple,     bg = 'NONE'            })  -- $(command)
  highlight(0, 'zshBacktick',          { fg = colors.purple,     bg = 'NONE'            })  -- `command`

  -- Process Substitution
  highlight(0, 'zshProcSubst',         { fg = colors.purple,     bg = 'NONE'            })  -- <(cmd) or >(cmd)

  -- Flags/Switches
  highlight(0, 'zshFlag',              { fg = colors.blue,       bg = 'NONE'            })  -- -flag options
  highlight(0, 'zshSwitches',          { fg = colors.blue,       bg = 'NONE'            })  -- Command switches

  -- History Expansion
  highlight(0, 'zshHistory',           { fg = colors.pink,       bg = 'NONE'            })  -- !!, !$, !*, etc.
  highlight(0, 'zshHistoryMod',        { fg = colors.pink,       bg = 'NONE'            })  -- History modifiers :h, :t, :r, etc.

  -- Prompt Escapes
  highlight(0, 'zshPrompt',            { fg = colors.pink,       bg = 'NONE'            })  -- Prompt escape sequences %~, %n, etc.

  -- Comments
  highlight(0, 'zshComment',           { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'zshTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'zshError',             { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.zsh)
  -- Note: Zsh uses bash treesitter parser, so most highlights come from @xxx.bash
  -- These are additional zsh-specific captures if a dedicated parser is used

  -- Variables
  highlight(0, '@variable.zsh',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.zsh',      { fg = colors.pink,      bg = 'NONE' })  -- ZDOTDIR, ZSH_VERSION, etc.
  highlight(0, '@variable.parameter.zsh',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters

  -- Constants
  highlight(0, '@constant.zsh',              { fg = colors.purple,    bg = 'NONE' })  -- UPPERCASE variables
  highlight(0, '@constant.builtin.zsh',      { fg = colors.pink,      bg = 'NONE' })  -- Built-in constants

  -- Functions
  highlight(0, '@function.zsh',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.zsh',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.zsh',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in commands

  -- Keywords
  highlight(0, '@keyword.zsh',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.zsh',      { fg = colors.blue,      bg = 'NONE' })  -- function
  highlight(0, '@keyword.operator.zsh',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@keyword.return.zsh',        { fg = colors.blue,      bg = 'NONE' })  -- return, exit
  highlight(0, '@keyword.repeat.zsh',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, until, repeat
  highlight(0, '@keyword.conditional.zsh',   { fg = colors.blue,      bg = 'NONE' })  -- if, then, else, elif, fi, case, esac
  highlight(0, '@keyword.import.zsh',        { fg = colors.pink,      bg = 'NONE' })  -- source, autoload
  highlight(0, '@keyword.directive.zsh',     { fg = colors.pink,      bg = 'NONE' })  -- setopt, unsetopt

  -- Strings
  highlight(0, '@string.zsh',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.zsh',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.regexp.zsh',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex patterns
  highlight(0, '@string.special.zsh',        { fg = colors.pink,      bg = 'NONE' })  -- Special strings

  -- Numbers
  highlight(0, '@number.zsh',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- Booleans
  highlight(0, '@boolean.zsh',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.zsh',               { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.zsh',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.zsh',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, [[]]
  highlight(0, '@punctuation.delimiter.zsh', { fg = colors.white,     bg = 'NONE' })  -- ; ;;
  highlight(0, '@punctuation.special.zsh',   { fg = colors.pink,      bg = 'NONE' })  -- $ in expansions


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.zsh)

  highlight(0, '@lsp.type.variable.zsh',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.zsh',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.function.zsh',      { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.keyword.zsh',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.zsh',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.zsh',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.zsh',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.zsh',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.zsh',    { fg = colors.pink,   bg = 'NONE' })  -- Built-in variables
  highlight(0, '@lsp.typemod.function.declaration.zsh', { fg = colors.orange, bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.zsh', { fg = colors.orange, bg = 'NONE' })  -- Built-in commands
end

return zsh

