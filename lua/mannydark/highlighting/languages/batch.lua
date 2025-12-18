-------------------------------------------------------------------------------
-- Windows Batch/CMD Files
-- Highlighting for .bat, .cmd files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local batch   = {}


-------------------------------------------------------------------------------
-- Settings

batch.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - dosbatch*

  -- Keywords - Control Flow
  highlight(0, 'dosbatchStatement',     { fg = colors.blue,       bg = 'NONE'            })  -- goto, call, exit
  highlight(0, 'dosbatchConditional',   { fg = colors.blue,       bg = 'NONE'            })  -- if, else
  highlight(0, 'dosbatchRepeat',        { fg = colors.blue,       bg = 'NONE'            })  -- for, do, in
  highlight(0, 'dosbatchKeyword',       { fg = colors.blue,       bg = 'NONE'            })  -- General keywords

  -- If Statement Operators
  highlight(0, 'dosbatchIfOperator',    { fg = colors.blue,       bg = 'NONE'            })  -- exist, defined, errorlevel, not, /i
  highlight(0, 'dosbatchOperator',      { fg = colors.blue,       bg = 'NONE'            })  -- EQU, NEQ, LSS, LEQ, GTR, GEQ, ==

  -- Built-in Commands (Internal)
  highlight(0, 'dosbatchImplicit',      { fg = colors.orange,     bg = 'NONE'            })  -- dir, copy, echo, del, type, set, cd, cls, etc.
  highlight(0, 'dosbatchCommand',       { fg = colors.orange,     bg = 'NONE'            })  -- Commands
  highlight(0, 'dosbatchBuiltin',       { fg = colors.orange,     bg = 'NONE'            })  -- Built-in commands

  -- Echo Command
  highlight(0, 'dosbatchEcho',          { fg = colors.orange,     bg = 'NONE'            })  -- echo
  highlight(0, 'dosbatchEchoOperator',  { fg = colors.white,      bg = 'NONE'            })  -- echo. echo:

  -- Set Command
  highlight(0, 'dosbatchSet',           { fg = colors.orange,     bg = 'NONE'            })  -- set
  highlight(0, 'dosbatchSetOperator',   { fg = colors.white,      bg = 'NONE'            })  -- = in set statements

  -- Specific Internal Commands
  highlight(0, 'dosbatchAssoc',         { fg = colors.orange,     bg = 'NONE'            })  -- assoc
  highlight(0, 'dosbatchAttrib',        { fg = colors.orange,     bg = 'NONE'            })  -- attrib
  highlight(0, 'dosbatchBreak',         { fg = colors.orange,     bg = 'NONE'            })  -- break
  highlight(0, 'dosbatchCall',          { fg = colors.blue,       bg = 'NONE'            })  -- call
  highlight(0, 'dosbatchCd',            { fg = colors.orange,     bg = 'NONE'            })  -- cd, chdir
  highlight(0, 'dosbatchCls',           { fg = colors.orange,     bg = 'NONE'            })  -- cls
  highlight(0, 'dosbatchColor',         { fg = colors.orange,     bg = 'NONE'            })  -- color
  highlight(0, 'dosbatchCopy',          { fg = colors.orange,     bg = 'NONE'            })  -- copy
  highlight(0, 'dosbatchDate',          { fg = colors.orange,     bg = 'NONE'            })  -- date
  highlight(0, 'dosbatchDel',           { fg = colors.orange,     bg = 'NONE'            })  -- del, erase
  highlight(0, 'dosbatchDir',           { fg = colors.orange,     bg = 'NONE'            })  -- dir
  highlight(0, 'dosbatchEndlocal',      { fg = colors.blue,       bg = 'NONE'            })  -- endlocal
  highlight(0, 'dosbatchExit',          { fg = colors.blue,       bg = 'NONE'            })  -- exit
  highlight(0, 'dosbatchFor',           { fg = colors.blue,       bg = 'NONE'            })  -- for
  highlight(0, 'dosbatchGoto',          { fg = colors.blue,       bg = 'NONE'            })  -- goto
  highlight(0, 'dosbatchIf',            { fg = colors.blue,       bg = 'NONE'            })  -- if
  highlight(0, 'dosbatchMd',            { fg = colors.orange,     bg = 'NONE'            })  -- md, mkdir
  highlight(0, 'dosbatchMklink',        { fg = colors.orange,     bg = 'NONE'            })  -- mklink
  highlight(0, 'dosbatchMove',          { fg = colors.orange,     bg = 'NONE'            })  -- move
  highlight(0, 'dosbatchPath',          { fg = colors.orange,     bg = 'NONE'            })  -- path
  highlight(0, 'dosbatchPause',         { fg = colors.orange,     bg = 'NONE'            })  -- pause
  highlight(0, 'dosbatchPopd',          { fg = colors.orange,     bg = 'NONE'            })  -- popd
  highlight(0, 'dosbatchPrompt',        { fg = colors.orange,     bg = 'NONE'            })  -- prompt
  highlight(0, 'dosbatchPushd',         { fg = colors.orange,     bg = 'NONE'            })  -- pushd
  highlight(0, 'dosbatchRd',            { fg = colors.orange,     bg = 'NONE'            })  -- rd, rmdir
  highlight(0, 'dosbatchRem',           { fg = colors.red,        bg = 'NONE'            })  -- rem (comment keyword)
  highlight(0, 'dosbatchRen',           { fg = colors.orange,     bg = 'NONE'            })  -- ren, rename
  highlight(0, 'dosbatchSetlocal',      { fg = colors.blue,       bg = 'NONE'            })  -- setlocal
  highlight(0, 'dosbatchShift',         { fg = colors.orange,     bg = 'NONE'            })  -- shift
  highlight(0, 'dosbatchStart',         { fg = colors.orange,     bg = 'NONE'            })  -- start
  highlight(0, 'dosbatchTime',          { fg = colors.orange,     bg = 'NONE'            })  -- time
  highlight(0, 'dosbatchTitle',         { fg = colors.orange,     bg = 'NONE'            })  -- title
  highlight(0, 'dosbatchType',          { fg = colors.orange,     bg = 'NONE'            })  -- type
  highlight(0, 'dosbatchVer',           { fg = colors.orange,     bg = 'NONE'            })  -- ver
  highlight(0, 'dosbatchVerify',        { fg = colors.orange,     bg = 'NONE'            })  -- verify
  highlight(0, 'dosbatchVol',           { fg = colors.orange,     bg = 'NONE'            })  -- vol

  -- Common External Commands
  highlight(0, 'dosbatchExternal',      { fg = colors.orange,     bg = 'NONE'            })  -- External commands (findstr, xcopy, robocopy, etc.)

  -- Variables
  highlight(0, 'dosbatchVariable',      { fg = colors.purple,     bg = 'NONE'            })  -- %variable%, !variable!
  highlight(0, 'dosbatchIdentifier',    { fg = colors.purple,     bg = 'NONE'            })  -- Variable names
  highlight(0, 'dosbatchEnvironment',   { fg = colors.purple,     bg = 'NONE'            })  -- Environment variables

  -- Special/Built-in Variables
  highlight(0, 'dosbatchSpecialVar',    { fg = colors.pink,       bg = 'NONE'            })  -- %ERRORLEVEL%, %CD%, %DATE%, %TIME%, %RANDOM%
  highlight(0, 'dosbatchBuiltinVar',    { fg = colors.pink,       bg = 'NONE'            })  -- %CMDCMDLINE%, %CMDEXTVERSION%, %PATHEXT%, %COMSPEC%

  -- Arguments/Parameters
  highlight(0, 'dosbatchArgument',      { fg = colors.pink,       bg = 'NONE'            })  -- %0-%9, %*, %%i (for loop)
  highlight(0, 'dosbatchParameter',     { fg = colors.pink,       bg = 'NONE'            })  -- Parameter variables
  highlight(0, 'dosbatchParamMod',      { fg = colors.pink,       bg = 'NONE'            })  -- %~f1, %~d1, %~p1, %~n1, %~x1, etc.

  -- Labels
  highlight(0, 'dosbatchLabel',         { fg = colors.turquoise,  bg = 'NONE'            })  -- :label, :EOF
  highlight(0, 'dosbatchLabelRegion',   { fg = colors.turquoise,  bg = 'NONE'            })  -- Label regions

  -- Strings
  highlight(0, 'dosbatchString',        { fg = colors.redLight,   bg = 'NONE'            })  -- "quoted strings"
  highlight(0, 'dosbatchQuote',         { fg = colors.redLight,   bg = 'NONE'            })  -- Quote characters

  -- Numbers
  highlight(0, 'dosbatchNumber',        { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'dosbatchInteger',       { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'dosbatchHex',           { fg = colors.greenLight, bg = 'NONE'            })  -- 0x... hexadecimal
  highlight(0, 'dosbatchOctal',         { fg = colors.greenLight, bg = 'NONE'            })  -- 0... octal
  highlight(0, 'dosbatchBinary',        { fg = colors.greenLight, bg = 'NONE'            })  -- Binary numbers

  -- Operators
  highlight(0, 'dosbatchArithOp',       { fg = colors.white,      bg = 'NONE'            })  -- +, -, *, /, %% (SET /A)
  highlight(0, 'dosbatchCompareOp',     { fg = colors.blue,       bg = 'NONE'            })  -- EQU, NEQ, LSS, LEQ, GTR, GEQ
  highlight(0, 'dosbatchLogicalOp',     { fg = colors.white,      bg = 'NONE'            })  -- &&, ||, &
  highlight(0, 'dosbatchAssignOp',      { fg = colors.white,      bg = 'NONE'            })  -- =

  -- Redirection
  highlight(0, 'dosbatchRedirection',   { fg = colors.pink,       bg = 'NONE'            })  -- >, >>, <, 2>, 2>>, 2>&1
  highlight(0, 'dosbatchRedir',         { fg = colors.pink,       bg = 'NONE'            })  -- Redirection operators

  -- Pipe
  highlight(0, 'dosbatchPipe',          { fg = colors.white,      bg = 'NONE'            })  -- |

  -- Switches/Flags
  highlight(0, 'dosbatchSwitch',        { fg = colors.blue,       bg = 'NONE'            })  -- /a, /p, /f, /r, /d, /l, etc.
  highlight(0, 'dosbatchOption',        { fg = colors.blue,       bg = 'NONE'            })  -- Command options

  -- Code Blocks
  highlight(0, 'dosbatchCodeBlock',     { fg = colors.white,      bg = 'NONE'            })  -- ( ... ) parenthesized blocks
  highlight(0, 'dosbatchParens',        { fg = colors.white,      bg = 'NONE'            })  -- Parentheses

  -- Special Characters
  highlight(0, 'dosbatchSpecialChar',   { fg = colors.pink,       bg = 'NONE'            })  -- ^, %%, !, @
  highlight(0, 'dosbatchEscape',        { fg = colors.pink,       bg = 'NONE'            })  -- ^ escape character
  highlight(0, 'dosbatchDelayedVar',    { fg = colors.purple,     bg = 'NONE'            })  -- !variable! delayed expansion

  -- @ Symbol (Echo Off)
  highlight(0, 'dosbatchAtSign',        { fg = colors.pink,       bg = 'NONE'            })  -- @ (suppress echo)

  -- Comments
  highlight(0, 'dosbatchRemComment',    { fg = colors.red,        bg = 'NONE'            })  -- REM comment
  highlight(0, 'dosbatchColonComment',  { fg = colors.red,        bg = 'NONE'            })  -- :: comment
  highlight(0, 'dosbatchComment',       { fg = colors.red,        bg = 'NONE'            })  -- General comments
  highlight(0, 'dosbatchTodo',          { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Error
  highlight(0, 'dosbatchError',         { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.dosbatch / @xxx.batch)
  -- Note: Batch may use different parser names

  -- Variables
  highlight(0, '@variable.dosbatch',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.batch',                 { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.dosbatch',      { fg = colors.pink,      bg = 'NONE' })  -- ERRORLEVEL, CD, DATE, TIME, etc.
  highlight(0, '@variable.builtin.batch',         { fg = colors.pink,      bg = 'NONE' })  -- Built-in variables
  highlight(0, '@variable.parameter.dosbatch',    { fg = colors.pink,      bg = 'NONE' })  -- %1-%9, %%i
  highlight(0, '@variable.parameter.batch',       { fg = colors.pink,      bg = 'NONE' })  -- Parameters

  -- Constants
  highlight(0, '@constant.dosbatch',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.batch',                 { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.dosbatch',      { fg = colors.pink,      bg = 'NONE' })  -- Built-in constants
  highlight(0, '@constant.builtin.batch',         { fg = colors.pink,      bg = 'NONE' })  -- Built-in constants

  -- Functions/Commands
  highlight(0, '@function.dosbatch',              { fg = colors.orange,    bg = 'NONE' })  -- Commands
  highlight(0, '@function.batch',                 { fg = colors.orange,    bg = 'NONE' })  -- Commands
  highlight(0, '@function.call.dosbatch',         { fg = colors.orange,    bg = 'NONE' })  -- Command calls
  highlight(0, '@function.call.batch',            { fg = colors.orange,    bg = 'NONE' })  -- Command calls
  highlight(0, '@function.builtin.dosbatch',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in commands
  highlight(0, '@function.builtin.batch',         { fg = colors.orange,    bg = 'NONE' })  -- Built-in commands

  -- Labels
  highlight(0, '@label.dosbatch',                 { fg = colors.turquoise, bg = 'NONE' })  -- :label
  highlight(0, '@label.batch',                    { fg = colors.turquoise, bg = 'NONE' })  -- :label

  -- Keywords
  highlight(0, '@keyword.dosbatch',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.batch',                  { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.dosbatch',      { fg = colors.blue,      bg = 'NONE' })  -- call
  highlight(0, '@keyword.function.batch',         { fg = colors.blue,      bg = 'NONE' })  -- call
  highlight(0, '@keyword.operator.dosbatch',      { fg = colors.blue,      bg = 'NONE' })  -- EQU, NEQ, LSS, etc.
  highlight(0, '@keyword.operator.batch',         { fg = colors.blue,      bg = 'NONE' })  -- Comparison operators
  highlight(0, '@keyword.return.dosbatch',        { fg = colors.blue,      bg = 'NONE' })  -- exit
  highlight(0, '@keyword.return.batch',           { fg = colors.blue,      bg = 'NONE' })  -- exit
  highlight(0, '@keyword.repeat.dosbatch',        { fg = colors.blue,      bg = 'NONE' })  -- for, do
  highlight(0, '@keyword.repeat.batch',           { fg = colors.blue,      bg = 'NONE' })  -- for, do
  highlight(0, '@keyword.conditional.dosbatch',   { fg = colors.blue,      bg = 'NONE' })  -- if, else
  highlight(0, '@keyword.conditional.batch',      { fg = colors.blue,      bg = 'NONE' })  -- if, else
  highlight(0, '@keyword.directive.dosbatch',     { fg = colors.pink,      bg = 'NONE' })  -- @echo off
  highlight(0, '@keyword.directive.batch',        { fg = colors.pink,      bg = 'NONE' })  -- @echo off

  -- Strings
  highlight(0, '@string.dosbatch',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.batch',                   { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.dosbatch',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.escape.batch',            { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.dosbatch',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.batch',                   { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- Comments
  highlight(0, '@comment.dosbatch',               { fg = colors.red,       bg = 'NONE' })  -- REM, ::
  highlight(0, '@comment.batch',                  { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- Operators and Punctuation
  highlight(0, '@operator.dosbatch',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@operator.batch',                 { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.dosbatch',   { fg = colors.white,     bg = 'NONE' })  -- (), []
  highlight(0, '@punctuation.bracket.batch',      { fg = colors.white,     bg = 'NONE' })  -- Brackets
  highlight(0, '@punctuation.delimiter.dosbatch', { fg = colors.white,     bg = 'NONE' })  -- Delimiters
  highlight(0, '@punctuation.delimiter.batch',    { fg = colors.white,     bg = 'NONE' })  -- Delimiters
  highlight(0, '@punctuation.special.dosbatch',   { fg = colors.pink,      bg = 'NONE' })  -- %, !, @, ^
  highlight(0, '@punctuation.special.batch',      { fg = colors.pink,      bg = 'NONE' })  -- Special punctuation


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.dosbatch / @lsp.type.xxx.bat)

  highlight(0, '@lsp.type.variable.dosbatch',     { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.variable.bat',          { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.dosbatch',    { fg = colors.pink,      bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.parameter.bat',         { fg = colors.pink,      bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.function.dosbatch',     { fg = colors.orange,    bg = 'NONE' })  -- Commands
  highlight(0, '@lsp.type.function.bat',          { fg = colors.orange,    bg = 'NONE' })  -- Commands
  highlight(0, '@lsp.type.keyword.dosbatch',      { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.keyword.bat',           { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.dosbatch',     { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.operator.bat',          { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.dosbatch',       { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.string.bat',            { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.dosbatch',       { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.number.bat',            { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.dosbatch',      { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.comment.bat',           { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.label.dosbatch',        { fg = colors.turquoise, bg = 'NONE' })  -- Labels
  highlight(0, '@lsp.type.label.bat',             { fg = colors.turquoise, bg = 'NONE' })  -- Labels

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.dosbatch',    { fg = colors.pink,   bg = 'NONE' })  -- Built-in variables
  highlight(0, '@lsp.typemod.variable.readonly.bat',         { fg = colors.pink,   bg = 'NONE' })  -- Built-in variables
  highlight(0, '@lsp.typemod.function.declaration.dosbatch', { fg = colors.orange, bg = 'NONE' })  -- Label declarations
  highlight(0, '@lsp.typemod.function.declaration.bat',      { fg = colors.orange, bg = 'NONE' })  -- Label declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.dosbatch', { fg = colors.orange, bg = 'NONE' })  -- Built-in commands
  highlight(0, '@lsp.typemod.function.defaultLibrary.bat',      { fg = colors.orange, bg = 'NONE' })  -- Built-in commands
end

return batch


