-------------------------------------------------------------------------------
-- Vim Script Language
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local vimscript = {}


-------------------------------------------------------------------------------
-- Settings

vimscript.setupHighlighting = function()

  ---------------------------------------------------------------
  -- Vim Legacy Syntax Groups - Commands
  ---------------------------------------------------------------

  -- General Commands
  highlight(0, 'vimCommand',                    { fg = colors.blue,       bg = 'NONE' })           -- General commands
  highlight(0, 'vimStatement',                  { fg = colors.blue,       bg = 'NONE' })           -- Statements
  highlight(0, 'vimIsCommand',                  { fg = colors.blue,       bg = 'NONE' })           -- Is a command
  highlight(0, 'vimExtCmd',                     { fg = colors.blue,       bg = 'NONE' })           -- External commands
  highlight(0, 'vimNotFunc',                    { fg = colors.blue,       bg = 'NONE' })           -- Not a function

  -- Abbreviations
  highlight(0, 'vimAbb',                        { fg = colors.blue,       bg = 'NONE' })           -- Abbreviation commands
  highlight(0, 'vimAbbrev',                     { fg = colors.blue,       bg = 'NONE' })           -- Abbreviation

  -- Address & Range
  highlight(0, 'vimAddress',                    { fg = colors.purple,     bg = 'NONE' })           -- Address specifications
  highlight(0, 'vimMark',                       { fg = colors.purple,     bg = 'NONE' })           -- Marks

  -- Autocmd & Augroup
  highlight(0, 'vimAutoCmd',                    { fg = colors.blue,       bg = 'NONE' })           -- autocmd
  highlight(0, 'vimAutoCmdOpt',                 { fg = colors.turquoise,  bg = 'NONE' })           -- autocmd options
  highlight(0, 'vimAutoCmdSfxList',             { fg = colors.turquoise,  bg = 'NONE' })           -- autocmd suffix list
  highlight(0, 'vimAutoEvent',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Auto events
  highlight(0, 'vimAutoEventList',              { fg = colors.turquoise,  bg = 'NONE' })           -- Event list
  highlight(0, 'vimAugroup',                    { fg = colors.blue,       bg = 'NONE' })           -- augroup
  highlight(0, 'vimAugroupKey',                 { fg = colors.blue,       bg = 'NONE' })           -- augroup keyword
  highlight(0, 'vimAugroupBang',                { fg = colors.blue,       bg = 'NONE' })           -- augroup!
  highlight(0, 'vimAutoCmdGroup',               { fg = colors.turquoise,  bg = 'NONE' })           -- Autocmd group name
  highlight(0, 'vimAugroupError',               { fg = colors.red,        bg = 'NONE' })           -- Augroup errors
  highlight(0, 'vimAuHighlight',                { fg = colors.blue,       bg = 'NONE' })           -- Highlight in autocmd

  -- Behave
  highlight(0, 'vimBehave',                     { fg = colors.blue,       bg = 'NONE' })           -- behave command
  highlight(0, 'vimBehaveModel',                { fg = colors.turquoise,  bg = 'NONE' })           -- mswin, xterm

  -- Call & Execute
  highlight(0, 'vimCall',                       { fg = colors.blue,       bg = 'NONE' })           -- call command
  highlight(0, 'vimExecute',                    { fg = colors.blue,       bg = 'NONE' })           -- execute command

  -- Echo Commands
  highlight(0, 'vimEcho',                       { fg = colors.blue,       bg = 'NONE' })           -- echo
  highlight(0, 'vimEchoHL',                     { fg = colors.blue,       bg = 'NONE' })           -- echohl
  highlight(0, 'vimEchohl',                     { fg = colors.blue,       bg = 'NONE' })           -- echohl
  highlight(0, 'vimEchomsg',                    { fg = colors.blue,       bg = 'NONE' })           -- echomsg
  highlight(0, 'vimEchoerr',                    { fg = colors.blue,       bg = 'NONE' })           -- echoerr
  highlight(0, 'vimEchon',                      { fg = colors.blue,       bg = 'NONE' })           -- echon

  -- Function Definition
  highlight(0, 'vimFunction',                   { fg = colors.blue,       bg = 'NONE' })           -- function keyword
  highlight(0, 'vimFunctionError',              { fg = colors.red,        bg = 'NONE' })           -- Function errors
  highlight(0, 'vimFuncKey',                    { fg = colors.blue,       bg = 'NONE' })           -- function keyword
  highlight(0, 'vimFuncName',                   { fg = colors.orange,     bg = 'NONE' })           -- Function name
  highlight(0, 'vimFuncSID',                    { fg = colors.orange,     bg = 'NONE' })           -- <SID> prefix
  highlight(0, 'vimFuncVar',                    { fg = colors.purple,     bg = 'NONE' })           -- Function variable
  highlight(0, 'vimFuncParam',                  { fg = colors.purple,     bg = 'NONE' })           -- Function parameter
  highlight(0, 'vimFuncParamName',              { fg = colors.purple,     bg = 'NONE' })           -- Parameter name
  highlight(0, 'vimFuncBody',                   { fg = colors.white,      bg = 'NONE' })           -- Function body
  highlight(0, 'vimFuncFold',                   { fg = colors.blue,       bg = 'NONE' })           -- Folded function
  highlight(0, 'vimEndfunction',                { fg = colors.blue,       bg = 'NONE' })           -- endfunction
  highlight(0, 'vimUserFunc',                   { fg = colors.orange,     bg = 'NONE' })           -- User function
  highlight(0, 'vimFunc',                       { fg = colors.orange,     bg = 'NONE' })           -- Function call
  highlight(0, 'vimFuncBlank',                  { fg = colors.white,      bg = 'NONE' })           -- Function blank

  -- Def (Vim9)
  highlight(0, 'vimDef',                        { fg = colors.blue,       bg = 'NONE' })           -- def keyword
  highlight(0, 'vimDefFold',                    { fg = colors.blue,       bg = 'NONE' })           -- Folded def
  highlight(0, 'vimDefKey',                     { fg = colors.blue,       bg = 'NONE' })           -- def keyword
  highlight(0, 'vimDefName',                    { fg = colors.orange,     bg = 'NONE' })           -- def name
  highlight(0, 'vimDefParam',                   { fg = colors.purple,     bg = 'NONE' })           -- def parameter
  highlight(0, 'vimEnddef',                     { fg = colors.blue,       bg = 'NONE' })           -- enddef

  -- Global Command
  highlight(0, 'vimGlobal',                     { fg = colors.blue,       bg = 'NONE' })           -- :g :global

  -- Highlight Command
  highlight(0, 'vimHighlight',                  { fg = colors.blue,       bg = 'NONE' })           -- highlight command
  highlight(0, 'vimHiAttrib',                   { fg = colors.turquoise,  bg = 'NONE' })           -- Highlight attributes
  highlight(0, 'vimHiBang',                     { fg = colors.blue,       bg = 'NONE' })           -- highlight!
  highlight(0, 'vimHiClear',                    { fg = colors.blue,       bg = 'NONE' })           -- highlight clear
  highlight(0, 'vimHiCtermColor',               { fg = colors.greenLight, bg = 'NONE' })           -- cterm color
  highlight(0, 'vimHiCtermFgBg',                { fg = colors.turquoise,  bg = 'NONE' })           -- ctermfg/ctermbg
  highlight(0, 'vimHiFontname',                 { fg = colors.redLight,   bg = 'NONE' })           -- Font name
  highlight(0, 'vimHiGroup',                    { fg = colors.turquoise,  bg = 'NONE' })           -- Highlight group
  highlight(0, 'vimHiGui',                      { fg = colors.turquoise,  bg = 'NONE' })           -- gui=
  highlight(0, 'vimHiGuiFgBg',                  { fg = colors.turquoise,  bg = 'NONE' })           -- guifg/guibg
  highlight(0, 'vimHiGuiFont',                  { fg = colors.turquoise,  bg = 'NONE' })           -- guifont
  highlight(0, 'vimHiGuiRgb',                   { fg = colors.greenLight, bg = 'NONE' })           -- GUI RGB color
  highlight(0, 'vimHiKeyError',                 { fg = colors.red,        bg = 'NONE' })           -- Key error
  highlight(0, 'vimHiKeyList',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Key list
  highlight(0, 'vimHiLink',                     { fg = colors.blue,       bg = 'NONE' })           -- link
  highlight(0, 'vimHiNmbr',                     { fg = colors.greenLight, bg = 'NONE' })           -- Number
  highlight(0, 'vimHiStartStop',                { fg = colors.turquoise,  bg = 'NONE' })           -- start/stop
  highlight(0, 'vimHiTerm',                     { fg = colors.turquoise,  bg = 'NONE' })           -- term=
  highlight(0, 'vimHiTermcap',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Termcap
  highlight(0, 'vimHLGroup',                    { fg = colors.turquoise,  bg = 'NONE' })           -- Highlight group name
  highlight(0, 'vimGroup',                      { fg = colors.turquoise,  bg = 'NONE' })           -- Group name
  highlight(0, 'vimGroupName',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Group name
  highlight(0, 'vimGroupSpecial',               { fg = colors.turquoise,  bg = 'NONE' })           -- Special groups

  -- Let & Const
  highlight(0, 'vimLet',                        { fg = colors.blue,       bg = 'NONE' })           -- let command
  highlight(0, 'vimLetVar',                     { fg = colors.white,      bg = 'NONE' })           -- Variable in let
  highlight(0, 'vimLetHereDoc',                 { fg = colors.redLight,   bg = 'NONE' })           -- Here document
  highlight(0, 'vimLetHereDocStart',            { fg = colors.blue,       bg = 'NONE' })           -- Here doc start
  highlight(0, 'vimLetHereDocStop',             { fg = colors.blue,       bg = 'NONE' })           -- Here doc stop
  highlight(0, 'vimConst',                      { fg = colors.blue,       bg = 'NONE' })           -- const
  highlight(0, 'vimUnlet',                      { fg = colors.blue,       bg = 'NONE' })           -- unlet

  -- Mapping Commands
  highlight(0, 'vimMap',                        { fg = colors.blue,       bg = 'NONE' })           -- map command
  highlight(0, 'vimMapBang',                    { fg = colors.blue,       bg = 'NONE' })           -- map!
  highlight(0, 'vimMapLhs',                     { fg = colors.white,      bg = 'NONE' })           -- Left-hand side
  highlight(0, 'vimMapMod',                     { fg = colors.turquoise,  bg = 'NONE' })           -- Map modifiers
  highlight(0, 'vimMapModErr',                  { fg = colors.red,        bg = 'NONE' })           -- Modifier error
  highlight(0, 'vimMapModKey',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Modifier key
  highlight(0, 'vimMapRhs',                     { fg = colors.white,      bg = 'NONE' })           -- Right-hand side
  highlight(0, 'vimMapRhsExtend',               { fg = colors.white,      bg = 'NONE' })           -- Extended RHS
  highlight(0, 'vimUnmap',                      { fg = colors.blue,       bg = 'NONE' })           -- unmap
  highlight(0, 'vimMapClear',                   { fg = colors.blue,       bg = 'NONE' })           -- mapclear

  -- Match Command
  highlight(0, 'vimMatch',                      { fg = colors.blue,       bg = 'NONE' })           -- match command
  highlight(0, 'vim2Match',                     { fg = colors.blue,       bg = 'NONE' })           -- 2match
  highlight(0, 'vim3Match',                     { fg = colors.blue,       bg = 'NONE' })           -- 3match
  highlight(0, 'vimMatchGroup',                 { fg = colors.turquoise,  bg = 'NONE' })           -- Match group

  -- Menu Command
  highlight(0, 'vimMenu',                       { fg = colors.blue,       bg = 'NONE' })           -- menu command
  highlight(0, 'vimMenuBang',                   { fg = colors.blue,       bg = 'NONE' })           -- menu!
  highlight(0, 'vimMenuMap',                    { fg = colors.white,      bg = 'NONE' })           -- Menu mapping
  highlight(0, 'vimMenuMod',                    { fg = colors.turquoise,  bg = 'NONE' })           -- Menu modifier
  highlight(0, 'vimMenuName',                   { fg = colors.white,      bg = 'NONE' })           -- Menu name
  highlight(0, 'vimMenuNameMore',               { fg = colors.white,      bg = 'NONE' })           -- Extended name
  highlight(0, 'vimMenuPriority',               { fg = colors.greenLight, bg = 'NONE' })           -- Priority
  highlight(0, 'vimMenutranslate',              { fg = colors.blue,       bg = 'NONE' })           -- menutranslate

  -- Normal Command
  highlight(0, 'vimNormal',                     { fg = colors.blue,       bg = 'NONE' })           -- normal command
  highlight(0, 'vimNormalCmd',                  { fg = colors.blue,       bg = 'NONE' })           -- normal cmd
  highlight(0, 'vimNorm',                       { fg = colors.blue,       bg = 'NONE' })           -- norm

  -- Set Command
  highlight(0, 'vimSet',                        { fg = colors.blue,       bg = 'NONE' })           -- set command
  highlight(0, 'vimSetEqual',                   { fg = colors.white,      bg = 'NONE' })           -- = in set
  highlight(0, 'vimSetMod',                     { fg = colors.turquoise,  bg = 'NONE' })           -- Set modifier
  highlight(0, 'vimSetSep',                     { fg = colors.white,      bg = 'NONE' })           -- Separator
  highlight(0, 'vimSetString',                  { fg = colors.redLight,   bg = 'NONE' })           -- String value

  -- Sleep Command
  highlight(0, 'vimSleep',                      { fg = colors.blue,       bg = 'NONE' })           -- sleep command

  -- Source Command
  highlight(0, 'vimSource',                     { fg = colors.blue,       bg = 'NONE' })           -- source command

  -- Syntax Command
  highlight(0, 'vimSyntax',                     { fg = colors.blue,       bg = 'NONE' })           -- syntax command
  highlight(0, 'vimSynCase',                    { fg = colors.blue,       bg = 'NONE' })           -- syntax case
  highlight(0, 'vimSynCaseError',               { fg = colors.red,        bg = 'NONE' })           -- Case error
  highlight(0, 'vimSynContains',                { fg = colors.turquoise,  bg = 'NONE' })           -- contains=
  highlight(0, 'vimSynError',                   { fg = colors.red,        bg = 'NONE' })           -- Syntax error
  highlight(0, 'vimSynKeyContainedin',          { fg = colors.turquoise,  bg = 'NONE' })           -- containedin=
  highlight(0, 'vimSynKeyOpt',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Keyword option
  highlight(0, 'vimSynKeyword',                 { fg = colors.blue,       bg = 'NONE' })           -- syntax keyword
  highlight(0, 'vimSynMatch',                   { fg = colors.blue,       bg = 'NONE' })           -- syntax match
  highlight(0, 'vimSynMatchOpt',                { fg = colors.turquoise,  bg = 'NONE' })           -- Match option
  highlight(0, 'vimSynMtchCchar',               { fg = colors.turquoise,  bg = 'NONE' })           -- Conceal char
  highlight(0, 'vimSynMtchGroup',               { fg = colors.turquoise,  bg = 'NONE' })           -- Match group
  highlight(0, 'vimSynMtchGrp',                 { fg = colors.turquoise,  bg = 'NONE' })           -- Match grp
  highlight(0, 'vimSynMtchOpt',                 { fg = colors.turquoise,  bg = 'NONE' })           -- Match opt
  highlight(0, 'vimSynNextgroup',               { fg = colors.turquoise,  bg = 'NONE' })           -- nextgroup=
  highlight(0, 'vimSynNotPatRange',             { fg = colors.redLight,   bg = 'NONE' })           -- Not pattern range
  highlight(0, 'vimSynOption',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Syntax option
  highlight(0, 'vimSynPatRange',                { fg = colors.redLight,   bg = 'NONE' })           -- Pattern range
  highlight(0, 'vimSynRegion',                  { fg = colors.blue,       bg = 'NONE' })           -- syntax region
  highlight(0, 'vimSynRegOpt',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Region option
  highlight(0, 'vimSynRegPat',                  { fg = colors.redLight,   bg = 'NONE' })           -- Region pattern
  highlight(0, 'vimSynReg',                     { fg = colors.blue,       bg = 'NONE' })           -- syn region
  highlight(0, 'vimSynType',                    { fg = colors.blue,       bg = 'NONE' })           -- Syntax type
  highlight(0, 'vimSyncC',                      { fg = colors.blue,       bg = 'NONE' })           -- sync
  highlight(0, 'vimSyncError',                  { fg = colors.red,        bg = 'NONE' })           -- Sync error
  highlight(0, 'vimSyncGroup',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Sync group
  highlight(0, 'vimSyncGroupName',              { fg = colors.turquoise,  bg = 'NONE' })           -- Sync group name
  highlight(0, 'vimSyncKey',                    { fg = colors.turquoise,  bg = 'NONE' })           -- Sync key
  highlight(0, 'vimSyncLinecont',               { fg = colors.turquoise,  bg = 'NONE' })           -- Linecont
  highlight(0, 'vimSyncLines',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Lines
  highlight(0, 'vimSyncMatch',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Sync match
  highlight(0, 'vimSyncNone',                   { fg = colors.turquoise,  bg = 'NONE' })           -- Sync none
  highlight(0, 'vimSyncRegion',                 { fg = colors.turquoise,  bg = 'NONE' })           -- Sync region
  highlight(0, 'vimSynLine',                    { fg = colors.white,      bg = 'NONE' })           -- Syntax line

  -- Try/Catch/Throw
  highlight(0, 'vimTry',                        { fg = colors.blue,       bg = 'NONE' })           -- try
  highlight(0, 'vimCatch',                      { fg = colors.blue,       bg = 'NONE' })           -- catch
  highlight(0, 'vimFinally',                    { fg = colors.blue,       bg = 'NONE' })           -- finally
  highlight(0, 'vimEndtry',                     { fg = colors.blue,       bg = 'NONE' })           -- endtry
  highlight(0, 'vimThrow',                      { fg = colors.blue,       bg = 'NONE' })           -- throw

  -- User Command
  highlight(0, 'vimUserCmd',                    { fg = colors.blue,       bg = 'NONE' })           -- User command
  highlight(0, 'vimUserCommand',                { fg = colors.blue,       bg = 'NONE' })           -- User command
  highlight(0, 'vimUserCmdError',               { fg = colors.red,        bg = 'NONE' })           -- Command error
  highlight(0, 'vimDelcommand',                 { fg = colors.blue,       bg = 'NONE' })           -- delcommand

  -- Other Commands
  highlight(0, 'vimFilter',                     { fg = colors.blue,       bg = 'NONE' })           -- filter command
  highlight(0, 'vimLoadkeymap',                 { fg = colors.blue,       bg = 'NONE' })           -- loadkeymap
  highlight(0, 'vimCmplxRepeat',                { fg = colors.blue,       bg = 'NONE' })           -- Complex repeat

  ---------------------------------------------------------------
  -- Variables & Scope
  ---------------------------------------------------------------

  highlight(0, 'vimVar',                        { fg = colors.white,      bg = 'NONE' })           -- Variable
  highlight(0, 'vimVarScope',                   { fg = colors.purple,     bg = 'NONE' })           -- Scope prefix (g:, b:, etc.)
  highlight(0, 'vimVimVar',                     { fg = colors.purple,     bg = 'NONE' })           -- v: variables
  highlight(0, 'vimFBVar',                      { fg = colors.purple,     bg = 'NONE' })           -- Function body var
  highlight(0, 'vimEnvvar',                     { fg = colors.purple,     bg = 'NONE' })           -- $ENVVAR
  highlight(0, 'vimLetRegister',                { fg = colors.purple,     bg = 'NONE' })           -- Register in let

  ---------------------------------------------------------------
  -- Options
  ---------------------------------------------------------------

  highlight(0, 'vimOption',                     { fg = colors.turquoise,  bg = 'NONE' })           -- Vim options
  highlight(0, 'vimOptionVar',                  { fg = colors.turquoise,  bg = 'NONE' })           -- &option
  highlight(0, 'vimOptionVarName',              { fg = colors.turquoise,  bg = 'NONE' })           -- Option name

  ---------------------------------------------------------------
  -- Control Flow
  ---------------------------------------------------------------

  highlight(0, 'vimIf',                         { fg = colors.blue,       bg = 'NONE' })           -- if
  highlight(0, 'vimElse',                       { fg = colors.blue,       bg = 'NONE' })           -- else
  highlight(0, 'vimElseif',                     { fg = colors.blue,       bg = 'NONE' })           -- elseif
  highlight(0, 'vimEndif',                      { fg = colors.blue,       bg = 'NONE' })           -- endif
  highlight(0, 'vimWhile',                      { fg = colors.blue,       bg = 'NONE' })           -- while
  highlight(0, 'vimEndwhile',                   { fg = colors.blue,       bg = 'NONE' })           -- endwhile
  highlight(0, 'vimFor',                        { fg = colors.blue,       bg = 'NONE' })           -- for
  highlight(0, 'vimEndfor',                     { fg = colors.blue,       bg = 'NONE' })           -- endfor
  highlight(0, 'vimReturn',                     { fg = colors.blue,       bg = 'NONE' })           -- return
  highlight(0, 'vimBreak',                      { fg = colors.blue,       bg = 'NONE' })           -- break
  highlight(0, 'vimContinue',                   { fg = colors.blue,       bg = 'NONE' })           -- continue

  ---------------------------------------------------------------
  -- Strings & Patterns
  ---------------------------------------------------------------

  highlight(0, 'vimString',                     { fg = colors.redLight,   bg = 'NONE' })           -- Strings
  highlight(0, 'vimStringCont',                 { fg = colors.redLight,   bg = 'NONE' })           -- Continued string
  highlight(0, 'vimStringEnd',                  { fg = colors.redLight,   bg = 'NONE' })           -- String end
  highlight(0, 'vimEscape',                     { fg = colors.purple,     bg = 'NONE' })           -- Escape sequences
  highlight(0, 'vimEscapeBrace',                { fg = colors.purple,     bg = 'NONE' })           -- Escaped brace
  highlight(0, 'vimPatSep',                     { fg = colors.blue,       bg = 'NONE' })           -- Pattern separator
  highlight(0, 'vimPattern',                    { fg = colors.redLight,   bg = 'NONE' })           -- Pattern
  highlight(0, 'vimPatSepErr',                  { fg = colors.red,        bg = 'NONE' })           -- Pattern error
  highlight(0, 'vimPatSepR',                    { fg = colors.blue,       bg = 'NONE' })           -- Pattern sep R
  highlight(0, 'vimPatSepZ',                    { fg = colors.blue,       bg = 'NONE' })           -- Pattern sep Z
  highlight(0, 'vimPatSepZone',                 { fg = colors.blue,       bg = 'NONE' })           -- Pattern sep zone
  highlight(0, 'vimSubst',                      { fg = colors.blue,       bg = 'NONE' })           -- Substitution
  highlight(0, 'vimSubstDelim',                 { fg = colors.blue,       bg = 'NONE' })           -- Subst delimiter
  highlight(0, 'vimSubstPat',                   { fg = colors.redLight,   bg = 'NONE' })           -- Subst pattern
  highlight(0, 'vimSubstRange',                 { fg = colors.redLight,   bg = 'NONE' })           -- Subst range
  highlight(0, 'vimSubstRep',                   { fg = colors.redLight,   bg = 'NONE' })           -- Subst replacement
  highlight(0, 'vimSubstRep4',                  { fg = colors.redLight,   bg = 'NONE' })           -- Subst rep 4
  highlight(0, 'vimSubstFlags',                 { fg = colors.turquoise,  bg = 'NONE' })           -- Subst flags
  highlight(0, 'vimSubstSubstr',                { fg = colors.purple,     bg = 'NONE' })           -- Subst substring
  highlight(0, 'vimSubstTwoBS',                 { fg = colors.purple,     bg = 'NONE' })           -- Subst two backslash
  highlight(0, 'vimCollection',                 { fg = colors.redLight,   bg = 'NONE' })           -- Collection

  ---------------------------------------------------------------
  -- Numbers
  ---------------------------------------------------------------

  highlight(0, 'vimNumber',                     { fg = colors.greenLight, bg = 'NONE' })           -- Numbers
  highlight(0, 'vimFloat',                      { fg = colors.greenLight, bg = 'NONE' })           -- Float
  highlight(0, 'vimHexNumber',                  { fg = colors.greenLight, bg = 'NONE' })           -- Hex number
  highlight(0, 'vimOctNumber',                  { fg = colors.greenLight, bg = 'NONE' })           -- Octal number
  highlight(0, 'vimBinNumber',                  { fg = colors.greenLight, bg = 'NONE' })           -- Binary number

  ---------------------------------------------------------------
  -- Operators & Punctuation
  ---------------------------------------------------------------

  highlight(0, 'vimOper',                       { fg = colors.white,      bg = 'NONE' })           -- Operators
  highlight(0, 'vimOperParen',                  { fg = colors.white,      bg = 'NONE' })           -- Operator paren
  highlight(0, 'vimOperError',                  { fg = colors.red,        bg = 'NONE' })           -- Operator error
  highlight(0, 'vimSep',                        { fg = colors.white,      bg = 'NONE' })           -- Separator
  highlight(0, 'vimParenSep',                   { fg = colors.white,      bg = 'NONE' })           -- Paren separator
  highlight(0, 'vimBracket',                    { fg = colors.white,      bg = 'NONE' })           -- Brackets
  highlight(0, 'vimCmdSep',                     { fg = colors.white,      bg = 'NONE' })           -- Command separator

  ---------------------------------------------------------------
  -- Registers
  ---------------------------------------------------------------

  highlight(0, 'vimRegister',                   { fg = colors.purple,     bg = 'NONE' })           -- Registers (@x)

  ---------------------------------------------------------------
  -- Comments
  ---------------------------------------------------------------

  highlight(0, 'vimComment',                    { fg = colors.red,        bg = 'NONE' })           -- " comments
  highlight(0, 'vimLineComment',                { fg = colors.red,        bg = 'NONE' })           -- Line comment
  highlight(0, 'vimCommentString',              { fg = colors.red,        bg = 'NONE' })           -- String in comment
  highlight(0, 'vimCommentTitle',               { fg = colors.red,        bg = 'NONE', bold = true })  -- Comment title
  highlight(0, 'vimTodo',                       { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO/FIXME
  highlight(0, 'vimScriptDelim',                { fg = colors.red,        bg = 'NONE', bold = true })  -- Script delimiter

  ---------------------------------------------------------------
  -- Special Notation
  ---------------------------------------------------------------

  highlight(0, 'vimNotation',                   { fg = colors.turquoise,  bg = 'NONE' })           -- <CR>, <Esc>, etc.
  highlight(0, 'vimBracketNotation',            { fg = colors.turquoise,  bg = 'NONE' })           -- Bracket notation
  highlight(0, 'vimCtrlChar',                   { fg = colors.turquoise,  bg = 'NONE' })           -- Control chars
  highlight(0, 'vimNotFunc',                    { fg = colors.blue,       bg = 'NONE' })           -- Not function
  highlight(0, 'vimSpecFile',                   { fg = colors.turquoise,  bg = 'NONE' })           -- Special file
  highlight(0, 'vimSpecFileMod',                { fg = colors.turquoise,  bg = 'NONE' })           -- File modifier

  ---------------------------------------------------------------
  -- Errors
  ---------------------------------------------------------------

  highlight(0, 'vimError',                      { fg = colors.red,        bg = 'NONE' })           -- Errors
  highlight(0, 'vimBufnrWarn',                  { fg = colors.yellow,     bg = 'NONE' })           -- Buffer warnings
  highlight(0, 'vimKeyCodeError',               { fg = colors.red,        bg = 'NONE' })           -- Keycode error
  highlight(0, 'vimWarn',                       { fg = colors.yellow,     bg = 'NONE' })           -- Warnings
  highlight(0, 'vimErrSetting',                 { fg = colors.red,        bg = 'NONE' })           -- Setting error

  ---------------------------------------------------------------
  -- Line Continuation
  ---------------------------------------------------------------

  highlight(0, 'vimContinueComment',            { fg = colors.red,        bg = 'NONE' })           -- Continued comment
  highlight(0, 'vimLineContinue',               { fg = colors.blue,       bg = 'NONE' })           -- Line continuation \

  ---------------------------------------------------------------
  -- Vim9 Script Specific
  ---------------------------------------------------------------

  highlight(0, 'vim9Script',                    { fg = colors.blue,       bg = 'NONE' })           -- vim9script
  highlight(0, 'vim9ScriptArg',                 { fg = colors.turquoise,  bg = 'NONE' })           -- noclear
  highlight(0, 'vim9Cmd',                       { fg = colors.blue,       bg = 'NONE' })           -- vim9cmd
  highlight(0, 'vim9Comment',                   { fg = colors.red,        bg = 'NONE' })           -- # comments
  highlight(0, 'vim9LineComment',               { fg = colors.red,        bg = 'NONE' })           -- # line comments
  highlight(0, 'vim9Block',                     { fg = colors.white,      bg = 'NONE' })           -- Code block
  highlight(0, 'vim9Continue',                  { fg = colors.blue,       bg = 'NONE' })           -- Continuation
  highlight(0, 'vim9Type',                      { fg = colors.turquoise,  bg = 'NONE' })           -- Type annotations
  highlight(0, 'vim9Import',                    { fg = colors.blue,       bg = 'NONE' })           -- import
  highlight(0, 'vim9Export',                    { fg = colors.blue,       bg = 'NONE' })           -- export
  highlight(0, 'vim9Autoload',                  { fg = colors.blue,       bg = 'NONE' })           -- autoload
  highlight(0, 'vim9As',                        { fg = colors.blue,       bg = 'NONE' })           -- as
  highlight(0, 'vim9From',                      { fg = colors.blue,       bg = 'NONE' })           -- from

  ---------------------------------------------------------------
  -- Folding
  ---------------------------------------------------------------

  highlight(0, 'vimFold',                       { fg = colors.red,        bg = 'NONE' })           -- Fold markers
  highlight(0, 'vimFoldCol',                    { fg = colors.grey,       bg = 'NONE' })           -- Fold column
  highlight(0, 'vimFoldErr',                    { fg = colors.red,        bg = 'NONE' })           -- Fold error

  ---------------------------------------------------------------
  -- Miscellaneous
  ---------------------------------------------------------------

  highlight(0, 'vimRegion',                     { fg = colors.white,      bg = 'NONE' })           -- Region
  highlight(0, 'vimStdPlugin',                  { fg = colors.turquoise,  bg = 'NONE' })           -- Standard plugin
  highlight(0, 'vimSearch',                     { fg = colors.redLight,   bg = 'NONE' })           -- Search pattern
  highlight(0, 'vimSearchDelim',                { fg = colors.blue,       bg = 'NONE' })           -- Search delimiter
  highlight(0, 'vimSynSpell',                   { fg = colors.redLight,   bg = 'NONE' })           -- Spell check
  highlight(0, 'vimInsert',                     { fg = colors.white,      bg = 'NONE' })           -- Insert text
  highlight(0, 'vimFiletype',                   { fg = colors.turquoise,  bg = 'NONE' })           -- Filetype
  highlight(0, 'vimAugroupSyncA',               { fg = colors.turquoise,  bg = 'NONE' })           -- Augroup sync

  ---------------------------------------------------------------
  -- Treesitter Captures (@xxx.vim)
  ---------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.vim',                         { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@variable.builtin.vim',                 { fg = colors.purple,     bg = 'NONE' })       -- v:, g:, etc.
  highlight(0, '@variable.parameter.vim',               { fg = colors.purple,     bg = 'NONE' })       -- Function params

  -- Constants
  highlight(0, '@constant.vim',                         { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@constant.builtin.vim',                 { fg = colors.purple,     bg = 'NONE' })       -- v:true, v:false

  -- Numbers
  highlight(0, '@number.vim',                           { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@number.float.vim',                     { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@boolean.vim',                          { fg = colors.purple,     bg = 'NONE' })       -- true, false

  -- Strings
  highlight(0, '@string.vim',                           { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@string.special.vim',                   { fg = colors.purple,     bg = 'NONE' })       -- Special strings
  highlight(0, '@string.special.path.vim',              { fg = colors.redLight,   bg = 'NONE' })       -- File paths
  highlight(0, '@string.regexp.vim',                    { fg = colors.redLight,   bg = 'NONE' })       -- Regex patterns
  highlight(0, '@string.escape.vim',                    { fg = colors.purple,     bg = 'NONE' })       -- Escape sequences

  -- Functions
  highlight(0, '@function.vim',                         { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.call.vim',                    { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@function.builtin.vim',                 { fg = colors.orange,     bg = 'NONE' })       -- Built-in functions
  highlight(0, '@function.macro.vim',                   { fg = colors.orange,     bg = 'NONE' })       -- Macros

  -- Keywords
  highlight(0, '@keyword.vim',                          { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.function.vim',                 { fg = colors.blue,       bg = 'NONE' })       -- function, def
  highlight(0, '@keyword.return.vim',                   { fg = colors.blue,       bg = 'NONE' })       -- return
  highlight(0, '@keyword.conditional.vim',              { fg = colors.blue,       bg = 'NONE' })       -- if, else, elseif
  highlight(0, '@keyword.conditional.ternary.vim',      { fg = colors.white,      bg = 'NONE' })       -- ?:
  highlight(0, '@keyword.repeat.vim',                   { fg = colors.blue,       bg = 'NONE' })       -- for, while
  highlight(0, '@keyword.exception.vim',                { fg = colors.blue,       bg = 'NONE' })       -- try, catch, throw
  highlight(0, '@keyword.operator.vim',                 { fg = colors.blue,       bg = 'NONE' })       -- is, isnot
  highlight(0, '@keyword.import.vim',                   { fg = colors.blue,       bg = 'NONE' })       -- import, source

  -- Types
  highlight(0, '@type.vim',                             { fg = colors.turquoise,  bg = 'NONE' })       -- Type annotations
  highlight(0, '@type.builtin.vim',                     { fg = colors.turquoise,  bg = 'NONE' })       -- Built-in types

  -- Modules
  highlight(0, '@module.vim',                           { fg = colors.turquoise,  bg = 'NONE' })       -- Module names

  -- Properties
  highlight(0, '@property.vim',                         { fg = colors.turquoise,  bg = 'NONE' })       -- Properties

  -- Operators
  highlight(0, '@operator.vim',                         { fg = colors.white,      bg = 'NONE' })

  -- Punctuation
  highlight(0, '@punctuation.bracket.vim',              { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.delimiter.vim',            { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.special.vim',              { fg = colors.blue,       bg = 'NONE' })

  -- Labels
  highlight(0, '@label.vim',                            { fg = colors.yellow,     bg = 'NONE' })       -- Labels

  -- Comments
  highlight(0, '@comment.vim',                          { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@comment.documentation.vim',            { fg = colors.red,        bg = 'NONE' })
  highlight(0, '@spell.vim',                            { fg = 'NONE',            bg = 'NONE' })

  -- Character Special
  highlight(0, '@character.special.vim',                { fg = colors.blue,       bg = 'NONE' })       -- <CR>, <Esc>

  ---------------------------------------------------------------
  -- LSP Semantic Tokens
  ---------------------------------------------------------------

  highlight(0, '@lsp.type.variable.vim',                { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.function.vim',                { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@lsp.type.keyword.vim',                 { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@lsp.type.string.vim',                  { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@lsp.type.number.vim',                  { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@lsp.type.operator.vim',                { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@lsp.type.comment.vim',                 { fg = colors.red,        bg = 'NONE' })

  ---------------------------------------------------------------
  -- Built-in Functions Categories
  ---------------------------------------------------------------

  -- Buffer/Window Functions
  highlight(0, 'vimFuncBufnr',                  { fg = colors.orange,     bg = 'NONE' })           -- bufnr()
  highlight(0, 'vimFuncWinnr',                  { fg = colors.orange,     bg = 'NONE' })           -- winnr()
  highlight(0, 'vimFuncTabpagenr',              { fg = colors.orange,     bg = 'NONE' })           -- tabpagenr()
  highlight(0, 'vimFuncBufname',                { fg = colors.orange,     bg = 'NONE' })           -- bufname()
  highlight(0, 'vimFuncBufexists',              { fg = colors.orange,     bg = 'NONE' })           -- bufexists()
  highlight(0, 'vimFuncBufloaded',              { fg = colors.orange,     bg = 'NONE' })           -- bufloaded()
  highlight(0, 'vimFuncBufwinnr',               { fg = colors.orange,     bg = 'NONE' })           -- bufwinnr()
  highlight(0, 'vimFuncGetbufline',             { fg = colors.orange,     bg = 'NONE' })           -- getbufline()
  highlight(0, 'vimFuncSetbufline',             { fg = colors.orange,     bg = 'NONE' })           -- setbufline()

  -- String Functions
  highlight(0, 'vimFuncStrlen',                 { fg = colors.orange,     bg = 'NONE' })           -- strlen()
  highlight(0, 'vimFuncSubstitute',             { fg = colors.orange,     bg = 'NONE' })           -- substitute()
  highlight(0, 'vimFuncMatchstr',               { fg = colors.orange,     bg = 'NONE' })           -- matchstr()
  highlight(0, 'vimFuncSplit',                  { fg = colors.orange,     bg = 'NONE' })           -- split()
  highlight(0, 'vimFuncJoin',                   { fg = colors.orange,     bg = 'NONE' })           -- join()
  highlight(0, 'vimFuncTolower',                { fg = colors.orange,     bg = 'NONE' })           -- tolower()
  highlight(0, 'vimFuncToupper',                { fg = colors.orange,     bg = 'NONE' })           -- toupper()
  highlight(0, 'vimFuncTrim',                   { fg = colors.orange,     bg = 'NONE' })           -- trim()
  highlight(0, 'vimFuncEscape',                 { fg = colors.orange,     bg = 'NONE' })           -- escape()
  highlight(0, 'vimFuncShellescape',            { fg = colors.orange,     bg = 'NONE' })           -- shellescape()
  highlight(0, 'vimFuncFnameescape',            { fg = colors.orange,     bg = 'NONE' })           -- fnameescape()
  highlight(0, 'vimFuncPrintf',                 { fg = colors.orange,     bg = 'NONE' })           -- printf()

  -- List Functions
  highlight(0, 'vimFuncLen',                    { fg = colors.orange,     bg = 'NONE' })           -- len()
  highlight(0, 'vimFuncEmpty',                  { fg = colors.orange,     bg = 'NONE' })           -- empty()
  highlight(0, 'vimFuncRange',                  { fg = colors.orange,     bg = 'NONE' })           -- range()
  highlight(0, 'vimFuncAdd',                    { fg = colors.orange,     bg = 'NONE' })           -- add()
  highlight(0, 'vimFuncRemove',                 { fg = colors.orange,     bg = 'NONE' })           -- remove()
  highlight(0, 'vimFuncFilter',                 { fg = colors.orange,     bg = 'NONE' })           -- filter()
  highlight(0, 'vimFuncMap',                    { fg = colors.orange,     bg = 'NONE' })           -- map()
  highlight(0, 'vimFuncSort',                   { fg = colors.orange,     bg = 'NONE' })           -- sort()
  highlight(0, 'vimFuncReverse',                { fg = colors.orange,     bg = 'NONE' })           -- reverse()
  highlight(0, 'vimFuncUniq',                   { fg = colors.orange,     bg = 'NONE' })           -- uniq()
  highlight(0, 'vimFuncIndex',                  { fg = colors.orange,     bg = 'NONE' })           -- index()
  highlight(0, 'vimFuncCount',                  { fg = colors.orange,     bg = 'NONE' })           -- count()
  highlight(0, 'vimFuncExtend',                 { fg = colors.orange,     bg = 'NONE' })           -- extend()
  highlight(0, 'vimFuncCopy',                   { fg = colors.orange,     bg = 'NONE' })           -- copy()
  highlight(0, 'vimFuncDeepcopy',               { fg = colors.orange,     bg = 'NONE' })           -- deepcopy()

  -- Dictionary Functions
  highlight(0, 'vimFuncHas_key',                { fg = colors.orange,     bg = 'NONE' })           -- has_key()
  highlight(0, 'vimFuncKeys',                   { fg = colors.orange,     bg = 'NONE' })           -- keys()
  highlight(0, 'vimFuncValues',                 { fg = colors.orange,     bg = 'NONE' })           -- values()
  highlight(0, 'vimFuncItems',                  { fg = colors.orange,     bg = 'NONE' })           -- items()
  highlight(0, 'vimFuncGet',                    { fg = colors.orange,     bg = 'NONE' })           -- get()

  -- File Functions
  highlight(0, 'vimFuncFilereadable',           { fg = colors.orange,     bg = 'NONE' })           -- filereadable()
  highlight(0, 'vimFuncFilewritable',           { fg = colors.orange,     bg = 'NONE' })           -- filewritable()
  highlight(0, 'vimFuncIsdirectory',            { fg = colors.orange,     bg = 'NONE' })           -- isdirectory()
  highlight(0, 'vimFuncFnamemodify',            { fg = colors.orange,     bg = 'NONE' })           -- fnamemodify()
  highlight(0, 'vimFuncGlob',                   { fg = colors.orange,     bg = 'NONE' })           -- glob()
  highlight(0, 'vimFuncGlobpath',               { fg = colors.orange,     bg = 'NONE' })           -- globpath()
  highlight(0, 'vimFuncReadfile',               { fg = colors.orange,     bg = 'NONE' })           -- readfile()
  highlight(0, 'vimFuncWritefile',              { fg = colors.orange,     bg = 'NONE' })           -- writefile()
  highlight(0, 'vimFuncExpand',                 { fg = colors.orange,     bg = 'NONE' })           -- expand()
  highlight(0, 'vimFuncResolve',                { fg = colors.orange,     bg = 'NONE' })           -- resolve()

  -- Type Functions
  highlight(0, 'vimFuncType',                   { fg = colors.orange,     bg = 'NONE' })           -- type()
  highlight(0, 'vimFuncTypename',               { fg = colors.orange,     bg = 'NONE' })           -- typename()
  highlight(0, 'vimFuncString',                 { fg = colors.orange,     bg = 'NONE' })           -- string()
  highlight(0, 'vimFuncStr2nr',                 { fg = colors.orange,     bg = 'NONE' })           -- str2nr()
  highlight(0, 'vimFuncStr2float',              { fg = colors.orange,     bg = 'NONE' })           -- str2float()
  highlight(0, 'vimFuncFloat2nr',               { fg = colors.orange,     bg = 'NONE' })           -- float2nr()

  -- System Functions
  highlight(0, 'vimFuncSystem',                 { fg = colors.orange,     bg = 'NONE' })           -- system()
  highlight(0, 'vimFuncSystemlist',             { fg = colors.orange,     bg = 'NONE' })           -- systemlist()
  highlight(0, 'vimFuncExecute',                { fg = colors.orange,     bg = 'NONE' })           -- execute()

  -- Feature Functions
  highlight(0, 'vimFuncHas',                    { fg = colors.orange,     bg = 'NONE' })           -- has()
  highlight(0, 'vimFuncExists',                 { fg = colors.orange,     bg = 'NONE' })           -- exists()

  -- Cursor/Position Functions
  highlight(0, 'vimFuncLine',                   { fg = colors.orange,     bg = 'NONE' })           -- line()
  highlight(0, 'vimFuncCol',                    { fg = colors.orange,     bg = 'NONE' })           -- col()
  highlight(0, 'vimFuncCursor',                 { fg = colors.orange,     bg = 'NONE' })           -- cursor()
  highlight(0, 'vimFuncGetpos',                 { fg = colors.orange,     bg = 'NONE' })           -- getpos()
  highlight(0, 'vimFuncSetpos',                 { fg = colors.orange,     bg = 'NONE' })           -- setpos()
  highlight(0, 'vimFuncGetcurpos',              { fg = colors.orange,     bg = 'NONE' })           -- getcurpos()
  highlight(0, 'vimFuncSearchpos',              { fg = colors.orange,     bg = 'NONE' })           -- searchpos()

  -- Text Functions
  highlight(0, 'vimFuncGetline',                { fg = colors.orange,     bg = 'NONE' })           -- getline()
  highlight(0, 'vimFuncSetline',                { fg = colors.orange,     bg = 'NONE' })           -- setline()
  highlight(0, 'vimFuncAppend',                 { fg = colors.orange,     bg = 'NONE' })           -- append()
  highlight(0, 'vimFuncNextnonblank',           { fg = colors.orange,     bg = 'NONE' })           -- nextnonblank()
  highlight(0, 'vimFuncPrevnonblank',           { fg = colors.orange,     bg = 'NONE' })           -- prevnonblank()
  highlight(0, 'vimFuncIndent',                 { fg = colors.orange,     bg = 'NONE' })           -- indent()

  -- Input Functions
  highlight(0, 'vimFuncInput',                  { fg = colors.orange,     bg = 'NONE' })           -- input()
  highlight(0, 'vimFuncInputlist',              { fg = colors.orange,     bg = 'NONE' })           -- inputlist()
  highlight(0, 'vimFuncConfirm',                { fg = colors.orange,     bg = 'NONE' })           -- confirm()
  highlight(0, 'vimFuncGetchar',                { fg = colors.orange,     bg = 'NONE' })           -- getchar()
  highlight(0, 'vimFuncGetcharmod',             { fg = colors.orange,     bg = 'NONE' })           -- getcharmod()

  -- Window/Tab Functions
  highlight(0, 'vimFuncWinwidth',               { fg = colors.orange,     bg = 'NONE' })           -- winwidth()
  highlight(0, 'vimFuncWinheight',              { fg = colors.orange,     bg = 'NONE' })           -- winheight()
  highlight(0, 'vimFuncGetwininfo',             { fg = colors.orange,     bg = 'NONE' })           -- getwininfo()
  highlight(0, 'vimFuncGettabinfo',             { fg = colors.orange,     bg = 'NONE' })           -- gettabinfo()

  -- Syntax/Highlight Functions
  highlight(0, 'vimFuncSynID',                  { fg = colors.orange,     bg = 'NONE' })           -- synID()
  highlight(0, 'vimFuncSynIDattr',              { fg = colors.orange,     bg = 'NONE' })           -- synIDattr()
  highlight(0, 'vimFuncSynIDtrans',             { fg = colors.orange,     bg = 'NONE' })           -- synIDtrans()
  highlight(0, 'vimFuncHlID',                   { fg = colors.orange,     bg = 'NONE' })           -- hlID()
  highlight(0, 'vimFuncHlexists',               { fg = colors.orange,     bg = 'NONE' })           -- hlexists()

  -- Timer Functions
  highlight(0, 'vimFuncTimer_start',            { fg = colors.orange,     bg = 'NONE' })           -- timer_start()
  highlight(0, 'vimFuncTimer_stop',             { fg = colors.orange,     bg = 'NONE' })           -- timer_stop()

  ---------------------------------------------------------------
  -- Plugin Support
  ---------------------------------------------------------------

  -- nvim-cmp
  highlight(0, 'CmpItemKindVim',                { fg = colors.green,      bg = 'NONE' })

end

return vimscript
