-------------------------------------------------------------------------------
-- Shell
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local shell     = {}


--------------------------------------------------------------
-- Settings

shell.setupHighlighting = function()
  highlight(0, 'shDerefVar',      { fg = colors.purple,     bg = 'NONE' })  -- Variable call.
  highlight(0, 'shOption',        { fg = colors.blue,       bg = 'NONE' })  -- Option flags of commands.
  highlight(0, 'shComment',       { fg = colors.red,        bg = 'NONE' })  -- Comments.
  highlight(0, 'shRange',         { fg = colors.white,      bg = 'NONE' })  -- Delimiters like [].
  highlight(0, 'shExpr',          { fg = colors.purple,     bg = 'NONE' })  -- Array index variables.
  highlight(0, 'shOperator',      { fg = colors.white,      bg = 'NONE' })  -- Operators like =.
  highlight(0, 'shQuote',         { fg = colors.redLight,   bg = 'NONE' })  -- Quotes of strings.
  highlight(0, 'shSingleQuote',   { fg = colors.redLight,   bg = 'NONE' })  -- Strings.
  highlight(0, 'shVariable',      { fg = colors.purple,     bg = 'NONE' })  -- Name of Variables.
  highlight(0, 'shDeref',         { fg = colors.white,      bg = 'NONE' })  -- Brackets [] in variable call.
  highlight(0, 'shEscape',        { fg = colors.blue,       bg = 'NONE' })  -- Escape characters.
  highlight(0, 'shDerefVarArray', { fg = colors.purple,     bg = 'NONE' })  -- Reused array index variables.
  highlight(0, 'shFunctionKey',   { fg = colors.blue,       bg = 'NONE' })  -- Function keyword.
  highlight(0, 'shFunction',      { fg = colors.orange,     bg = 'NONE' })  -- Name of functions.
  highlight(0, 'shConditional',   { fg = colors.blue,       bg = 'NONE' })  -- Conditional statement keywords.
  highlight(0, 'shSemicolon',     { fg = colors.white,      bg = 'NONE' })  -- Semicolon.
  highlight(0, 'shDerefSimple',   { fg = colors.purple,     bg = 'NONE' })  -- Variable call without brackets.
  highlight(0, 'shTestOpr',       { fg = colors.blue,       bg = 'NONE' })  -- Test operators like -eq.
  highlight(0, 'shNumber',        { fg = colors.greenLight, bg = 'NONE' })  -- Numbers.
  highlight(0, 'shStatement',     { fg = colors.orange,     bg = 'NONE' })  -- Internal statement functions like echo.
  highlight(0, 'shShebang',       { fg = colors.white,      bg = 'NONE' })  -- Entry line (Shebang) in shell scripts.
  highlight(0, 'shCtrlSeq',       { fg = colors.blue,       bg = 'NONE' })  -- Control sequences like \n.
  highlight(0, 'shTodo',          { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO comments.

  ----------------------- Not used by now:
  -- highlight(0, 'shFunctionOne',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shIf',          { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCmdParenRegion',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCommandSub',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'bashSpecialVariables',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCmdSubRegion',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shFor',         { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSnglCase',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shVarAssign',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDoError',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shIfError',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shInError',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseError',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shEsacError',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCurlyError',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shParenError',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shTestError',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shOK',          { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shArithmetic',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shArithParen',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseEsac',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDo',          { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shEcho',        { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shPosnParm',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shExSingleQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shExDoubleQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereString',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shRedir',       { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDoubleQuote', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shAlias',       { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shTest',        { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecial',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shParen',       { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shFunctionTwo', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseStart',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseLabel',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCase',        { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseBar',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseIn',      { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseCommandSub',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseExSingleQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseSingleQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseDoubleQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shStringSpecial',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseRange',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shColon',       { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCommandSubBQ',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDblBrace',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSetList',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSource',      { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSubSh',       { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shComma',       { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefSpecial',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecialDQ',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefWordError',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefPSR',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefPPS',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefOffset', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefOp',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefOpError',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shEchoQuote',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCharClass',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shBeginHere',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHerePayload', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shWrapLineOperator',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSetOption',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shAtExpr',      { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDblParen',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSet',         { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shTouch',       { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecialNoZS', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shEchoDelim',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shQuickComment',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecialVar',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shEmbeddedEcho',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shTouchCmd',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shPattern',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shExprRegion',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecialNxt',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSubShRegion', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shNoQuote',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shString',      { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shAstQuote',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shTestDoubleQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shTestSingleQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shTestPattern', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shLoop',        { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCurlyIn',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shRepeat',      { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shBQComment',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecialStart',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecialSQ',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shBkslshSnglQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shBkslshDblQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc01',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc02',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc03',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc04',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc05',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc06',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc07',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc08',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc09',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc10',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc11',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc12',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc13',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc14',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc15',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc16',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shLoop',        { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCurlyIn',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shRepeat',      { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shBQComment',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecialStart',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSpecialSQ',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shBkslshSnglQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shBkslshDblQuote',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shTodo',        { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc01',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc02',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc03',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc04',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc05',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc06',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc07',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc08',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc09',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc10',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc11',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc12',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc13',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc14',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc15',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shHereDoc16',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shVar',         { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shSetListDelim',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shFunctionStart',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shFunctionThree',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shFunctionFour',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefPattern',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefString', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefEscape', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefDelim',  { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefLen',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefPPSleft',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefPPSright', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefPSRleft',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefPSRright',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shArithRegion', { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCondError',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shCaseEsacSync',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDoSync',      { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shForSync',     { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shIfSync',      { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shUntilSync',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shWhileSync',   { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shShellVariables',{ fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shDerefPOL',    { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shForPP',       { fg = colors.pink, bg = colors.green })
  -- highlight(0, 'shFunctionName,',{ fg = colors.pink, bg = colors.green })
  -- shellbang      xxx links to Comment
end

return shell

