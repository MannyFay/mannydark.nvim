------------------------------------------------------------------------------
-- JavaScript Files
-- Highlighting for .js files.
------------------------------------------------------------------------------

local colors     = require('mannydark.palette')
local highlight  = vim.api.nvim_set_hl
local javaScript = {}


--------------------------------------------------------------
-- Settings

javaScript.setupHighlighting = function()
  highlight(0, 'jsClassDefinition',               { fg = colors.turquoise, bg = 'NONE' })  -- Name of classes.
  highlight(0, 'jsFuncArgs',                      { fg = colors.purple,    bg = 'NONE' })  -- Parameters of functions.
  highlight(0, 'jsThis',                          { fg = colors.blue,      bg = 'NONE' })  -- 'this' keyword.
  highlight(0, 'jsArrowFunction',                 { fg = colors.white,     bg = 'NONE' })  -- Arrow => of arrow functions.
  highlight(0, 'jsStorageClass',                  { fg = colors.blue,      bg = 'NONE' })  -- 'const' Keyword.
  highlight(0, 'jsVariableDef',                   { fg = colors.purple,    bg = 'NONE' })  -- Names of variables.
  highlight(0, 'jsOperatorKeyword',               { fg = colors.blue,      bg = 'NONE' })  -- 'new' keyword.
  highlight(0, 'jsDot',                           { fg = colors.white,     bg = 'NONE' })  -- Dots in JS files.
  highlight(0, 'jsFuncName',                      { fg = colors.orange,    bg = 'NONE' })  -- Name of functions.
  highlight(0, 'jsFunction',                      { fg = colors.blue,      bg = 'NONE' })  -- 'function' keyword.
  highlight(0, 'jsClassKeyword',                  { fg = colors.blue,      bg = 'NONE' })  -- 'class' keyword.
  highlight(0, 'jsComment',                       { fg = colors.red,       bg = 'NONE' })  -- Comments.
  highlight(0, 'jsCommentTodo',                   { fg = colors.red,       bg = 'NONE' })  -- 'Todo' text.
  highlight(0, 'jsOperator',                      { fg = colors.white,     bg = 'NONE' })  -- Operators like =.
  highlight(0, 'jsFuncBraces',                    { fg = colors.white,     bg = 'NONE' })  -- Curly braces {}.
  highlight(0, 'jsFuncCall',                      { fg = colors.orange,    bg = 'NONE' })  -- Functions that are called.
  highlight(0, 'jsString',                        { fg = colors.redLight,  bg = 'NONE' })  -- Regular strings.
  highlight(0, 'jsGlobalObjects',                 { fg = colors.purple,    bg = 'NONE' })  -- Objects.
  highlight(0, 'jsParens',                        { fg = colors.white,     bg = 'NONE' })  -- Parentheses ().
  highlight(0, 'jsNoise',                         { fg = colors.white,     bg = 'NONE' })  -- Semicolon ;.
  highlight(0, 'jsParen',                         { fg = 'NONE',           bg = 'NONE' })  -- White space.
  highlight(0, 'jsFuncParens',                    { fg = colors.white,     bg = 'NONE' })  -- Parentheses () of functions.
  highlight(0, 'jsReturn',                        { fg = colors.blue,      bg = 'NONE' })  -- 'return' keyword.
  highlight(0, 'jsFuncBlock',                     { fg = 'NONE',           bg = 'NONE' })  -- Whole function body area.
  highlight(0, 'jsFuncArgCommas',                 { fg = colors.white,     bg = 'NONE' })  -- Comas in functions parameter list.
  highlight(0, 'jsClassFuncName',                 { fg = colors.orange,    bg = 'NONE' })  -- Names of methods.
  highlight(0, 'jsClassBlock',                    { fg = 'NONE',           bg = 'NONE' })  -- Whole area of method body.
  highlight(0, 'jsObjectProp',                    { fg = colors.purple,    bg = 'NONE' })  -- Variable in methods.
  highlight(0, 'jsClassProperty',                 { fg = colors.purple,    bg = 'NONE' })  -- Attribute.
  highlight(0, 'jsClassBraces',                   { fg = colors.white,     bg = 'NONE' })  -- Curly Braces {} of classes.
  highlight(0, 'jsConditional',                   { fg = colors.blue,      bg = 'NONE' })  -- Keywords of conditional statements.
  highlight(0, 'jsIfElseBlock',                   { fg = 'NONE',           bg = 'NONE' })  -- Body of conditional statements.
  highlight(0, 'jsIfElseBraces',                  { fg = colors.white,     bg = 'NONE' })  -- Curly braces {} of conditional statement.
  highlight(0, 'jsParensIfElse',                  { fg = colors.white,     bg = 'NONE' })  -- Parentheses of conditional statements.
  highlight(0, 'jsParenIfElse',                   { fg = 'NONE',           bg = 'NONE' })  -- Content of conditional statement.
  highlight(0, 'jsClassValue',                    { fg = 'NONE',           bg = 'NONE' })  -- White spaces inside of classes.
  highlight(0, 'jsTemplateString',                { fg = colors.redLight,  bg = 'NONE' })  -- Template string back ticks.
  highlight(0, 'jsTemplateBraces',                { fg = colors.redLight,  bg = 'NONE' })  -- Curly braces {} in template strings.
  highlight(0, 'jsTemplateExpression',            { fg = colors.redLight,  bg = 'NONE' })  -- Content of template strings.
  highlight(0, 'jsGlobalNodeObjects',             { fg = colors.orange,    bg = 'NONE' })  -- Global function names.
  highlight(0, 'jsBrackets',                      { fg = colors.white,     bg = 'NONE' })  -- Brackets [].
  highlight(0, 'jsObjectBraces',                  { fg = colors.white,     bg = 'NONE' })  -- Curly braces {} of objects.
  highlight(0, 'jsObjectKey',                     { fg = colors.purple,    bg = 'NONE' })  -- Object key words.
  highlight(0, 'jsObjectColon',                   { fg = colors.white,     bg = 'NONE' })  -- Colon of object key words.
  highlight(0, 'jsObjectSeparator',               { fg = colors.white,     bg = 'NONE' })  -- Comas of object values.
  highlight(0, 'jsBracket',                       { fg = 'NONE',           bg = 'NONE' })  -- Content of brackets [].
  highlight(0, 'jsObjectValue',                   { fg = 'NONE',           bg = 'NONE' })  -- Content of objects.
  highlight(0, 'jsRegexpString',                  { fg = colors.redLight,  bg = 'NONE' })  -- Regular expression strings.
  highlight(0, 'jsRegexpGroup',                   { fg = colors.redLight,  bg = 'NONE' })  -- Content of regular expression strings.
  highlight(0, 'jsSpecial',                       { fg = colors.white,     bg = 'NONE' })  -- Special characters.
  highlight(0, 'jsRegexpBoundary',                { fg = colors.redLight,  bg = 'NONE' })  -- Boundary of regular expression strings.
  highlight(0, 'jsRegexpOr',                      { fg = colors.white,     bg = 'NONE' })  -- Or of regular expression strings.
  highlight(0, 'jsModuleKeyword',                 { fg = colors.turquoise, bg = 'NONE' })  -- Module name.
  highlight(0, 'javaScript',                      { fg = colors.white,     bg = 'NONE' })  -- JS in HTML.
  highlight(0, 'jsBooleanTrue',                   { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'jsBooleanFalse',                  { fg = colors.blue,      bg = 'NONE' })
  highlight(0, 'jsExportDefault',                 { fg = colors.blue,      bg = 'NONE' })  -- 'export default' keyword.
  highlight(0, 'jsFrom',                          { fg = colors.blue,      bg = 'NONE' })  -- 'from' keyword.
  highlight(0, 'jsModuleBraces',                  { fg = colors.white,     bg = 'NONE' })  -- Curly braces {} of modules.
  highlight(0, 'jsModuleGroup',                   { fg = 'NONE',           bg = 'NONE' })  -- Whole area of modules.
  highlight(0, 'jsDocTags',                       { fg = colors.green,     bg = 'NONE' })  -- Tags of documentation.
  highlight(0, 'jsDocTypeNoParam',                { fg = colors.green,     bg = 'NONE' })  -- Type of documentation.
  highlight(0, 'jsObjectShorthandProp',           { fg = colors.purple,    bg = 'NONE' })  -- Shorthand properties.
  highlight(0, 'jsObjectKeyString',               { fg = colors.redLight,  bg = 'NONE' })  -- Object key strings.

  ------------------------- Not used by now:
  highlight(0, 'jsPrototype',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsTaggedTemplate',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParensError',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringBlock',            { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringArray',            { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowDefinition',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsOf',                            { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsModuleAsterisk',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowImportType',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowTypeStatement',             { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsModuleAs',                      { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsModuleComma',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsExportDefaultGroup',            { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowTypeKeyword',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFloat',                         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRegexpCharClass',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRegexpBackRef',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRegexpQuantifier',              { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRegexpMod',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFunctionKey',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsObjectKeyComputed',             { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsObjectFuncName',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsObjectMethodType',              { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsObjectStringKey',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsNull',                          { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsUndefined',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsNan',                           { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsSuper',                         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsBlock',                         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsBlockLabel',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsBlockLabelKey',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsStatement',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsCommentIfElse',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParenSwitch',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsWhile',                         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParenWhile',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFor',                           { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParenFor',                      { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsForAwait',                      { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDo',                            { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRepeatBlock',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsLabel',                         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsSwitchColon',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsSwitchCase',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsTry',                           { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsTryCatchBlock',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFinally',                       { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFinallyBlock',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsCatch',                         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParenCatch',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsException',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsAsyncKeyword',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsSwitchBlock',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsExceptions',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsBuiltins',                      { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFutureKeys',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDomErrNo',                      { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDomNodeConsts',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsHtmlEvents',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsSpreadExpression',              { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParensDecorator',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParenDecorator',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParensWhile',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsCommentRepeat',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParensFor',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParensSwitch',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParensCatch',                   { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFuncArgExpression',             { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRestExpression',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowArgumentDef',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsCommentFunction',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowReturn',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsClassMethodType',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsArrowFuncArgs',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsGenerator',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDecorator',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsClassPropertyComputed',         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsClassStringKey',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsTryCatchBraces',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFinallyBraces',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsSwitchBraces',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRepeatBraces',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringBraces',           { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringProperty',         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringAssignment',       { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringNoise',            { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringPropertyComputed', { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringPropertyValue',    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsBraces',                        { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsSpreadOperator',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRestOperator',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsTernaryIfOperator',             { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsTernaryIf',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowFunctionGroup',             { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFuncArgOperator',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsArguments',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsExtendsKeyword',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsClassNoise',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowClassFunctionGroup',        { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowClassGroup',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsCommentClass',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsFlowClassDef',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringValue',            { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDestructuringValueAssignment',  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsEnvComment',                    { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDecoratorFunction',             { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsCvsTag',                        { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDocParam',                      { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDocType',                       { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDocSeeTag',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDocTypeBrackets',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDocTypeRecord',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsParensRepeat',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsCharacter',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsBranch',                        { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsRepeat',                        { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsError',                         { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDomElemAttrs',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsDomElemFuncs',                  { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsHtmlElemAttrs',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsHtmlElemFuncs',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsCssStyles',                     { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javascriptHtmlEvents',            { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javascriptDomElemAttrs',          { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javascriptDomElemFuncs',          { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javascriptCommentTodo',           { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javaScriptBlock',                 { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javaScriptLineComment',           { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javaScriptComment',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javascriptSpreadOp',              { fg = colors.purple, bg = colors.blue })
  highlight(0, 'jsTemplateStringTag',             { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javascriptTagRef',                { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javaScriptSpecial',               { fg = colors.purple, bg = colors.blue })
  highlight(0, 'javaScriptEmbed',                 { fg = colors.purple, bg = colors.blue })
end

return javaScript

