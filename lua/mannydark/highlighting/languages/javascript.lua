-------------------------------------------------------------------------------
-- JavaScript Files
-- Highlighting for .js, .mjs, .cjs files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local javascript = {}


-------------------------------------------------------------------------------
-- Settings

javascript.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - js*

  -- Keywords - Storage/Declaration
  highlight(0, 'jsStorageClass',          { fg = colors.blue,       bg = 'NONE'            })  -- const, let, var
  highlight(0, 'jsVariableDef',           { link = "Constant"})  -- Variable names in declarations

  -- Keywords - Control Flow
  highlight(0, 'jsConditional',           { link = "Keyword"            })  -- if, else, switch, case
  highlight(0, 'jsRepeat',                { link = "Keyword"            })  -- for, while, do
  highlight(0, 'jsStatement',             { link = "Keyword"            })  -- break, continue, debugger
  highlight(0, 'jsReturn',                { link = "Keyword"            })  -- return
  highlight(0, 'jsBranch',                { link = "Keyword"            })  -- break, continue
  highlight(0, 'jsLabel',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Label names
  highlight(0, 'jsBlockLabel',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Block labels
  highlight(0, 'jsBlockLabelKey',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Block label keys

  -- Keywords - Exception Handling
  highlight(0, 'jsTry',                   { link = "Keyword"            })  -- try
  highlight(0, 'jsCatch',                 { link = "Keyword"            })  -- catch
  highlight(0, 'jsFinally',               { link = "Keyword"            })  -- finally
  highlight(0, 'jsException',             { link = "Keyword"            })  -- throw
  highlight(0, 'jsExceptions',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Error, TypeError, etc.

  -- Keywords - Loops
  highlight(0, 'jsFor',                   { link = "Keyword"  })  -- for
  highlight(0, 'jsForAwait',              { link = "Keyword"  })  -- for await
  highlight(0, 'jsWhile',                 { link = "Keyword"  })  -- while
  highlight(0, 'jsDo',                    { link = "Keyword"  })  -- do
  highlight(0, 'jsOf',                    { link = "Keyword"  })  -- of (for...of)

  -- Keywords - Switch
  highlight(0, 'jsSwitchCase',            { link = "Keyword" })  -- case, default
  highlight(0, 'jsSwitchColon',           { link = "Normal" })  -- : in switch

  -- Keywords - Class
  highlight(0, 'jsClassKeyword',          { link = "Keyword"            })  -- class
  highlight(0, 'jsExtendsKeyword',        { link = "Keyword"            })  -- extends
  highlight(0, 'jsClassMethodType',       { link = "Keyword"            })  -- static, get, set
  highlight(0, 'jsSuper',                 { link = "Keyword"            })  -- super
  highlight(0, 'jsClassDefinition',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names

  -- Keywords - Function
  highlight(0, 'jsFunction',              { link = "Keyword"          })  -- function
  highlight(0, 'jsGenerator',             { link = "Keyword"          })  -- function* generator
  highlight(0, 'jsAsyncKeyword',          { link = "Keyword"          })  -- async, await
  highlight(0, 'jsArrowFunction',         { link = "Normal"           })  -- => arrow

  -- Keywords - Operators
  highlight(0, 'jsOperatorKeyword',       { link = "Keyword"      })  -- new, delete, typeof, instanceof, in, void
  highlight(0, 'jsThis',                  { link = "Keyword"      })  -- this

  -- Keywords - Module
  highlight(0, 'jsImport',                { link = "Keyword"        })  -- import
  highlight(0, 'jsExport',                { link = "Keyword"        })  -- export
  highlight(0, 'jsExportDefault',         { link = "Keyword"        })  -- export default
  highlight(0, 'jsFrom',                  { link = "Keyword"        })  -- from
  highlight(0, 'jsModuleAs',              { link = "Keyword"        })  -- as
  highlight(0, 'jsModuleKeyword',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Module/import names
  highlight(0, 'jsModuleAsterisk',        { link = "Keyword" })  -- * in imports

  -- Functions
  highlight(0, 'jsFuncName',              { link = "Function" }) -- Function names
  highlight(0, 'jsFuncCall',              { link = "Function" }) -- Function calls
  highlight(0, 'jsClassFuncName',         { link = "Function" }) -- Method names
  highlight(0, 'jsObjectFuncName',        { link = "Function" }) -- Object method names
  highlight(0, 'jsFunctionKey',           { link = "Function" }) -- Function as object key

  -- Function Parameters
  highlight(0, 'jsFuncArgs',              { link = "Constant"           })  -- Function parameters
  highlight(0, 'jsArrowFuncArgs',         { link = "Constant"           })  -- Arrow function parameters
  highlight(0, 'jsFuncArgExpression',     { link = "Constant"           })  -- Parameter expressions
  highlight(0, 'jsArguments',             { link = "Constant"           })  -- arguments object
  highlight(0, 'jsFuncArgOperator',       { fg = colors.white,      bg = 'NONE'            })  -- = in default params

  -- Variables/Properties
  highlight(0, 'jsObjectProp',            { fg = colors.purple,     bg = 'NONE'            })  -- Object properties
  highlight(0, 'jsObjectKey',             { fg = colors.purple,     bg = 'NONE'            })  -- Object keys
  highlight(0, 'jsObjectKeyString',       { fg = colors.redLight,   bg = 'NONE'            })  -- String object keys
  highlight(0, 'jsObjectStringKey',       { fg = colors.redLight,   bg = 'NONE'            })  -- String keys
  highlight(0, 'jsObjectKeyComputed',     { fg = colors.purple,     bg = 'NONE'            })  -- [computed] keys
  highlight(0, 'jsObjectShorthandProp',   { fg = colors.purple,     bg = 'NONE'            })  -- Shorthand properties
  highlight(0, 'jsClassProperty',         { fg = colors.purple,     bg = 'NONE'            })  -- Class properties
  highlight(0, 'jsClassPropertyComputed', { fg = colors.purple,     bg = 'NONE'            })  -- Computed class properties
  highlight(0, 'jsClassStringKey',        { fg = colors.redLight,   bg = 'NONE'            })  -- String class property keys

  -- Global Objects
  highlight(0, 'jsGlobalObjects',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Array, Object, String, etc.
  highlight(0, 'jsGlobalNodeObjects',     { fg = colors.turquoise,  bg = 'NONE'            })  -- process, Buffer, etc.
  highlight(0, 'jsBuiltins',              { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions
  highlight(0, 'jsPrototype',             { fg = colors.pink,       bg = 'NONE'            })  -- prototype

  -- Literals - Booleans/Null/Undefined
  highlight(0, 'jsBooleanTrue',           { fg = colors.blue,       bg = 'NONE'            })  -- true
  highlight(0, 'jsBooleanFalse',          { fg = colors.blue,       bg = 'NONE'            })  -- false
  highlight(0, 'jsNull',                  { fg = colors.blue,       bg = 'NONE'            })  -- null
  highlight(0, 'jsUndefined',             { fg = colors.blue,       bg = 'NONE'            })  -- undefined
  highlight(0, 'jsNan',                   { fg = colors.blue,       bg = 'NONE'            })  -- NaN

  -- Literals - Numbers
  highlight(0, 'jsNumber',                { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'jsFloat',                 { fg = colors.greenLight, bg = 'NONE'            })  -- Float numbers

  -- Literals - Strings
  highlight(0, 'jsString',                { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, 'jsCharacter',             { fg = colors.pink,       bg = 'NONE'            })  -- Character escapes
  highlight(0, 'jsSpecial',               { fg = colors.pink,       bg = 'NONE'            })  -- Special escape sequences

  -- Template Strings
  highlight(0, 'jsTemplateString',        { fg = colors.redLight,   bg = 'NONE'            })  -- Template literals `...`
  highlight(0, 'jsTemplateBraces',        { fg = colors.pink,       bg = 'NONE'            })  -- ${} in templates
  highlight(0, 'jsTemplateExpression',    { fg = 'NONE',            bg = 'NONE'            })  -- Expression inside ${}
  highlight(0, 'jsTaggedTemplate',        { fg = colors.orange,     bg = 'NONE'            })  -- Tagged template function
  highlight(0, 'jsTemplateStringTag',     { fg = colors.orange,     bg = 'NONE'            })  -- Tag function name

  -- Regular Expressions
  highlight(0, 'jsRegexpString',          { fg = colors.redLight,   bg = 'NONE'            })  -- /regex/
  highlight(0, 'jsRegexpBoundary',        { fg = colors.redLight,   bg = 'NONE'            })  -- / delimiters
  highlight(0, 'jsRegexpGroup',           { fg = colors.redLight,   bg = 'NONE'            })  -- Groups
  highlight(0, 'jsRegexpCharClass',       { fg = colors.pink,       bg = 'NONE'            })  -- [...] character class
  highlight(0, 'jsRegexpBackRef',         { fg = colors.pink,       bg = 'NONE'            })  -- Back references
  highlight(0, 'jsRegexpQuantifier',      { fg = colors.pink,       bg = 'NONE'            })  -- +, *, ?, {n,m}
  highlight(0, 'jsRegexpMod',             { fg = colors.pink,       bg = 'NONE'            })  -- Flags (g, i, m, etc.)
  highlight(0, 'jsRegexpOr',              { fg = colors.white,      bg = 'NONE'            })  -- | alternation

  -- Operators
  highlight(0, 'jsOperator',              { fg = colors.white,      bg = 'NONE'            })  -- =, +, -, etc.
  highlight(0, 'jsSpreadOperator',        { fg = colors.pink,       bg = 'NONE'            })  -- ... spread
  highlight(0, 'jsRestOperator',          { fg = colors.pink,       bg = 'NONE'            })  -- ... rest
  highlight(0, 'jsSpreadExpression',      { fg = colors.pink,       bg = 'NONE'            })  -- Spread expressions
  highlight(0, 'jsRestExpression',        { fg = colors.pink,       bg = 'NONE'            })  -- Rest expressions
  highlight(0, 'jsTernaryIfOperator',     { fg = colors.white,      bg = 'NONE'            })  -- ? : ternary
  highlight(0, 'jsTernaryIf',             { fg = colors.white,      bg = 'NONE'            })  -- Ternary expression

  -- Destructuring
  highlight(0, 'jsDestructuringBlock',        { fg = 'NONE',            bg = 'NONE'            })  -- Destructuring block
  highlight(0, 'jsDestructuringArray',        { fg = 'NONE',            bg = 'NONE'            })  -- Array destructuring
  highlight(0, 'jsDestructuringProperty',     { fg = colors.purple,     bg = 'NONE'            })  -- Property in destructuring
  highlight(0, 'jsDestructuringAssignment',   { fg = colors.purple,     bg = 'NONE'            })  -- Assignment in destructuring
  highlight(0, 'jsDestructuringValue',        { fg = colors.purple,     bg = 'NONE'            })  -- Default value
  highlight(0, 'jsDestructuringValueAssignment', { fg = colors.white,   bg = 'NONE'            })  -- = in default value
  highlight(0, 'jsDestructuringPropertyComputed', { fg = colors.purple, bg = 'NONE'            })  -- Computed property
  highlight(0, 'jsDestructuringPropertyValue', { fg = colors.purple,    bg = 'NONE'            })  -- Property value
  highlight(0, 'jsDestructuringBraces',       { fg = colors.white,      bg = 'NONE'            })  -- { } in destructuring
  highlight(0, 'jsDestructuringNoise',        { fg = colors.white,      bg = 'NONE'            })  -- , and other punctuation

  -- Punctuation/Brackets
  highlight(0, 'jsParens',                { fg = colors.white,      bg = 'NONE'            })  -- ()
  highlight(0, 'jsParen',                 { fg = 'NONE',            bg = 'NONE'            })  -- Paren content
  highlight(0, 'jsBraces',                { fg = colors.white,      bg = 'NONE'            })  -- {}
  highlight(0, 'jsBrackets',              { fg = colors.white,      bg = 'NONE'            })  -- []
  highlight(0, 'jsBracket',               { fg = 'NONE',            bg = 'NONE'            })  -- Bracket content
  highlight(0, 'jsFuncBraces',            { fg = colors.white,      bg = 'NONE'            })  -- Function {}
  highlight(0, 'jsFuncParens',            { fg = colors.white,      bg = 'NONE'            })  -- Function ()
  highlight(0, 'jsClassBraces',           { fg = colors.white,      bg = 'NONE'            })  -- Class {}
  highlight(0, 'jsObjectBraces',          { fg = colors.white,      bg = 'NONE'            })  -- Object {}
  highlight(0, 'jsModuleBraces',          { fg = colors.white,      bg = 'NONE'            })  -- Import/Export {}
  highlight(0, 'jsIfElseBraces',          { fg = colors.white,      bg = 'NONE'            })  -- If/Else {}
  highlight(0, 'jsParensIfElse',          { fg = colors.white,      bg = 'NONE'            })  -- If/Else ()
  highlight(0, 'jsSwitchBraces',          { fg = colors.white,      bg = 'NONE'            })  -- Switch {}
  highlight(0, 'jsParenSwitch',           { fg = colors.white,      bg = 'NONE'            })  -- Switch ()
  highlight(0, 'jsParensSwitch',          { fg = colors.white,      bg = 'NONE'            })  -- Switch ()
  highlight(0, 'jsRepeatBraces',          { fg = colors.white,      bg = 'NONE'            })  -- Loop {}
  highlight(0, 'jsParenFor',              { fg = colors.white,      bg = 'NONE'            })  -- For ()
  highlight(0, 'jsParensFor',             { fg = colors.white,      bg = 'NONE'            })  -- For ()
  highlight(0, 'jsParenWhile',            { fg = colors.white,      bg = 'NONE'            })  -- While ()
  highlight(0, 'jsParensWhile',           { fg = colors.white,      bg = 'NONE'            })  -- While ()
  highlight(0, 'jsTryCatchBraces',        { fg = colors.white,      bg = 'NONE'            })  -- Try/Catch {}
  highlight(0, 'jsParenCatch',            { fg = colors.white,      bg = 'NONE'            })  -- Catch ()
  highlight(0, 'jsParensCatch',           { fg = colors.white,      bg = 'NONE'            })  -- Catch ()
  highlight(0, 'jsFinallyBraces',         { fg = colors.white,      bg = 'NONE'            })  -- Finally {}

  -- Punctuation/Separators
  highlight(0, 'jsDot',                   { fg = colors.white,      bg = 'NONE'            })  -- . dot
  highlight(0, 'jsNoise',                 { fg = colors.white,      bg = 'NONE'            })  -- ; semicolon
  highlight(0, 'jsObjectColon',           { fg = colors.white,      bg = 'NONE'            })  -- : in objects
  highlight(0, 'jsObjectSeparator',       { fg = colors.white,      bg = 'NONE'            })  -- , in objects
  highlight(0, 'jsFuncArgCommas',         { fg = colors.white,      bg = 'NONE'            })  -- , in params
  highlight(0, 'jsModuleComma',           { fg = colors.white,      bg = 'NONE'            })  -- , in imports

  -- Decorators
  highlight(0, 'jsDecorator',             { fg = colors.pink,       bg = 'NONE'            })  -- @decorator
  highlight(0, 'jsDecoratorFunction',     { fg = colors.pink,       bg = 'NONE'            })  -- Decorator function
  highlight(0, 'jsParensDecorator',       { fg = colors.white,      bg = 'NONE'            })  -- Decorator ()
  highlight(0, 'jsParenDecorator',        { fg = 'NONE',            bg = 'NONE'            })  -- Decorator paren content

  -- Blocks (cleared/transparent)
  highlight(0, 'jsFuncBlock',             { fg = 'NONE',            bg = 'NONE'            })  -- Function body
  highlight(0, 'jsClassBlock',            { fg = 'NONE',            bg = 'NONE'            })  -- Class body
  highlight(0, 'jsClassValue',            { fg = 'NONE',            bg = 'NONE'            })  -- Class content
  highlight(0, 'jsClassNoise',            { fg = colors.white,      bg = 'NONE'            })  -- Class punctuation
  highlight(0, 'jsIfElseBlock',           { fg = 'NONE',            bg = 'NONE'            })  -- If/Else body
  highlight(0, 'jsParenIfElse',           { fg = 'NONE',            bg = 'NONE'            })  -- If/Else condition
  highlight(0, 'jsRepeatBlock',           { fg = 'NONE',            bg = 'NONE'            })  -- Loop body
  highlight(0, 'jsSwitchBlock',           { fg = 'NONE',            bg = 'NONE'            })  -- Switch body
  highlight(0, 'jsTryCatchBlock',         { fg = 'NONE',            bg = 'NONE'            })  -- Try/Catch body
  highlight(0, 'jsFinallyBlock',          { fg = 'NONE',            bg = 'NONE'            })  -- Finally body
  highlight(0, 'jsModuleGroup',           { fg = 'NONE',            bg = 'NONE'            })  -- Module group
  highlight(0, 'jsExportDefaultGroup',    { fg = 'NONE',            bg = 'NONE'            })  -- Export default group
  highlight(0, 'jsObjectValue',           { fg = 'NONE',            bg = 'NONE'            })  -- Object value content
  highlight(0, 'jsBlock',                 { fg = 'NONE',            bg = 'NONE'            })  -- Generic block

  -- Comments
  highlight(0, 'jsComment',               { fg = colors.red,        bg = 'NONE'            })  -- Comments
  highlight(0, 'jsCommentTodo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME
  highlight(0, 'jsCommentFunction',       { fg = colors.red,        bg = 'NONE'            })  -- Function comments
  highlight(0, 'jsCommentClass',          { fg = colors.red,        bg = 'NONE'            })  -- Class comments
  highlight(0, 'jsCommentIfElse',         { fg = colors.red,        bg = 'NONE'            })  -- If/Else comments
  highlight(0, 'jsCommentRepeat',         { fg = colors.red,        bg = 'NONE'            })  -- Loop comments
  highlight(0, 'jsEnvComment',            { fg = colors.red,        bg = 'NONE'            })  -- Shebang #!/usr/bin/env
  highlight(0, 'jsCvsTag',                { fg = colors.red,        bg = 'NONE'            })  -- CVS tags

  -- JSDoc
  highlight(0, 'jsDocTags',               { fg = colors.pink,       bg = 'NONE'            })  -- @param, @returns, etc.
  highlight(0, 'jsDocType',               { fg = colors.turquoise,  bg = 'NONE'            })  -- {Type} in JSDoc
  highlight(0, 'jsDocTypeNoParam',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Type without param
  highlight(0, 'jsDocParam',              { fg = colors.purple,     bg = 'NONE'            })  -- Param name in JSDoc
  highlight(0, 'jsDocSeeTag',             { fg = colors.pink,       bg = 'NONE'            })  -- @see tag
  highlight(0, 'jsDocTypeBrackets',       { fg = colors.white,      bg = 'NONE'            })  -- {} in JSDoc
  highlight(0, 'jsDocTypeRecord',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Record type

  -- Flow Types (if used)
  highlight(0, 'jsFlowTypeKeyword',       { fg = colors.blue,       bg = 'NONE'            })  -- type keyword
  highlight(0, 'jsFlowDefinition',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Flow type definitions
  highlight(0, 'jsFlowImportType',        { fg = colors.blue,       bg = 'NONE'            })  -- import type
  highlight(0, 'jsFlowTypeStatement',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Type statements
  highlight(0, 'jsFlowArgumentDef',       { fg = colors.purple,     bg = 'NONE'            })  -- Argument definitions
  highlight(0, 'jsFlowReturn',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Return type
  highlight(0, 'jsFlowFunctionGroup',     { fg = 'NONE',            bg = 'NONE'            })  -- Function group
  highlight(0, 'jsFlowClassFunctionGroup', { fg = 'NONE',           bg = 'NONE'            })  -- Class function group
  highlight(0, 'jsFlowClassGroup',        { fg = 'NONE',            bg = 'NONE'            })  -- Class group
  highlight(0, 'jsFlowClassDef',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Class definition

  -- DOM/HTML Integration
  highlight(0, 'jsDomElemAttrs',          { fg = colors.purple,     bg = 'NONE'            })  -- DOM attributes
  highlight(0, 'jsDomElemFuncs',          { fg = colors.orange,     bg = 'NONE'            })  -- DOM methods
  highlight(0, 'jsHtmlElemAttrs',         { fg = colors.purple,     bg = 'NONE'            })  -- HTML attributes
  highlight(0, 'jsHtmlElemFuncs',         { fg = colors.orange,     bg = 'NONE'            })  -- HTML methods
  highlight(0, 'jsHtmlEvents',            { fg = colors.purple,     bg = 'NONE'            })  -- HTML events
  highlight(0, 'jsDomNodeConsts',         { fg = colors.pink,       bg = 'NONE'            })  -- DOM constants
  highlight(0, 'jsCssStyles',             { fg = colors.purple,     bg = 'NONE'            })  -- CSS styles

  -- Future/Reserved Keywords
  highlight(0, 'jsFutureKeys',            { fg = colors.blue,       bg = 'NONE'            })  -- Future reserved words

  -- Errors
  highlight(0, 'jsError',                 { fg = 'NONE',            bg = 'NONE', sp = colors.red, undercurl = true })  -- Errors
  highlight(0, 'jsParensError',           { fg = 'NONE',            bg = 'NONE', sp = colors.red, undercurl = true })  -- Paren errors
  highlight(0, 'jsDomErrNo',              { fg = 'NONE',            bg = 'NONE', sp = colors.red, undercurl = true })  -- DOM errors

  -- Legacy javascript* groups
  highlight(0, 'javaScript',              { fg = colors.white,      bg = 'NONE'            })  -- JS in HTML
  highlight(0, 'javaScriptLineComment',   { fg = colors.red,        bg = 'NONE'            })  -- // comment
  highlight(0, 'javaScriptComment',       { fg = colors.red,        bg = 'NONE'            })  -- /* comment */
  highlight(0, 'javascriptCommentTodo',   { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO
  highlight(0, 'javascriptHtmlEvents',    { fg = colors.purple,     bg = 'NONE'            })  -- HTML events
  highlight(0, 'javascriptDomElemAttrs',  { fg = colors.purple,     bg = 'NONE'            })  -- DOM attrs
  highlight(0, 'javascriptDomElemFuncs',  { fg = colors.orange,     bg = 'NONE'            })  -- DOM funcs
  highlight(0, 'javascriptSpreadOp',      { fg = colors.pink,       bg = 'NONE'            })  -- ... spread
  highlight(0, 'javascriptTagRef',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Tag reference
  highlight(0, 'javaScriptBlock',         { fg = 'NONE',            bg = 'NONE'            })  -- Block
  highlight(0, 'javaScriptSpecial',       { fg = colors.pink,       bg = 'NONE'            })  -- Special chars
  highlight(0, 'javaScriptEmbed',         { fg = colors.white,      bg = 'NONE'            })  -- Embedded JS


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.javascript)

  -- Variables
  highlight(0, '@variable.javascript',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.javascript',      { fg = colors.pink,      bg = 'NONE' })  -- this, arguments, super
  highlight(0, '@variable.member.javascript',       { fg = colors.purple,    bg = 'NONE' })  -- Object properties
  highlight(0, '@variable.parameter.javascript',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters

  -- Constants
  highlight(0, '@constant.javascript',              { fg = colors.pink,      bg = 'NONE' })  -- UPPER_CASE constants
  highlight(0, '@constant.builtin.javascript',      { fg = colors.blue,      bg = 'NONE' })  -- true, false, null, undefined, NaN, Infinity

  -- Modules
  highlight(0, '@module.javascript',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names
  highlight(0, '@module.builtin.javascript',        { fg = colors.turquoise, bg = 'NONE' })  -- Built-in modules

  -- Types
  highlight(0, '@type.javascript',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.javascript',          { fg = colors.turquoise, bg = 'NONE' })  -- Array, Object, String, etc.

  -- Functions
  highlight(0, '@function.javascript',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.javascript',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.javascript',       { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@function.method.call.javascript',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.builtin.javascript',      { fg = colors.orange,    bg = 'NONE' })  -- Built-in functions
  highlight(0, '@constructor.javascript',           { fg = colors.turquoise, bg = 'NONE' })  -- Constructor calls

  -- Keywords
  highlight(0, '@keyword.javascript',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.javascript',      { fg = colors.blue,      bg = 'NONE' })  -- function, async
  highlight(0, '@keyword.operator.javascript',      { fg = colors.blue,      bg = 'NONE' })  -- typeof, instanceof, new, delete, in, void
  highlight(0, '@keyword.return.javascript',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.conditional.javascript',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, switch, case
  highlight(0, '@keyword.conditional.ternary.javascript', { fg = colors.white, bg = 'NONE' })  -- ? :
  highlight(0, '@keyword.repeat.javascript',        { fg = colors.blue,      bg = 'NONE' })  -- for, while, do
  highlight(0, '@keyword.exception.javascript',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally, throw
  highlight(0, '@keyword.import.javascript',        { fg = colors.blue,      bg = 'NONE' })  -- import, export, from
  highlight(0, '@keyword.coroutine.javascript',     { fg = colors.blue,      bg = 'NONE' })  -- async, await, yield
  highlight(0, '@keyword.type.javascript',          { fg = colors.blue,      bg = 'NONE' })  -- class, interface
  highlight(0, '@keyword.directive.javascript',     { link = "Keyword"})  -- "use strict"

  -- Attributes (Decorators)
  highlight(0, '@attribute.javascript',             { fg = colors.pink,      bg = 'NONE' })  -- @decorators

  -- Labels
  highlight(0, '@label.javascript',                 { fg = colors.turquoise, bg = 'NONE' })  -- Labels

  -- Strings
  highlight(0, '@string.javascript',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.javascript',         { fg = colors.pink,      bg = 'NONE' })  -- \n, \t, etc.
  highlight(0, '@string.regexp.javascript',         { fg = colors.redLight,  bg = 'NONE' })  -- /regex/
  highlight(0, '@string.special.javascript',        { fg = colors.pink,      bg = 'NONE' })  -- Special strings

  -- Numbers
  highlight(0, '@number.javascript',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.javascript',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.javascript',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Comments
  highlight(0, '@comment.javascript',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.javascript', { link = "Ignore" })  -- JSDoc comments

  -- Operators and Punctuation
  highlight(0, '@operator.javascript',              { fg = colors.white,     bg = 'NONE' })  -- +, -, =, etc.
  highlight(0, '@punctuation.bracket.javascript',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.javascript', { fg = colors.white,     bg = 'NONE' })  -- , ; :
  highlight(0, '@punctuation.special.javascript',   { fg = colors.pink,      bg = 'NONE' })  -- ${}, template braces

  -- Character Special
  highlight(0, '@character.special.javascript',     { fg = colors.pink,      bg = 'NONE' })  -- Special characters


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.javascript)

  highlight(0, '@lsp.type.class.javascript',        { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.interface.javascript',    { fg = colors.turquoise, bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.type.javascript',         { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.enum.javascript',         { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.javascript',   { fg = colors.pink,      bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.function.javascript',     { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.javascript',       { link = "Function" })  -- Methods
  highlight(0, '@lsp.type.variable.javascript',     { link = "Constant" })  -- Variables
  highlight(0, '@lsp.type.parameter.javascript',    { link = "Constant" })  -- Parameters
  highlight(0, '@lsp.type.property.javascript',     { link = "Constant" })  -- Properties
  highlight(0, '@lsp.type.namespace.javascript',    { fg = colors.turquoise, bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.keyword.javascript',      { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.decorator.javascript',    { fg = colors.pink,      bg = 'NONE' })  -- Decorators
  highlight(0, '@lsp.type.string.javascript',       { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.javascript',       { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.regexp.javascript',       { fg = colors.redLight,  bg = 'NONE' })  -- RegExp
  highlight(0, '@lsp.type.comment.javascript',      { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.javascript',       { link = "Constant" })  -- const variables
  highlight(0, '@lsp.typemod.variable.defaultLibrary.javascript', { link = "Constant" })  -- Built-in variables
  highlight(0, '@lsp.typemod.function.declaration.javascript',    { link = "Function" })  -- Function declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.javascript', { link = "Function" })  -- Built-in functions
  highlight(0, '@lsp.typemod.class.declaration.javascript',    { fg = colors.turquoise, bg = 'NONE' })  -- Class declarations
  highlight(0, '@lsp.typemod.class.defaultLibrary.javascript', { fg = colors.turquoise, bg = 'NONE' })  -- Built-in classes
  highlight(0, '@lsp.typemod.property.declaration.javascript',  { link = "Constant" })  -- Property declarations
  highlight(0, '@lsp.typemod.parameter.declaration.javascript', { link = "Constant"})  -- Parameter declarations
end

return javascript


