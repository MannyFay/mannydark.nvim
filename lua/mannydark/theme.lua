
local c     = require('mannydark.palette')
local hl    = vim.api.nvim_set_hl
local theme = {}

package.path = package.path .. ";../../highlightings/?.lua"

local ini = require('inifiles')



theme.buildTheme = function()
  ini.setupHighlighting()
------------------------------------------------------------------------------
-- Editor
------------------------------------------------------------------------------

  -- Typescript
  -- React (jsx)
  -- graphQL
  -- SQL
  
------------------------------------------------------------------------------
-- JavaScript
------------------------------------------------------------------------------
  
hl(0, 'jsClassDefinition', { fg = c.turquoise, bg = 'NONE'    }) -- Name of classes.
hl(0, 'jsFuncArgs', { fg = c.purple, bg = 'NONE'    }) -- Parameters of functions.
hl(0, 'jsThis', { fg = c.blue, bg = 'NONE'    }) -- 'this' keyword.
hl(0, 'jsArrowFunction', { fg = c.white, bg = 'NONE'    }) -- Arrow => of arrow functions.
hl(0, 'jsStorageClass', { fg = c.blue, bg = 'NONE'    }) -- 'const' Keyword.
hl(0, 'jsVariableDef', { fg = c.purple, bg = 'NONE'    }) -- Names of variables.
hl(0, 'jsOperatorKeyword', { fg = c.blue, bg = 'NONE'    }) -- 'new' keyword.
hl(0, 'jsDot'         , { fg = c.white      , bg = 'NONE' })  -- Dots in JS files.
hl(0, 'jsFuncName'         , { fg = c.orange      , bg = 'NONE' })  -- Name of functions.
hl(0, 'jsFunction'         , { fg = c.blue      , bg = 'NONE' })  -- 'function' keyword.
hl(0, 'jsClassKeyword'         , { fg = c.blue      , bg = 'NONE' })  -- 'class' keyword.
hl(0, 'jsComment'         , { fg = c.red     , bg = 'NONE' })  -- Comments.
hl(0, 'jsCommentTodo'         , { fg = c.red      , bg = 'NONE' })  -- 'Todo' text.
hl(0, 'jsOperator'         , { fg = c.white      , bg = 'NONE' })  -- Operators like =.
hl(0, 'jsFuncBraces'         , { fg = c.white      , bg = 'NONE' })  -- Curly braces {}.
hl(0, 'jsFuncCall'         , { fg = c.orange      , bg = 'NONE' })  -- Funtions that are called.
hl(0, 'jsString'         , { fg = c.redLight      , bg = 'NONE' })  -- Regular strings.
hl(0, 'jsGlobalObjects'         , { fg = c.purple      , bg = 'NONE' }) -- Objects.
hl(0, 'jsParens'         , { fg = c.white      , bg = 'NONE' })  -- Parentheses ().
hl(0, 'jsNoise'         , { fg = c.white      , bg = 'NONE' })  -- Semikolon ;.
hl(0, 'jsParen'         , { fg = 'NONE'      , bg = 'NONE' })  -- Whitespace.
hl(0, 'jsFuncParens'         , { fg = c.white      , bg = 'NONE' })  -- Parentheses () of functions.
hl(0, 'jsReturn'         , { fg = c.blue      , bg = 'NONE' })  -- 'return' keyword.
hl(0, 'jsFuncBlock'         , { fg = 'NONE'      , bg = 'NONE' })  -- Whole function body area.
hl(0, 'jsFuncArgCommas'         , { fg = c.white      , bg = 'NONE' })  -- Commata in functions parameter list.
hl(0, 'jsClassFuncName'         , { fg = c.orange      , bg = 'NONE' })  -- Names of methods.
hl(0, 'jsClassBlock'         , { fg = 'NONE'      , bg = 'NONE' })  -- Whole area of method body.
hl(0, 'jsObjectProp'         , { fg = c.purple      , bg = 'NONE' })  -- Variable in methods.
hl(0, 'jsClassProperty'         , { fg = c.purple      , bg = 'NONE' })  -- Attribute.
hl(0, 'jsClassBraces'         , { fg = c.white      , bg = 'NONE' })  -- Curly Braces {} of classes.
hl(0, 'jsConditional'         , { fg = c.blue      , bg = 'NONE' })  -- Keywords of conditional statements.
hl(0, 'jsIfElseBlock'         , { fg = 'NONE'      , bg = 'NONE' })  -- Body of conditional statements.
hl(0, 'jsIfElseBraces'         , { fg = c.white      , bg = 'NONE' })  -- Curly braces {} of conditional statement.
hl(0, 'jsParensIfElse'         , { fg = c.white      , bg = 'NONE' })  -- Parentheses of conditional statements.
hl(0, 'jsParenIfElse'         , { fg = 'NONE'      , bg = 'NONE' })  -- Content of conditional statement.
hl(0, 'jsClassValue'         , { fg = 'NONE'      , bg = 'NONE' })  -- Whitespaces inside of classes.
hl(0, 'jsTemplateString'         , { fg = c.redLight      , bg = 'NONE' })  -- Template string back ticks.
hl(0, 'jsTemplateBraces'         , { fg = c.redLight      , bg = 'NONE' })  -- Curly braces {} in template strings.
hl(0, 'jsTemplateExpression'         , { fg = c.redLight      , bg = 'NONE' })  -- Content of template strings.
hl(0, 'jsGlobalNodeObjects'         , { fg = c.orange      , bg = 'NONE' })  -- Global function names.



  
hl(0, 'jsPrototype'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsTaggedTemplate'         , { fg = c.purple      , bg = c.blue })

  

  
hl(0, 'jsParensError'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringBlock'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringArray'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowDefinition'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsOf'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBooleanTrue'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBooleanFalse'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsModuleAsterisk'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsModuleKeyword'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsModuleGroup'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowImportType'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsExportDefault'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowTypeStatement'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsModuleAs'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFrom'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsModuleComma'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsExportDefaultGroup'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowTypeKeyword'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsSpecial'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFloat'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRegexpCharClass'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRegexpBoundary'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRegexpBackRef'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRegexpQuantifier'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRegexpOr'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRegexpMod'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRegexpGroup'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRegexpString'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectSeparator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectShorthandProp'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFunctionKey'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectValue'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectKey'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectKeyString'         , { fg = c.purple      , bg = c.blue })

  
hl(0, 'jsBrackets'         , { fg = c.white      , bg = 'NONE' })

  
hl(0, 'jsObjectKeyComputed'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectColon'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectFuncName'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectMethodType'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectStringKey'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsNull'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsUndefined'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsNan'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsSuper'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBlock'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBlockLabel'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBlockLabelKey'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsStatement'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsCommentIfElse'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParenSwitch'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsWhile'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParenWhile'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFor'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParenFor'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsForAwait'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDo'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRepeatBlock'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsLabel'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsSwitchColon'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsSwitchCase'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsTry'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsTryCatchBlock'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFinally'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFinallyBlock'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsCatch'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParenCatch'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsException'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsAsyncKeyword'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsSwitchBlock'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsExceptions'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBuiltins'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFutureKeys'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDomErrNo'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDomNodeConsts'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsHtmlEvents'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsSpreadExpression'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBracket'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParensDecorator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParenDecorator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParensWhile'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsCommentRepeat'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParensFor'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParensSwitch'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParensCatch'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFuncArgExpression'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRestExpression'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowArgumentDef'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsCommentFunction'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowReturn'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsClassMethodType'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsArrowFuncArgs'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsGenerator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDecorator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsClassPropertyComputed'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsClassStringKey'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsTryCatchBraces'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFinallyBraces'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsSwitchBraces'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRepeatBraces'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringBraces'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringProperty'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringAssignment'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringNoise'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringPropertyComputed'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringPropertyValue'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsObjectBraces'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBraces'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsModuleBraces'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsSpreadOperator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRestOperator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsTernaryIfOperator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsTernaryIf'         , { fg = c.purple      , bg = c.blue })


  



  
hl(0, 'jsFlowFunctionGroup'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFuncArgOperator'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsArguments'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsExtendsKeyword'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsClassNoise'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowClassFunctionGroup'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowClassGroup'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsCommentClass'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsFlowClassDef'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringValue'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDestructuringValueAssignment'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsEnvComment'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDecoratorFunction'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDocTags'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsCvsTag'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDocParam'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDocType'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDocTypeNoParam'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDocSeeTag'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDocTypeBrackets'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDocTypeRecord'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsParensRepeat'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsCharacter'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsBranch'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsRepeat'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsError'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDomElemAttrs'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsDomElemFuncs'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsHtmlElemAttrs'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsHtmlElemFuncs'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsCssStyles'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javaScript'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javascriptHtmlEvents'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javascriptDomElemAttrs'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javascriptDomElemFuncs'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javascriptCommentTodo'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javaScriptBlock'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javaScriptLineComment'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javaScriptComment'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javascriptSpreadOp'         , { fg = c.purple      , bg = c.blue })
hl(0, 'jsTemplateStringTag'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javascriptTagRef'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javaScriptSpecial'         , { fg = c.purple      , bg = c.blue })
hl(0, 'javaScriptEmbed'         , { fg = c.purple      , bg = c.blue })



  
------------------------------------------------------------------------------
-- Configuration Files
------------------------------------------------------------------------------
  -- hl(0, 'dosiniLabel', { fg = c.purple, bg = 'NONE'                                  })  -- Settings variable of config files (.editorconfig).
  -- hl(0, 'dosiniValue', { fg = c.blue, bg = 'NONE'                                  })
  -- hl(0, 'dosiniComment', { fg = c.red, bg = 'NONE'                                  })
  -- hl(0, 'dosiniHeader', { fg = c.white, bg = 'NONE'                                  })
  -- hl(0, 'dosiniNumber', { fg = c.greenLight, bg = 'NONE'                                  })
  
  
  hl(0, 'FloatBorder', {
      fg = vim.api.nvim_get_hl_by_name('NormalFloat', true).background,
      bg = vim.api.nvim_get_hl_by_name('NormalFloat', true).background,
  })

  hl(0, 'IndentBlanklineChar', { fg = c.grayDark, bg = 'NONE'                                  })  -- Indent Blank Line plugin vertical lines.
  hl(0, 'SignColumn',          { fg = 'NONE',     bg = c.black                                 })  -- Sign column left of the line numbers.
  hl(0, 'ColorColumn',         { fg = 'NONE',     bg = c.grayDark                              })  -- Vertical visual guide line.
  hl(0, 'LineNr',              { fg = c.gray,     bg = 'NONE'                                  })  -- Line numbers.
  hl(0, 'CursorLineNr',        { fg = c.white,    bg = 'NONE'                                  })  -- Line number of current line.
  hl(0, 'VertSplit',           { fg = c.grayDark, bg = c.grayDark                              })  -- Vertical split separator line.
  hl(0, 'CursorLine',          { fg = 'NONE',     bg = c.grayDark                              })  -- Color of actual line.
  hl(0, 'Search',              { fg = c.black,    bg = c.pink                                  })  -- Highlighting of search patterns.
  hl(0, 'IncSearch',           { fg = c.black,    bg = c.white                                 })  -- Highlighting of first result from search pattern.
  hl(0, 'EndOfBuffer',         { fg = c.black,    bg = 'NONE'                                  })  -- Empty end of buffer.
  hl(0, 'Visual',              { fg = 'NONE',     bg = c.gray                                  })  -- Selected text in visual mode.
  hl(0, 'Pmenu',               { fg = c.white,    bg = c.grayDark                              })  -- Context menus.
  hl(0, 'PmenuSel',            { fg = c.black,    bg = c.gray                                  })  -- Selected item in context menus.
  hl(0, 'MsgArea',             { fg = c.white,    bg = 'NONE'                                  })  -- Area below status line.
  hl(0, 'ErrorMsg',            { fg = c.red,      bg = 'NONE'                                  })  -- Error messages below status line.
  hl(0, 'WarningMsg',          { fg = c.orange,   bg = 'NONE'                                  })  -- Warning messages below status line.
  hl(0, 'Question',            { fg = c.green,    bg = 'NONE'                                  })  -- Questions that Neovim asks the user.
  hl(0, 'NormalFloat',         { fg = c.white,    bg = c.grayDark                              })  -- Neovims regular floating window.
  hl(0, 'FloatBorder',         { fg = 'NONE',     bg = c.grayDark                              })  -- Border of Neovims regular floating window.
  hl(0, 'Whitespace',          { fg = c.red,      bg = 'NONE'                                  })  -- Trailing whitespaces in buffer.
  hl(0, 'PmenuSbar',           { fg = 'NONE',     bg = c.grayDark                              })  -- Scroll bar background in context menus.
  hl(0, 'PmenuThumb',          { fg = c.gray,     bg = 'NONE'                                  })  -- Scroll bar in context menus.
  hl(0, 'MsgSeparator',        { fg = 'NONE',     bg = c.black                                 })  -- Separator line above messages under status line.
  hl(0, 'SpellBad',            { fg = 'NONE',     bg = 'NONE',    sp = c.red, undercurl = true })  -- Spelling mistakes.
  hl(0, 'ModeMsg',             { fg = c.green,    bg = 'NONE'                                  })  -- Messages of Neovim in sepcific modes.
  hl(0, 'MoreMsg',             { fg = c.green,    bg = 'NONE'                                  })  -- Message of Neovim if it asks for more.
  hl(0, "NormalNC", { fg = 'NONE', bg = 'NONE' })  -- Inactive new buffer.
  hl(0, "SpecialKey", { fg = 'NONE', bg = 'NONE' })  -- Left sign column in floating window.


---------------------------- Not used by now:
  hl(0, "Cursor", { fg = c.red, bg = c.green })
  hl(0, "lCursor", { fg = c.red, bg = c.green })
  hl(0, "CursorIM", { fg = c.red, bg = c.green })
  hl(0, "SpellCap", { fg = c.orange, bg = 'NONE', sp = c.yellow, undercurl = true, })
  hl(0, "SpellLocal", { fg = c.blue, bg = c.orange, sp = c.pink, underline = true, })
  hl(0, "SpellRare", { fg = 'NONE', bg = c.blue, sp = c.purple, underline = true, })
  hl(0, "WildMenu", { fg = c.red, bg = c.purple })
  hl(0, "Folded", { fg = c.green, bg = c.orange })
  hl(0, "FoldColumn", { fg = c.red, bg = c.blue })
  hl(0, "CursorColumn", { fg = c.blue, bg = c.red })
  
  hl(0, "VisualNOS", { fg = c.blue, bg = c.green })

  
  hl(0, "QuickFixLine", { fg = c.green, bg = c.blue })

  
  hl(0, "MatchWord", { fg = c.pink, bg = c.purple })
  hl(0, "MatchWordCur", { fg = c.orange, bg = c.green })
  hl(0, "MatchParenCur", { fg = c.purple, bg = c.pink })
  hl(0, "TermCursor", { fg = c.pink, bg = c.purple })

  
  hl(0, "TermCursorNC", { fg = c.white, bg = c.pink })


  
  hl(0, "Conceal", { fg = c.orange, bg = c.purple })
  

  
  hl(0, "Substitute", { fg = c.orange, bg = c.purple })
  hl(0, "NonText", { fg = c.darkGray, bg = c.pink })
  hl(0, "TabLine", { fg = c.red, bg = c.purple })
  hl(0, "TabLineSel", { fg = c.green, bg = c.orange })
  hl(0, "TabLineFill", { fg = c.green, bg = c.blue })


--------------------------------------------------------------
-- Buffer

---------------------------- Not used by now:
  hl(0, "BufferCurrent", { fg = c.green, bg = c.purple })
  hl(0, "BufferCurrentIndex", { fg = c.orange, bg = c.green })
  hl(0, "BufferCurrentMod", { fg = c.purple, bg = c.blue })
  hl(0, "BufferCurrentSign", { fg = c.blue, bg = c.orange })
  hl(0, "BufferCurrentTarget", { fg = c.red, bg = c.green, bold = true, })
  hl(0, "BufferVisible", { fg = c.green, bg = c.pink })
  hl(0, "BufferVisibleIndex", { fg = c.orange, bg = c.purple })
  hl(0, "BufferVisibleMod", { fg = c.purple, bg = c.pink })
  
  hl(0, "BufferVisibleSign", { fg = c.blue, bg = c.pink })
  
  hl(0, "BufferVisibleTarget", { fg = c.red, bg = c.orange, bold = true, })
  hl(0, "BufferInactive", { fg = c.pink, bg = c.green })
  hl(0, "BufferInactiveIndex", { fg = c.blue, bg = c.purple })
  hl(0, "BufferInactiveMod", { fg = c.orange, bg = c.pink })
  hl(0, "BufferInactiveSign", { fg = c.gray, bg = c.yellow })
  hl(0, "BufferInactiveTarget", { fg = c.red, bg = c.green, bold = true, })


--------------------------------------------------------------
-- Status Line

  hl(0, 'StatusLine',   { fg = 'NONE',  bg = c.grayDark })  -- Little edge left to the status line.
  hl(0, 'StatusLineNC', { fg = c.black, bg = c.grayDark })  -- Little edge left to the status line if inactive.

----------------------- Not used by now:
  hl(0, "StatusLineSeparator", { fg = c.green, bg = c.red })
  hl(0, "StatusLineTerm", { fg = c.red, bg = c.black })
  hl(0, "StatusLineTermNC", { fg = c.white, bg = c.purple })


--------------------------------------------------------------
-- Hop

  hl(0, 'HopCursor',    { fg = c.blue,   bg = c.blue })  -- Cursor in Hop mode.
  hl(0, 'HopNextKey',   { fg = c.blue,   bg = 'NONE' })  -- Closest keys with one char.
  hl(0, 'HopNextKey1',  { fg = c.purple, bg = 'NONE' })  -- Next closest keys.
  hl(0, 'HopNextKey2',  { fg = c.blue,   bg = 'NONE' })  -- Many next to each other keys.
  hl(0, 'HopUnmatched', { fg = c.gray,   bg = 'NONE' })  -- Regular not matching text if Hop is active.

  ----------------------- Not used by now:
  hl(0, "HopPreview", { fg = c.pink, bg = c.purple })

  
------------------------------------------------------------------------------
-- Laravel Blade
------------------------------------------------------------------------------

hl(0, 'bladeDelimiter', { fg = c.white, bg = 'NONE' })
hl(0, 'bladeEcho', { fg = c.orange, bg = 'NONE' })

  
--------------------------------------------------------------
-- NvimTree

  hl(0, "NvimTreeRootFolder"               , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "Directory"                        , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeFolderIcon"               , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeIndentMarker"             , { fg = c.gray     , bg = 'NONE'      })
  hl(0, "NvimTreeWinSeparator"             , { fg = c.gray_dark, bg = c.gray_dark })
  hl(0, "NvimTreeNormal"                   , { fg = c.white    , bg = 'NONE'      })
  hl(0, "NvimTreeFolderName"               , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeOpenedFolderName"         , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeEmptyFolderName"          , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeEndOfBuffer"              , { fg = c.black    , bg = 'NONE'      })
  hl(0, "NvimTreeCursorLine"               , { fg = 'NONE'     , bg = c.gray_dark })
  hl(0, "NvimTreeImageFile"                , { fg = c.white    , bg = 'NONE'      })
  hl(0, "NvimTreeGitIgnored"               , { fg = c.gray     , bg = 'NONE'      })
  hl(0, "NvimTreeGitDeleted"               , { fg = c.gray     , bg = 'NONE'      })
  hl(0, "NvimTreeGitRenamed"               , { fg = c.red      , bg = 'NONE'      })
  hl(0, "NvimTreeGitNew"                   , { fg = c.red      , bg = 'NONE'      })
  hl(0, "NvimTreeGitDirty"                 , { fg = c.red      , bg = 'NONE'      })
  hl(0, "NvimTreeGitStaged"                , { fg = c.orange   , bg = 'NONE'      })
  hl(0, "NvimTreeGitMerge"                 , { fg = c.orange   , bg = 'NONE'      })
  hl(0, "NvimTreeLspDiagnosticsError"      , { fg = c.red      , bg = 'NONE'      })
  hl(0, "NvimTreeLspDiagnosticsWarning"    , { fg = c.orange   , bg = 'NONE'      })
  hl(0, "NvimTreeLspDiagnosticsInformation", { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeLspDiagnosticsHint"       , { fg = c.blue     , bg = 'NONE'      })
  hl(0, "NvimTreeFileIgnored"              , { fg = '#606366'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileDeleted"              , { fg = '#606366'  , bg = 'NONE'      })
  hl(0, "nvimtreefilerenamed"              , { fg = '#ff0000'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileNew"                  , { fg = '#FF0000'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileDirty"                , { fg = '#FF0000'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileStaged"               , { fg = '#E8BF6A'  , bg = 'NONE'      })
  hl(0, "NvimTreeFileMerge"                , { fg = '#E8BF6A'  , bg = 'NONE'      })
  hl(0, "NvimTreeModifiedFile"             , { fg = '#E8BF6A'  , bg = 'NONE'      })
  hl(0, "NvimTreeOpenedFile"               , { fg = '#E8BF6A'  , bg = 'NONE'      })
  hl(0, "NvimTreeNormalFloat"              , { fg = '#606366'  , bg = 'NONE'      })
  hl(0, "NvimTreeExecFile"                 , { fg = c.white    , bg = 'NONE'      })
  
  
  hl(0, "NvimTreeSpecialFile", { fg = c.white, bg = 'NONE' })

----------------------------

  hl(0, "NvimTreeVertSplit", { fg = c.red, bg = c.green })

  hl(0, "NvimTreeSymlink", { fg = c.cyan, bg = 'NONE' })

  
  --[[ NvimTreeSymlinkIcon ]]
--[[ NvimTreeSymlinkFolderName   (Directory) ]]
--[[ NvimTreeOpenedFolderIcon    (NvimTreeFolderIcon) ]]
--[[ NvimTreeClosedFolderIcon    (NvimTreeFolderIcon) ]]
--[[ NvimTreeFileIcon ]]
--[[ NvimTreeWindowPicker ]]

--[[ There are also links to normal bindings to style the tree itself. ]]

--[[ NvimTreeNormalFloat ]]
--[[ NvimTreeCursorLineNr    (CursorLineNr) ]]
--[[ NvimTreeLineNr          (LineNr) ]]
--[[ NvimTreeCursorColumn    (CursorColumn) ]]

--[[ There are 2 highlight groups for the live filter feature ]]
--[[ NvimTreeLiveFilterPrefix ]]
--[[ NvimTreeLiveFilterValue ]]

--[[ Color of the bookmark icon ]]
--[[ NvimTreeBookmark ]]

  
--------------------------------------------------------------
-- Lualine

  -- Normal Mode:
  hl(0, "lualine_a_normal", { fg = c.blue, bg = c.grayDark })
  hl(0, "lualine_b_normal", { fg = c.blue, bg = c.grayDark })
  hl(0, "lualine_c_normal", { fg = c.blue, bg = c.grayDark })
  hl(0, "lualine_x_normal", { fg = c.blue, bg = c.grayDark })
  hl(0, "lualine_y_normal", { fg = c.blue, bg = c.grayDark })
  hl(0, "lualine_z_normal", { fg = c.blue, bg = c.grayDark })



  
  hl(0, "lualine_a_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_a_replace", { fg = c.red, bg = c.green, italic = true, })

  hl(0, "lualine_b_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_b_replace", { fg = c.red, bg = c.green, italic = true, })
  
  hl(0, "lualine_c_inactive", { fg = c.red, bg = c.green, italic = true, })



  
  hl(0, "lualine_x_diff_added_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_replace", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_terminal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_added_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_replace", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_terminal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_modified_inactive", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_normal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_insert", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_visual", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_replace", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_command", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_terminal", { fg = c.red, bg = c.green, italic = true, })
  hl(0, "lualine_x_diff_removed_inactive", { fg = c.red, bg = c.green, italic = true, })



------------------------------------------------------------------------------
-- Language Defaults
------------------------------------------------------------------------------

  hl(0, 'Comment',     { fg = c.red,        bg = 'NONE' })  -- Block and line comments.
  hl(0, 'Variable',    { fg = c.purple,     bg = 'NONE' })  -- All kinds of variables.
  hl(0, 'String',      { fg = c.redLight,   bg = 'NONE' })  -- Main setting for everything between quotes.
  hl(0, 'Number',      { fg = c.greenLight, bg = 'NONE' })  -- All kinds of integer numbers.
  hl(0, 'Float',       { fg = c.greenLight, bg = 'NONE' })  -- All kinds of floating point numbers.
  hl(0, 'Boolean',     { fg = c.blue,       bg = 'NONE' })  -- The boolean values 'true' and 'false'.
  hl(0, 'Constant',    { fg = c.purple,     bg = 'NONE' }) -- All kinds of constants.
  hl(0, 'Function',    { fg = c.orange,     bg = 'NONE' })  -- All kinds of functions.
  hl(0, 'Keyword',     { fg = c.blue,       bg = 'NONE' })  -- Keywords of programming languages.
  hl(0, 'Character',   { fg = c.white,      bg = 'NONE' })  -- Regualar characters in a code file.
  hl(0, 'Conditional', { fg = c.blue,       bg = 'NONE' })  -- Conditional statements like 'if', 'else', etc.


  hl(0, "Type", { fg = c.turquoise, bg = 'NONE' })


  hl(0, "Delimiter", { fg = c.blue, bg = 'NONE' })

  hl(0, "Exception", { fg = c.blue, bg = 'NONE' })
  hl(0, "Identifier", { fg = c.purple, bg = 'NONE' })
  hl(0, "Include", { fg = c.blue, bg = 'NONE' })
  hl(0, "MatchParen", { fg = c.white, bg = c.gray })
  hl(0, "Normal", { fg = c.white, bg = c.black })
  hl(0, "Operator", { fg = c.white, bg = 'NONE' })
  hl(0, "PreProc", { fg = c.blue, bg = 'NONE' })
  hl(0, "Special", { fg = c.white, bg = 'NONE' })
  hl(0, "Todo", { fg = c.red, bg = 'NONE', bold = true, })
  hl(0, "Title", { fg = c.white, bg = 'NONE' })
  hl(0, "Error", { fg = c.red, bg = 'NONE', bold = true, })
  hl(0, "Statement", { fg = c.blue, bg = 'NONE' })
  hl(0, "Structure", { fg = c.blue, bg = 'NONE' })

-------------------------------------------------------------

  hl(0, "SpecialChar", { fg = c.white, bg = 'NONE' })
  hl(0, "Repeat", { fg = c.purple, bg = 'NONE' })
  hl(0, "StorageClass", { fg = c.cyan, bg = 'NONE' })
  hl(0, "Typedef", { fg = c.purple, bg = 'NONE' })
  hl(0, "Define", { fg = c.purple, bg = 'NONE' })
  hl(0, "Macro", { fg = c.purple, bg = 'NONE' })
  hl(0, "Debug", { fg = c.gray, bg = 'NONE' })
  hl(0, "Label", { fg = c.blue, bg = 'NONE' })
  hl(0, "SpecialComment", { fg = c.fg, bg = 'NONE' })
  hl(0, "Tag", { fg = c.blue, bg = 'NONE' })
  hl(0, "Bold", { fg = 'NONE', bg = 'NONE', bold = true, })
  hl(0, "Italic", { fg = 'NONE', bg = 'NONE', italic = true, })
  hl(0, "Underlined", { fg = 'NONE', bg = 'NONE', underline = true, })
  hl(0, "Ignore", { fg = c.magenta, bg = 'NONE', bold = true, })
  hl(0, "PreCondit", { fg = c.purple, bg = 'NONE' })


--------------------------------------------------------------
-- Treesitter

  hl(0, "@comment", { fg = c.red, bg = 'NONE' })
  hl(0, "@variable", { fg = c.purple, bg = 'NONE'})
  -- $this keyword:
  hl(0, "@variable.builtin", { fg = c.blue, bg = 'NONE' })
  hl(0, "@string", { link = 'String' })
  hl(0, "@number", { link = 'Number' })
  hl(0, "@float", { link = 'Float' })
  hl(0, "@boolean", { link = 'Boolean' })
  hl(0, "@constant", { link = 'Constant' })
  -- Data type (maybe better in turquoise?):
  hl(0, "@type", { link = 'Type' })
  hl(0, "@function", { link = 'Function' })
  hl(0, "@keyword", { link = 'Keyword' })
  hl(0, "@character", { link = 'Character' })
  hl(0, "@conditional", { link = 'Conditional' })
  hl(0, "@exception", { link = 'Exception' })
  hl(0, "@include", { link = 'Include' })
  hl(0, "@operator", { link = 'Operator' })
  hl(0, "@preproc", { link = 'PreProc' })
  hl(0, "@keyword.return", { link = 'Keyword' })
  hl(0, "@method", { link = 'Function' })
  hl(0, "@method.call", { link = 'Function' })
  hl(0, "@keyword.function", { link = 'Keyword' })
  hl(0, "@function.call", { link = 'Function' })
  hl(0, "@text.todo", { link = 'Todo' })
  hl(0, "@text.title", { link = 'Title' })
  -- Open/close bracket of tags:
  hl(0, "@tag.delimiter", { link = 'Tag' })
  hl(0, "@punctuation.delimiter", { fg = c.white, bg = 'NONE' })
  hl(0, "@punctuation.bracket", { fg = c.white, bg = 'NONE' })
  hl(0, "@punctuation.special", { fg = c.white, bg = 'NONE' })
  hl(0, "@constant.builtin", { link = 'Constant' })
  -- Return types:
  hl(0, "@type.builtin", { fg = c.blue, bg = 'NONE' })
  -- In PHP, every parameter with it's data type:
  hl(0, "@parameter", { fg = 'NONE', bg = 'NONE' })
  hl(0, "@constructor", { fg = c.orange, bg = 'NONE' })
  hl(0, "@type.qualifier", { fg = c.blue, bg = 'NONE' })
  hl(0, "@storageclass", { fg = c.blue, bg = 'NONE' })
  hl(0, "@none", { fg = 'NONE', bg = 'NONE' })
  hl(0, "@tag.attribute", { fg = c.turquoise, bg = 'NONE' })
  -- Path of namespaces:
  hl(0, "@namespace", { fg = c.white, bg = 'NONE' })
  hl(0, "@function.builtin", { fg = c.orange, bg = 'NONE' })
  -- In PHP the @stuff in a doc block:
  hl(0, "@attribute", { fg = c.blue, bg = 'NONE' })
  -- All properties (css classes too):
  hl(0, "@property", { fg = c.purple, bg = 'NONE' })


  hl(0, "@field", { fg = c.purple, bg = 'NONE' })
  hl(0, "@keyword.operator", { fg = c.blue, bg = 'NONE' })
  hl(0, "@string.escape", { fg = c.pink, bg = 'NONE' })

---------------------------------

  hl(0, "@define", { fg = c.orange, bg = c.red })
  hl(0, "@string.regex", { fg = c.blue, bg = c.red })
  hl(0, "@string.special", { fg = c.red, bg = c.orange })
  hl(0, "@character.special", { fg = c.white, bg = c.purple })


  hl(0, "@function.macro", { fg = c.black, bg = c.white })


  hl(0, "@repeat", { link = 'Repeat' })
  hl(0, "@debug", { link = 'Debug' })

  hl(0, "@label", { fg = c.blue, bg = 'NONE' })


  hl(0, "@type.definition", { fg = c.purple, bg = 'NONE' })







  hl(0, "@constant.macro", { fg = c.red, bg = c.green })
  hl(0, "@symbol", { fg = c.blue, bg = c.orange })
  hl(0, "@text", { link = 'None' })
  hl(0, "@text.strong", { link = 'Bold' })
  hl(0, "@text.emphasis", { link = 'Italic' })
  hl(0, "@text.underline", { link = 'Underlined' })
  hl(0, "@text.strike", { fg = 'NONE', bg = 'NONE', strikethrough = true, })
  hl(0, "@text.literal", { link = 'String' })
  hl(0, "@text.uri", { link = 'Underlined' })
  hl(0, "@text.math", { link = 'Special' })
  hl(0, "@text.environment", { link = 'Macro' })
  hl(0, "@text.environment.name", { link = 'Type' })
  hl(0, "@text.reference", { link = 'Constant' })
  hl(0, "@text.note", { link = 'SpecialComment' })
  hl(0, "@text.warning", { link = 'Todo' })
  hl(0, "@text.danger", { link = 'WarningMsg' })
  hl(0, "@tag", { link = 'Tag' })



------------------------------------------------------------------------------
-- CSS
------------------------------------------------------------------------------

  hl(0, "cssAtKeyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssAtRule", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssAttrComma", { fg = c.white, bg = 'NONE' })
  hl(0, "cssAttributeSelector", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssAttrRegion", { fg = c.white, bg = 'NONE' })
  hl(0, "cssBackgroundAttr", { fg = c.orange, bg = 'NONE' })
  hl(0, "cssBackgroundProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssBoxAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssBoxProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssBraceError", { fg = 'NONE', bg = c.red })
  hl(0, "cssBraces", { fg = c.white, bg = 'NONE' })
  hl(0, "cssClassName", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "cssClassNameDot", { fg = c.white, bg = 'NONE' })
  hl(0, "cssColor", { fg = c.white, bg = 'NONE' })
  hl(0, "cssComment", { fg = c.red, bg = 'NONE' })
  hl(0, "cssCommonAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssCustomProp", { fg = c.purple, bg = 'NONE' })
  hl(0, "cssError", { fg = 'NONE', bg = c.red })
  hl(0, "cssFlexibleBoxAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssFlexibleBoxProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssFontAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssFontDescriptorProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssFontProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssFunction", { fg = c.orange, bg = 'NONE' })
  hl(0, "cssFunctionComma", { fg = c.white, bg = 'NONE' })
  hl(0, "cssFunctionName", { fg = c.orange, bg = 'NONE' })
  hl(0, "cssGeneratedContentProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssGridProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssIdentifier", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "cssImportant", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssKeyFrameProp", { fg = c.white, bg = 'NONE' })
  hl(0, "cssListProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssMediaProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssMultiColumnProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssNoise", { fg = c.white, bg = 'NONE' })
  hl(0, "cssPageProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssPositioningAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssPositioningProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssPseudoClass", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "cssPseudoClassId", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssSelectorOp", { fg = c.white, bg = 'NONE' })
  hl(0, "cssSelectorOp2", { fg = c.white, bg = 'NONE' })
  hl(0, "cssStringQQ", { fg = c.red_light, bg = 'NONE' })
  hl(0, "cssTableAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssTagName", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssTextAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssTextProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssTransformProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssTransitionAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssUIAttr", { fg = c.white, bg = 'NONE' })
  hl(0, "cssUIProp", { fg = c.blue, bg = 'NONE' })
  hl(0, "cssUnitDecorators", { fg = c.white, bg = 'NONE' })
  hl(0, "cssValueAngle", { fg = c.green_light, bg = 'NONE' })
  hl(0, "cssValueLength", { fg = c.green_light, bg = 'NONE' })
  hl(0, "cssValueNumber", { fg = c.green_light, bg = 'NONE' })
  hl(0, "cssBorderProp", { fg = c.white, bg = 'NONE' })

----------------------------------

  hl(0, "cssInclude", { fg = c.purple, bg = 'NONE' })
  hl(0, "cssPseudoClassLang", { fg = c.yellow, bg = 'NONE' })
  hl(0, "cssDefinition", { fg = c.fg, bg = 'NONE' })
  hl(0, "cssVendor", { fg = c.orange, bg = 'NONE' })
  hl(0, "cssStyle", { fg = c.fg, bg = 'NONE' })



------------------------------------------------------------------------------
-- HTML
------------------------------------------------------------------------------

  hl(0, "htmlArg", { fg = c.turquoise, bg = 'NONE' })
  hl(0, "htmlEndTag", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlH1", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH2", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH3", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH4", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH5", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlH6", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlLink", { fg = c.white, bg = 'NONE', underline = true, })
  hl(0, "htmlSpecialChar", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlSpecialTagName", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlTag", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlTagN", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlTagName", { fg = c.blue, bg = 'NONE' })
  hl(0, "htmlTitle", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlString", { fg = c.red_light, bg = 'NONE' })
  hl(0, "htmlComment", { fg = c.red, bg = 'NONE' })
  hl(0, "htmlLeadingSpace", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlHead", { fg = c.white, bg = 'NONE' })
  hl(0, "htmlScriptTag", { fg = c.blue, bg = 'NONE' })



------------------------------------------------------------------------------
-- Git
------------------------------------------------------------------------------

  hl(0, "SignAdd", { fg = c.green, bg = 'NONE' })
  hl(0, "SignChange", { fg = c.blue, bg = 'NONE' })
  hl(0, "SignDelete", { fg = c.red, bg = 'NONE' })
  hl(0, "GitSignsAdd", { fg = c.green, bg = 'NONE' })
  hl(0, "GitSignsChange", { fg = c.blue, bg = 'NONE' })
  hl(0, "GitSignsDelete", { fg = c.red, bg = 'NONE' })
  hl(0, "GitignoreSeparator", { fg = c.white, bg = 'NONE' })  -- '/' in .gitignore files.

----------------------- Not used by now:
  hl(0, "DiffText", { fg = c.alt_bg, bg = c.sign_delete })
  hl(0, "DiffAdd", { fg = c.alt_bg, bg = c.sign_add })
  hl(0, "DiffChange", { fg = c.alt_bg, bg = c.sign_change, underline = true, })
  hl(0, "DiffDelete", { fg = c.alt_bg, bg = c.sign_delete })



------------------------------------------------------------------------------
-- JSON
------------------------------------------------------------------------------

  hl(0, "jsonNoise", { fg = c.white, bg = 'NONE' })
  hl(0, "jsonKeyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "jsonKeywordMatch", { fg = c.white, bg = 'NONE' })
  hl(0, "jsonQuote", { fg = c.white, bg = 'NONE' })
  hl(0, "jsonString", { fg = c.red_light, bg = 'NONE' })
  hl(0, "jsonEscape", { fg = c.blue, bg = 'NONE' })
  hl(0, "jsonBoolean", { fg = c.blue, bg = 'NONE' })
  hl(0, "jsonBraces", { fg = c.white, bg = 'NONE' })



------------------------------------------------------------------------------
-- Lua
------------------------------------------------------------------------------

  hl(0, 'luaLocal'          , { fg = c.blue,   bg = 'NONE' })  -- 'local' keyword.
  hl(0, 'luaFuncCall'       , { fg = c.orange, bg = 'NONE' })  -- Function that is called.
  hl(0, 'luaFuncKeyword'    , { fg = c.blue,   bg = 'NONE' })  -- 'function' keyword.
  hl(0, 'luaFloat'         , { fg = c.greenLight      , bg = 'NONE' })  -- Floating point numbers.
  hl(0, 'luaComma'         , { fg = c.white      , bg = 'NONE' })  -- Commata.
  hl(0, 'luaCommentLong'   , { fg = c.red      , bg = 'NONE' })  -- Content of comments.
  hl(0, 'luaCommentLongTag'         , { fg = c.red      , bg = 'NONE' })  -- Tags of comments.
  hl(0, 'luaNoise'         , { fg = c.white      , bg = 'NONE' }) -- Dot opperator.
  hl(0, 'luaSpecialValue'         , { fg = 'NONE'      , bg = 'NONE' })  -- Require function.
  hl(0, 'luaStringLongTag'         , { fg = c.redLight      , bg = 'NONE' })  -- Brackets [] of long string.
  hl(0, 'luaStringLong'         , { fg = c.redLight      , bg = 'NONE' })  -- Content of long strings.
  hl(0, 'luaParens'         , { fg = c.white      , bg = 'NONE' }) -- Parentheses ().
  hl(0, 'luaFuncParens'         , { fg = c.white      , bg = 'NONE' })  -- Parentheses () of function keyword.

  hl(0, 'luaFunction'       , { fg = c.blue       , bg = 'NONE' })
  hl(0, 'luaCond'           , { fg = c.blue       , bg = 'NONE' })
  hl(0, 'luaStatement'      , { fg = c.blue       , bg = 'NONE' })
  hl(0, 'luaOperator'       , { fg = c.blue       , bg = 'NONE' })
  hl(0, 'luaSymbolOperator' , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaComment'        , { fg = c.red        , bg = 'NONE' })
  hl(0, 'luaConstant'       , { fg = c.purple     , bg = 'NONE' })
  hl(0, 'luaString2'        , { fg = c.redLight  , bg = 'NONE' })
  hl(0, 'luaStringDelimiter', { fg = c.redLight  , bg = 'NONE' })
  hl(0, 'luaString'         , { fg = c.redLight  , bg = 'NONE' })
  hl(0, 'luaNumber'         , { fg = c.greenLight, bg = 'NONE' })
  hl(0, 'luaTable'          , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaFunc'           , { fg = c.orange     , bg = 'NONE' })
  hl(0, 'luaBraces'         , { fg = c.white      , bg = 'NONE' })


------------------------- Not used by now:
  hl(0, 'luaBuiltIn'         , { fg = c.white      , bg = c.blue })
  hl(0, 'luaParen'         , { fg = c.white      , bg = c.purple })
  hl(0, 'luaBracket'         , { fg = c.white      , bg = c.white })
  hl(0, 'luaSpecialTable'         , { fg = c.white      , bg = c.turquoise })
  hl(0, 'luaEllipsis'         , { fg = c.white      , bg = c.green })
  hl(0, 'luaError'         , { fg = c.blue      , bg = 'NONE' })
  hl(0, 'luaIfThen'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaBlock'         , { fg = c.purple      , bg = c.blue })
  hl(0, 'luaLoop'         , { fg = c.pink     , bg = 'NONE' })
  hl(0, 'luaGoto'         , { fg = c.white      , bg = c.turquoise })
  hl(0, 'luaLabel'         , { fg = c.white      , bg = c.pink })
  hl(0, 'luaSemiCol'         , { fg = c.turquoise      , bg = c.purple })
  hl(0, 'luaErrHand'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaBrackets'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaNotEqOperator'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaCommentTodo'         , { fg = c.white      , bg = c.purple })
  hl(0, 'luaDocTag'         , { fg = c.turquoise      , bg = 'NONE' })
  hl(0, 'luaFuncSig'         , { fg = c.blue      , bg = 'NONE' })
  hl(0, 'luaFuncId'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaFuncArgs'         , { fg = c.pink      , bg = 'NONE' })
  hl(0, 'luaFuncTable'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaFuncName'         , { fg = c.white      , bg = c.pink })
  hl(0, 'luaFuncArgName'         , { fg = c.blue      , bg = 'NONE' })
  hl(0, 'luaFuncArgComma'         , { fg = c.white      , bg = c.purple })
  hl(0, 'luaThenEnd'         , { fg = c.white      , bg = c.blue })
  hl(0, 'luaElseifThen'         , { fg = c.white      , bg = c.turquoise })
  hl(0, 'luaElse'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaRepeat'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaLoopBlock'         , { fg = c.white      , bg = c.purple })
  hl(0, 'luaIn'         , { fg = c.white      , bg = 'NONE' })
  hl(0, 'luaGotoLabel'         , { fg = c.white      , bg = c.blue })
  hl(0, 'luaStringSpecial'         , { fg = c.purple      , bg = 'NONE' })

  










  -- Whichkey
  --hl(0, "WhichKey", { fg = c.purple, bg = 'NONE' })
  --hl(0, "WhichKeySeperator", { fg = c.green, bg = 'NONE' })
  --hl(0, "WhichKeyGroup", { fg = c.blue, bg = 'NONE' })
  --hl(0, "WhichKeyDesc", { fg = c.fg, bg = 'NONE' })
  --hl(0, "WhichKeyFloat", { fg = 'NONE', bg = c.alt_bg })



------------------------------------------------------------------------------
-- Language Server Protocol (LSP)
------------------------------------------------------------------------------

  hl(0, "DiagnosticInfo", { fg = c.blue, bg = 'NONE' })
  hl(0, "DiagnosticError", { fg = c.red, bg = 'NONE' })
  hl(0, "DiagnosticHint", { fg = c.blue, bg = 'NONE' })
  hl(0, "DiagnosticWarn", { fg = c.orange, bg = 'NONE' })
  hl(0, "DiagnosticVirtualTextHint", { fg = c.blue, bg = c.hint_bg })
  hl(0, "DiagnosticVirtualTextInfo", { fg = c.blue, bg = c.info_bg })
  hl(0, "DiagnosticVirtualTextWarn", { fg = c.orange, bg = c.warn_bg })
  hl(0, "DiagnosticVirtualTextError", { fg = c.red, bg = c.error_bg })
  hl(0, "LspDiagnosticsError", { fg = c.red, bg = 'NONE' })
  hl(0, "LspDiagnosticsWarning", { fg = c.orange, bg = 'NONE' })
  hl(0, "LspDiagnosticsInfo", { fg = c.blue, bg = 'NONE' })
  hl(0, "LspDiagnosticsHint", { fg = c.blue, bg = 'NONE' })
  hl(0, "LspCodeLens", { fg = c.purple, bg = 'NONE', italic = true, })
  hl(0, "LspCodeLensSeparator", { fg = c.purple, bg = 'NONE', italic = true, })
  hl(0, "DiagnosticUnderlineHint", { fg = 'NONE', bg = 'NONE', sp = c.blue, undercurl = true, })
  hl(0, "DiagnosticUnderlineInfo", { fg = 'NONE', bg = 'NONE', sp = c.blue, undercurl = true, })
  hl(0, "DiagnosticUnderlineWarn", { fg = 'NONE', bg = 'NONE', sp = c.orange, undercurl = true, })
  hl(0, "DiagnosticUnderlineError", { fg = 'NONE', bg = 'NONE', sp = c.red, undercurl = true, })

----------------

  hl(0, "DiagnosticOther", { fg = c.purple, bg = 'NONE' })
  hl(0, "DiagnosticSignHint", { link = 'DiagnosticHint' })
  hl(0, "DiagnosticSignInfo", { link = 'DiagnosticInfo' })
  hl(0, "DiagnosticSignWarn", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticSignError", { link = 'DiagnosticError' })
  hl(0, "DiagnosticSignOther", { link = 'DiagnosticOther' })
  hl(0, "DiagnosticSignWarning", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticFloatingHint", { link = 'DiagnosticHint' })
  hl(0, "DiagnosticFloatingInfo", { link = 'DiagnosticInfo' })
  hl(0, "DiagnosticFloatingWarn", { link = 'DiagnosticWarn' })
  hl(0, "DiagnosticFloatingError", { link = 'DiagnosticError' })
  hl(0, "DiagnosticSignInformation", { link = 'DiagnosticInfo' })
  hl(0, "LspDiagnosticsInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsDefaultWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsDefaultInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsDefaultHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsVirtualTextError", { link = 'DiagnosticVirtualTextError' })
  hl(0, "LspDiagnosticsVirtualTextWarning", { link = 'DiagnosticVirtualTextWarn' })
  hl(0, "LspDiagnosticsVirtualTextInformation", { link = 'DiagnosticVirtualTextInfo' })
  hl(0, "LspDiagnosticsVirtualTextInfo", { link = 'DiagnosticVirtualTextInfo' })
  hl(0, "LspDiagnosticsVirtualTextHint", { link = 'DiagnosticVirtualTextHint' })
  hl(0, "LspDiagnosticsFloatingError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsFloatingWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsFloatingInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsFloatingInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsFloatingHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsSignError", { link = 'LspDiagnosticsError' })
  hl(0, "LspDiagnosticsSignWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "LspDiagnosticsSignInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsSignInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "LspDiagnosticsSignHint", { link = 'LspDiagnosticsHint' })
  hl(0, "NvimTreeLspDiagnosticsError", { link = 'LspDiagnosticsError' })
  hl(0, "NvimTreeLspDiagnosticsWarning", { link = 'LspDiagnosticsWarning' })
  hl(0, "NvimTreeLspDiagnosticsInformation", { link = 'LspDiagnosticsInfo' })
  hl(0, "NvimTreeLspDiagnosticsInfo", { link = 'LspDiagnosticsInfo' })
  hl(0, "NvimTreeLspDiagnosticsHint", { link = 'LspDiagnosticsHint' })
  hl(0, "LspDiagnosticsUnderlineError", { link = 'DiagnosticUnderlineError' })
  hl(0, "LspDiagnosticsUnderlineWarning", { link = 'DiagnosticUnderlineWarn' })
  hl(0, "LspDiagnosticsUnderlineInformation", { link = 'DiagnosticUnderlineInfo' })
  hl(0, "LspDiagnosticsUnderlineInfo", { link = 'DiagnosticUnderlineInfo' })
  hl(0, "LspDiagnosticsUnderlineHint", { link = 'DiagnosticUnderlineHint' })
  hl(0, "LspReferenceRead", { fg = 'NONE', bg = c.reference })
  hl(0, "LspReferenceText", { fg = 'NONE', bg = c.reference })
  hl(0, "LspReferenceWrite", { fg = 'NONE', bg = c.reference })
  hl(0, "IlluminatedWordRead", { link = 'LspReferenceRead' })
  hl(0, "IlluminatedWordText", { link = 'LspReferenceText' })
  hl(0, "IlluminatedWordWrite", { link = 'LspReferenceWrite' })



------------------------------------------------------------------------------
-- Markdown
------------------------------------------------------------------------------

  hl(0, "markdownBlockquote", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownBold", { fg = c.white, bg = 'NONE', bold = true, })
  hl(0, "markdownBoldDelimiter", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownCode", { fg = c.green, bg = 'NONE' })
  hl(0, "markdownCodeBlock", { fg = c.green, bg = 'NONE' })
  hl(0, "markdownCodeDelimiter", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownH1", { fg = c.pink, bg = 'NONE' , bold = true })
  hl(0, "markdownH2", { fg = c.pink, bg = 'NONE' , bold = true })
  hl(0, "markdownH3", { fg = c.pink, bg = 'NONE' , bold = true })
  hl(0, "markdownH4", { fg = c.pink, bg = 'NONE' , bold = true })
  hl(0, "markdownH5", { fg = c.pink, bg = 'NONE' , bold = true })
  hl(0, "markdownH6", { fg = c.pink, bg = 'NONE' , bold = true })
  hl(0, "markdownHeadingDelimiter", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownItalic", { fg = c.white, bg = 'NONE', italic = true })
  hl(0, "markdownLinkDelimiter", { fg = c.white, bg = 'NONE' })
  hl(0, "markdownLinkText", { fg = c.white, bg = 'NONE' })
  hl(0, "markdownListMarker", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownOrderedListMarker", { fg = c.blue, bg = 'NONE' })
  if vim.fn.has("nvim-0.7.3") == 1 then
    hl(0, "markdownUrl", { fg = c.blue_link, bg = 'NONE', underdotted = true, })
  else
    hl(0, "markdownUrl", { fg = c.blue_link, bg = 'NONE', underdot = true, })
  end
  hl(0, "mkdHeading", { fg = c.blue, bg = 'NONE' })
  hl(0, "mkdListItem", { fg = c.blue, bg = 'NONE' })

--[[ mkdItalic      xxx cleared ]]
--[[ mkdBold        xxx cleared ]]
--[[ mkdBoldItalic  xxx cleared ]]
--[[ mkdDelimiter   xxx links to Delimiter ]]
--[[ mkdFootnotes   xxx links to htmlLink ]]
--[[ mkdID          xxx links to Identifier ]]
--[[ mkdURL         xxx links to htmlString ]]
--[[ mkdLink        xxx links to htmlLink ]]
--[[ mkdInlineURL   xxx links to htmlLink ]]
--[[ mkdLinkDefTarget xxx links to mkdURL ]]
--[[ mkdLinkDef     xxx links to mkdID ]]
--[[ mkdLinkTitle   xxx links to htmlString ]]
--[[ mkdHeading     xxx cleared ]]
--[[ mkdLineBreak   xxx links to Visual ]]
--[[ mkdBlockquote  xxx links to Comment ]]
--[[ mkdCodeDelimiter xxx links to String ]]
--[[ mkdCode        xxx links to String ]]
--[[ mkdFootnote    xxx links to Comment ]]
--[[ mkdListItem    xxx links to Identifier ]]
--[[ mkdListItemLine xxx cleared ]]
--[[ mkdNonListItemBlock xxx cleared ]]
--[[ mkdRule        xxx links to Identifier ]]
--[[ mkdMath        xxx cleared ]]
--[[ mkdStrike      xxx cleared ]]
--[[ mkdString      xxx links to String ]]
--[[ mkdCodeStart   xxx links to String ]]
--[[ mkdCodeEnd     xxx links to String ]]
  hl(0, "markdownHeadingRule", { fg = c.fg, bg = 'NONE', bold = true, })
  hl(0, "markdownId", { link = 'Identifier' })
  hl(0, "markdownIdDeclaration", { fg = c.blue, bg = 'NONE' })
  hl(0, "markdownIdDelimiter", { fg = c.light_gray, bg = 'NONE' })
  hl(0, "markdownBoldItalic", { fg = c.yellow, bg = 'NONE', bold = true, italic = true, })
  hl(0, "markdownRule", { fg = c.gray, bg = 'NONE' })
  hl(0, "markdownFootnote", { fg = c.orange, bg = 'NONE' })
  hl(0, "markdownFootnoteDefinition", { fg = c.orange, bg = 'NONE' })
  hl(0, "markdownEscape", { fg = c.yellow, bg = 'NONE' })









  -- Quickscope
  hl(0, "QuickScopePrimary", { fg = '#ff007c', bg = 'NONE', underline = true, })
  hl(0, "QuickScopeSecondary", { fg = '#00dfff', bg = 'NONE', underline = true, })

  -- Telescope
  hl(0, "TelescopeSelection", { fg = 'NONE', bg = c.ui2_blue })
  hl(0, "TelescopeSelectionCaret", { fg = c.red, bg = c.ui2_blue })
  hl(0, "TelescopeMatching", { fg = c.info, bg = 'NONE', bold = true, italic = true, })
  hl(0, "TelescopeBorder", { fg = c.alt_fg, bg = 'NONE' })
  hl(0, "TelescopeNormal", { fg = c.fg, bg = c.menu_bg })
  hl(0, "TelescopePromptPrefix", { fg = c.hint, bg = 'NONE' })
  hl(0, "TelescopePromptTitle", { fg = c.ui_orange, bg = 'NONE', bold = true, })
  hl(0, "TelescopeResultsTitle", { fg = c.ui_orange, bg = 'NONE', bold = true, })
  hl(0, "TelescopePreviewTitle", { fg = c.ui_orange, bg = 'NONE', bold = true, })
  hl(0, "TelescopePromptCounter", { fg = c.red, bg = 'NONE' })
  hl(0, "TelescopePreviewHyphen", { fg = c.red, bg = 'NONE' })

  -- Lir
  hl(0, "LirFloatNormal", { fg = c.fg, bg = c.alt_bg })
  hl(0, "LirDir", { link = 'Directory' })
  hl(0, "LirSymLink", { fg = c.cyan, bg = 'NONE' })
  hl(0, "LirEmptyDirText", { fg = c.gray, bg = 'NONE', italic = true, })

  -- IndentBlankline
  --hl(0, "IndentBlanklineContextChar", { fg = c.context, bg = 'NONE' })
  --hl(0, "IndentBlanklineContextStart", { fg = 'NONE', bg = 'NONE', underline = true, })
  --hl(0, "IndentBlanklineChar", { fg = c.dark_gray, bg = 'NONE' })

  -- Dashboard
  --hl(0, "DashboardHeader", { fg = c.blue, bg = 'NONE' })
  --hl(0, "DashboardCenter", { fg = c.purple, bg = 'NONE' })
  --hl(0, "DashboardFooter", { fg = c.cyan, bg = 'NONE' })

  -- DiffView
  hl(0, "DiffViewNormal", { fg = c.gray, bg = c.alt_bg })
  hl(0, "DiffviewStatusAdded", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "DiffviewStatusModified", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "DiffviewStatusRenamed", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "DiffviewStatusDeleted", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "DiffviewFilePanelInsertion", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "DiffviewFilePanelDeletion", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "DiffviewVertSplit", { fg = 'NONE', bg = c.bg })

  -- Bookmarks
  hl(0, "BookmarkSign", { fg = c.sign_change, bg = 'NONE' })
  hl(0, "BookmarkAnnotationSign", { fg = c.yellow, bg = 'NONE' })
  hl(0, "BookmarkLine", { fg = c.ui2_blue, bg = 'NONE' })
  hl(0, "BookmarkAnnotationLine", { fg = c.ui2_blue, bg = 'NONE' })

  -- Bqf
  hl(0, "BqfPreviewBorder", { fg = c.fg, bg = 'NONE' })
  hl(0, "BqfPreviewRange", { fg = 'NONE', bg = c.ui2_blue })
  hl(0, "BqfSign", { fg = c.ui_orange, bg = 'NONE' })



------------------------------------------------------------------------------
-- CMP
------------------------------------------------------------------------------

  hl(0, "CmpItemAbbrMatch", { fg = c.black, bg = c.gray })
  hl(0, "CmpItemAbbrDeprecated", { fg = c.gray, bg = 'NONE', strikethrough = true, })

---------------------

  hl(0, "CmpItemAbbrMatchFuzzy", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindFunction", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindMethod", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindConstructor", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindClass", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindEnum", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindEvent", { fg = c.info, bg = 'NONE' })
  hl(0, "CmpItemKindInterface", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindStruct", { fg = c.orange, bg = 'NONE' })
  hl(0, "CmpItemKindVariable", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindField", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindProperty", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindEnumMember", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindConstant", { fg = c.purple, bg = 'NONE' })
  hl(0, "CmpItemKindKeyword", { fg = c.blue, bg = 'NONE' })
  hl(0, "CmpItemKindModule", { fg = c.cyan, bg = 'NONE' })
  hl(0, "CmpItemKindValue", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindUnit", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindText", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindSnippet", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindFile", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindFolder", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindColor", { fg = c.fg, bg = 'NONE' })
  hl(0, "CmpItemKindReference", { fg = c.light_blue, bg = 'NONE' })
  hl(0, "CmpItemKindOperator", { fg = c.white, bg = 'NONE' })
  hl(0, "CmpItemKindTypeParameter", { fg = c.blue, bg = 'NONE' })



------------------------------------------------------------------------------
-- Navic
------------------------------------------------------------------------------

  hl(0, "NavicIconsFile", { link = 'CmpItemKindFile' })
  hl(0, "NavicIconsModule", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsNamespace", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsPackage", { link = 'CmpItemKindModule' })
  hl(0, "NavicIconsClass", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsMethod", { link = 'CmpItemKindMethod' })
  hl(0, "NavicIconsProperty", { link = 'CmpItemKindProperty' })
  hl(0, "NavicIconsField", { link = 'CmpItemKindField' })
  hl(0, "NavicIconsConstructor", { link = 'CmpItemKindConstructor' })
  hl(0, "NavicIconsEnum", { link = 'CmpItemKindEnum' })
  hl(0, "NavicIconsInterface", { link = 'CmpItemKindInterface' })
  hl(0, "NavicIconsFunction", { link = 'CmpItemKindFunction' })
  hl(0, "NavicIconsVariable", { link = 'CmpItemKindVariable' })
  hl(0, "NavicIconsConstant", { link = 'CmpItemKindConstant' })
  hl(0, "NavicIconsString", { link = 'String' })
  hl(0, "NavicIconsNumber", { link = 'Number' })
  hl(0, "NavicIconsBoolean", { link = 'Boolean' })
  hl(0, "NavicIconsArray", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsObject", { link = 'CmpItemKindClass' })
  hl(0, "NavicIconsKey", { link = 'CmpItemKindKeyword' })
  hl(0, "NavicIconsKeyword", { link = 'CmpItemKindKeyword' })
  hl(0, "NavicIconsNull", { fg = c.blue, bg = 'NONE' })
  hl(0, "NavicIconsEnumMember", { link = 'CmpItemKindEnumMember' })
  hl(0, "NavicIconsStruct", { link = 'CmpItemKindStruct' })
  hl(0, "NavicIconsEvent", { link = 'CmpItemKindEvent' })
  hl(0, "NavicIconsOperator", { link = 'CmpItemKindOperator' })
  hl(0, "NavicIconsTypeParameter", { link = 'CmpItemKindTypeParameter' })
  hl(0, "NavicText", { fg = c.gray, bg = 'NONE' })
  hl(0, "NavicSeparator", { fg = c.context, bg = 'NONE' })



------------------------------------------------------------------------------
-- Notify
------------------------------------------------------------------------------

  hl(0, "NotifyERRORBorder", { fg = '#8A1F1F', bg = 'NONE' })
  hl(0, "NotifyWARNBorder", { fg = '#79491D', bg = 'NONE' })
  hl(0, "NotifyINFOBorder", { fg = c.ui_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGBorder", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACEBorder", { fg = '#4F3552', bg = 'NONE' })
  hl(0, "NotifyERRORIcon", { fg = c.error, bg = 'NONE' })
  hl(0, "NotifyWARNIcon", { fg = c.warn, bg = 'NONE' })
  hl(0, "NotifyINFOIcon", { fg = c.ui4_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGIcon", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACEIcon", { fg = c.ui_purple, bg = 'NONE' })
  --[[ hl(0, "NotifyERRORTitle", { fg = c.error, bg = 'NONE' })
  hl(0, "NotifyWARNTitle", { fg = c.warn, bg = 'NONE' })
  hl(0, "NotifyINFOTitle", { fg = c.ui4_blue, bg = 'NONE' })
  hl(0, "NotifyDEBUGTitle", { fg = c.gray, bg = 'NONE' })
  hl(0, "NotifyTRACETitle", { fg = c.ui_purple, bg = 'NONE' }) ]]



------------------------------------------------------------------------------
-- Packer
------------------------------------------------------------------------------

  hl(0, "packerString", { fg = c.ui_orange, bg = 'NONE' })
  hl(0, "packerHash", { fg = c.ui4_blue, bg = 'NONE' })
  hl(0, "packerOutput", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "packerRelDate", { fg = c.gray, bg = 'NONE' })
  hl(0, "packerSuccess", { fg = c.success_green, bg = 'NONE' })
  hl(0, "packerStatusSuccess", { fg = c.ui4_blue, bg = 'NONE' })



------------------------------------------------------------------------------
-- PHP
------------------------------------------------------------------------------

hl(0, "phpTodo"                   , { fg = c.red        , bg = 'NONE' })
hl(0, "phpComment"                , { fg = c.red        , bg = 'NONE' })
hl(0, "phpRegion"                 , { fg = c.turquoise  , bg = 'NONE' })
hl(0, "phpInclude"                , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpClass"                  , { fg = c.turquoise  , bg = 'NONE' })
hl(0, "phpClasses"                , { fg = c.turquoise  , bg = 'NONE' })
hl(0, "phpFunction"               , { fg = c.orange     , bg = 'NONE' })
hl(0, "phpType"                   , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpKeyword"                , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpVarSelector"            , { fg = c.purple     , bg = 'NONE' })
hl(0, "phpIdentifier"             , { fg = c.purple     , bg = 'NONE' })
hl(0, "phpMethod"                 , { fg = c.orange     , bg = 'NONE' })
hl(0, "phpMethodsVar"             , { fg = c.purple     , bg = 'NONE' })
hl(0, "phpMemberSelector"         , { fg = c.white      , bg = 'NONE' })
hl(0, "phpStorageClass"           , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpDefine"                 , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpSpecialFunction"        , { fg = c.orange     , bg = 'NONE' })
hl(0, "phpParent"                 , { fg = c.white      , bg = 'NONE' })
hl(0, "phpSuperglobals"           , { fg = c.purple     , bg = 'NONE' })
hl(0, "phpMagicConstants"         , { fg = c.purple     , bg = 'NONE' })
hl(0, "phpServerVars"             , { fg = c.red        , bg = c.green })
hl(0, "phpConstants"              , { fg = c.purple     , bg = 'NONE' })
hl(0, "phpFunctions"              , { fg = c.orange     , bg = 'NONE' })
hl(0, "phpDocComment"             , { fg = c.green      , bg = 'NONE' })
hl(0, "phpStringDouble"           , { fg = c.red_light  , bg = 'NONE' })
hl(0, "phpStringSingle"           , { fg = c.red_light  , bg = 'NONE' })
hl(0, "phpStatement"              , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpNullValue"              , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpOperator"               , { fg = c.white      , bg = 'NONE' })
hl(0, "phpMethods"                , { fg = c.orange     , bg = 'NONE' })
hl(0, "phpSplatOperator"          , { fg = c.green      , bg = c.red })
hl(0, "phpIdentifierSimply"       , { fg = c.orange           , bg = c.red })--xxx links to Identifier
hl(0, "phpSpecialChar"            , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpBacktick"               , { fg = c.purple           , bg = c.green_light})--xxx links to String
hl(0, "phpStrEsc"                 , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpIdentifierComplex"      , { fg = c.red           , bg = c.blue })--xxx cleared
hl(0, "phpBoolean"                , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpNumber"                 , { fg = c.green_light, bg = 'NONE' })
hl(0, "phpOctalError"             , { fg = c.purple           , bg = c.red })--xxx links to Error
hl(0, "phpHereDoc"                , { fg = c.red_light  , bg = 'NONE' })
hl(0, "phpCommentStar"            , { fg = c.green      , bg = 'NONE' })
hl(0, "phpCommentTitle"           , { fg = c.green      , bg = 'NONE' })
hl(0, "phpDocTags"                , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpDocParam"               , { fg = c.green      , bg = 'NONE' })
hl(0, "phpDocIdentifier"          , { fg = c.purple     , bg = 'NONE' })
hl(0, "phpDocNamespaceSeparator"  , { fg = c.white      , bg = 'NONE' })
hl(0, "phpStringDelimiter"        , { fg = c.red_light  , bg = 'NONE' })
hl(0, "phpNowDoc"                 , { fg = c.green           , bg = c.blue })--xxx links to String
hl(0, "phpStaticClasses"          , { fg = c.turquoise  , bg = 'NONE' })
hl(0, "phpClassNamespaceSeparator", { fg = c.purple           , bg = c.orange })--xxx links to phpClass
hl(0, "phpClassExtends"           , { fg = c.turquoise  , bg = 'NONE' })
hl(0, "phpClassImplements"        , { fg = c.red           , bg = c.green_light })--xxx links to phpClass
hl(0, "phpClassDelimiter"         , { fg = c.purple           , bg = c.white })--xxx links to phpRegion
hl(0, "phpUseNamespaceSeparator"  , { fg = c.white      , bg = 'NONE' })
hl(0, "phpUseFunction"            , { fg = c.turquoise           , bg = c.red })--xxx cleared
hl(0, "phpUseClass"               , { fg = c.turquoise  , bg = 'NONE' })
hl(0, "phpUseKeyword"             , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpUseAlias"               , { fg = c.turquoise  , bg = 'NONE' })
hl(0, "phpYieldFromKeyword"       , { fg = c.blue       , bg = 'NONE' })
hl(0, "phpStructure"              , { fg = c.turquoise           , bg = c.yellow })--xxx links to Statement
hl(0, "phpException"              , { fg = c.turquoise           , bg = c.blue })--xxx links to Exception
hl(0, "phpParentError"            , { fg = c.red_light           , bg = c.green_light })--xxx links to Error
hl(0, "phpFoldIfContainer"        , { fg = c.orange           , bg = 'NONE' })-- xxx cleared
hl(0, "phpFoldWhile"              , { fg = c.turquoise           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldDoWhile"            , { fg = c.red_light           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldFor"                , { fg = c.turquoise           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldForeach"            , { fg = c.red           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldTryContainer"       , { fg = c.turquoise           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldSwitch"             , { fg = c.green           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldFunction"           , { fg = c.turquoise           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldClass"              , { fg = c.red_light           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldInterface"          , { fg = c.red           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldHtmlInside"         , { fg = c.turquoise           , bg = 'NONE' })--xxx cleared
hl(0, "phpSCKeyword"              , { fg = c.green           , bg = 'NONE' })--xxx links to phpKeyword
hl(0, "phpFCKeyword"              , { fg = c.red           , bg = 'NONE' })--xxx links to phpKeyword
hl(0, "phpFoldCatch"              , { fg = c.orange           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldFinally"            , { fg = c.red           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldElseIf"             , { fg = c.green           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldElse"               , { fg = c.orange           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldCase"               , { fg = c.red           , bg = 'NONE' })--xxx cleared
hl(0, "phpFoldDefault"            , { fg = c.orange           , bg = 'NONE' })--xxx cleared
hl(0, "phpBackslashSequences"     , { fg = c.green           , bg = 'NONE' })--xxx cleared






  -- SymbolOutline
  hl(0, "SymbolsOutlineConnector", { fg = c.gray, bg = 'NONE' })
  hl(0, "FocusedSymbol", { fg = 'NONE', bg = '#36383F' })

  -- hl(0, "NeogitFold", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitStash", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitDiffAdd", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitObjectId", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitRebasing", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitDiffDelete", { fg = c.ui_purple, bg = 'NONE' })
  -- hl(0, "NeogitRebaseDone", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitBranch", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitRemote", { fg = c.yellow_orange, bg = 'NONE' })
  hl(0, "NeogitStashes", { fg = c.ui_purple, bg = 'NONE' })
  hl(0, "NeogitUnmergedInto", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnpulledFrom", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitRecentcommits", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitStagedchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUntrackedfiles", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnmergedchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnpulledchanges", { fg = c.blue, bg = 'NONE' })
  hl(0, "NeogitUnstagedchanges", { fg = c.blue, bg = 'NONE' })
  -- hl(0, "NoiceCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIcon", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconFilter", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconHelp", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconIncRename", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconInput", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconLua", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlineIconSearch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopup", { fg = c.hint, bg = c.hint })
hl(0, "NoiceCmdlinePopupBorder", { fg = c.hint, bg = "NONE" })
  -- hl(0, "NoiceCmdlinePopupBorderCmdline", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderFilter", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderHelp", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderIncRename", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderInput", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderLua", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePopupBorderSearch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCmdlinePrompt", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindClass", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindColor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindConstant", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindConstructor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindDefault", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindEnum", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindEnumMember ", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindField", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFile", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFolder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindFunction", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindInterface", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindKeyword", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindMethod", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindModule", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindProperty", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindSnippet", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindStruct", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindText", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindUnit", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindValue", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemKindVariable", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemMenu", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCompletionItemWord", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceConfirm", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceConfirmBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceCursor", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatConfirm", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatConfirmDefault", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatDate", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatEvent", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatKind", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelDebug", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelError", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelInfo", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelOff", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelTrace", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatLevelWarn", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatProgressDone", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatProgressTodo", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceFormatTitle", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressClient", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressSpinner", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceLspProgressTitle", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceMini", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopup", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenu", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuBorder", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuMatch", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoicePopupmenuSelected", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceScrollbar", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceScrollbarThumb", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceSplit", { fg = c.hint, bg = c.hint })
  -- hl(0, "NoiceSplitBorder", { fg = c.hint, bg = c.hint })
  hl(0, "NoiceVirtualText", { fg = c.hint, bg = c.hint_bg })
  -- Noice

  -- TreesitterContext
  hl(0, "TreesitterContext", { fg = 'NONE', bg = c.alt_bg })

  -- Crates
  hl(0, "CratesNvimLoading", { fg = c.hint, bg = 'NONE' })
  hl(0, "CratesNvimVersion", { fg = c.hint, bg = 'NONE' })

  -- Misc
  hl(0, "diffAdded", { fg = c.sign_add, bg = 'NONE' })
  hl(0, "diffRemoved", { fg = c.sign_delete, bg = 'NONE' })
  hl(0, "diffFileId", { fg = c.blue, bg = 'NONE', bold = true, reverse = true, })
  hl(0, "diffFile", { fg = c.alt_bg, bg = 'NONE' })
  hl(0, "diffNewFile", { fg = c.green, bg = 'NONE' })
  hl(0, "diffOldFile", { fg = c.red, bg = 'NONE' })
  hl(0, "debugPc", { fg = 'NONE', bg = c.ui5_blue })
  hl(0, "debugBreakpoint", { fg = c.red, bg = 'NONE', reverse = true, })
  hl(0, "CodiVirtualText", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunVirtualTextOk", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunFloatingWinOk", { fg = c.hint, bg = 'NONE' })
  hl(0, "SniprunVirtualTextErr", { fg = c.error, bg = 'NONE' })
  hl(0, "SniprunFloatingWinErr", { fg = c.error, bg = 'NONE' })
  hl(0, "DapBreakpoint", { fg = c.error, bg = 'NONE' })

  -- Language

  -- hl(0, "yamlPlainScalar", { fg = c.orange, bg = 'NONE' })
  -- hl(0, "yamlTSField", { fg = c.blue, bg = 'NONE' })
  hl(0, "hclTSPunctSpecial", { fg = c.alt_fg, bg = 'NONE' })
  hl(0, "yamlBlockMappingKey", { fg = c.blue, bg = 'NONE' })
  hl(0, "tomlTSProperty", { fg = c.blue, bg = 'NONE' })
  hl(0, "zshKSHFunction", { link = "Function" })
  hl(0, "zshVariableDef", { link = "Constant" })



------------------------------------------------------------------------------
-- XML
------------------------------------------------------------------------------

hl(0, 'xmlTag'            , { fg = c.blue , bg = 'NONE' })
hl(0, 'xmlTagName'        , { fg = c.blue , bg = 'NONE' })
hl(0, 'xmlEndTag'         , { fg = c.blue , bg = 'NONE' })
hl(0, 'xmlProcessingDelim', { fg = c.blue , bg = 'NONE' })
hl(0, 'xmlAttribPunct'    , { fg = c.white, bg = 'NONE' })
--[[ xmlError       xxx links to Error ]]
--[[ xmlEntity      xxx links to Statement ]]
--[[ xmlString      xxx links to String ]]
--[[ xmlEqual       xxx cleared ]]
--[[ xmlAttrib      xxx links to Type ]]
--[[ xmlNamespace   xxx links to Tag ]]
--[[ xmlCdata       xxx links to String ]]
--[[ xmlRegion      xxx cleared ]]
--[[ xmlComment     xxx links to Comment ]]
--[[ xmlProcessing  xxx links to Type ]]
--[[ xmlEntityPunct xxx links to Type ]]
--[[ xmlCommentStart xxx links to xmlComment ]]
--[[ xmlCommentError xxx links to Error ]]
--[[ xmlCommentPart xxx links to Comment ]]
--[[ xmlTodo        xxx links to Todo ]]
--[[ xmlCdataStart  xxx links to Type ]]
--[[ xmlCdataEnd    xxx links to Type ]]
--[[ xmlCdataCdata  xxx links to Statement ]]
--[[ xmlDocTypeKeyword xxx links to Statement ]]
--[[ xmlInlineDTD   xxx links to Function ]]
--[[ xmlDocTypeDecl xxx links to Function ]]
--[[ xmlDocType     xxx cleared ]]


end

return theme
