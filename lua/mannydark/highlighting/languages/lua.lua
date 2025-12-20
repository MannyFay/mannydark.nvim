-------------------------------------------------------------------------------
-- Lua
-- Uses links to languagedefaults.lua for consistency
-------------------------------------------------------------------------------

local highlight = vim.api.nvim_set_hl
local lua       = {}

lua.setupHighlighting = function()

  -----------------------------------------------------------------------------
  -- Vim Legacy Syntax Groups (link to base groups)
  -----------------------------------------------------------------------------

  -- Keywords & Statements (link to Keyword/Statement)
  highlight(0, 'luaStatement',            { link = 'Statement' })
  highlight(0, 'luaRepeat',               { link = 'Repeat' })
  highlight(0, 'luaFor',                  { link = 'Repeat' })
  highlight(0, 'luaWhile',                { link = 'Repeat' })
  highlight(0, 'luaCond',                 { link = 'Conditional' })
  highlight(0, 'luaCondElse',             { link = 'Conditional' })
  highlight(0, 'luaCondElseif',           { link = 'Conditional' })
  highlight(0, 'luaCondEnd',              { link = 'Keyword' })
  highlight(0, 'luaCondStart',            { link = 'Conditional' })
  highlight(0, 'luaFunction',             { link = "Function" })
  highlight(0, 'luaFuncKeyword',          { link = "Keyword" })
  highlight(0, 'luaLocal',                { link = 'Keyword' })
  highlight(0, 'luaIn',                   { link = 'Keyword' })
  highlight(0, 'luaThen',                 { link = 'Keyword' })
  highlight(0, 'luaDo',                   { link = 'Keyword' })
  highlight(0, 'luaEnd',                  { link = 'Keyword' })
  highlight(0, 'luaReturn',               { link = 'Keyword' })
  highlight(0, 'luaBreak',                { link = 'Keyword' })
  highlight(0, 'luaGoto',                 { link = 'Keyword' })
  highlight(0, 'luaLabel',                { link = 'Label' })

  -- Operators (link to Operator)
  highlight(0, 'luaOperator',             { link = "Operator" })           -- Word operators (and, or, not)
  highlight(0, 'luaSymbolOperator',       { link = "Operator" })
  highlight(0, 'luaBinaryOperator',       { link = "Operator" })
  highlight(0, 'luaUnaryOperator',        { link = "Operator" })
  highlight(0, 'luaRelationalOperator',   { link = "Operator" })
  highlight(0, 'luaLengthOperator',       { link = "Operator" })
  highlight(0, 'luaConcatOperator',       { link = "Operator" })
  highlight(0, 'luaNotEqOperator',        { link = "Operator" })
  highlight(0, 'luaEqOperator',           { link = "Operator" })

  -- Strings (link to String)
  highlight(0, 'luaString',               { link = "String" })
  highlight(0, 'luaString2',              { link = "String" })
  highlight(0, 'luaStringDelimiter',      { link = "Delimiter" })
  highlight(0, 'luaStringLong',           { link = "String" })
  highlight(0, 'luaStringLongTag',        { link = "String" })
  highlight(0, 'luaStringSpecial',        { link = "String" })
  highlight(0, 'luaCharacter',            { link = 'Character' })

  -- Numbers (link to Number)
  highlight(0, 'luaNumber',               { link = "Number" })
  highlight(0, 'luaFloat',                { link = 'Float' })
  highlight(0, 'luaHexNumber',            { link = "Number" })

  -- Constants & Booleans (link to Constant/Boolean)
  highlight(0, 'luaConstant',             { link = "Constant" })
  highlight(0, 'luaNil',                  { link = 'Constant' })
  highlight(0, 'luaTrue',                 { link = 'Boolean' })
  highlight(0, 'luaFalse',                { link = 'Boolean' })
  highlight(0, 'luaBoolean',              { link = "Boolean" })

  -- Functions (link to Function)
  highlight(0, 'luaFunc',                 { link = "Function" })
  highlight(0, 'luaFuncCall',             { link = "Function" })
  highlight(0, 'luaFuncName',             { link = "Function" })
  highlight(0, 'luaFuncId',               { link = "Function" })
  highlight(0, 'luaFuncSig',              { link = "Function" })
  highlight(0, 'luaFuncArgs',             { link = "Function" })
  highlight(0, 'luaFuncArgName',          { link = "Function" })
  highlight(0, 'luaFuncParens',           { link = "Function" })
  highlight(0, 'luaFuncArgComma',         { link = "Function" })
  highlight(0, 'luaFuncTable',            { link = "Function" })
  highlight(0, 'luaMetaMethod',           { link = "Function" })
  highlight(0, 'luaSpecialValue',         { link = 'Function' })

  -- Tables (link to Delimiter/Identifier)
  highlight(0, 'luaTable',                { link = 'Delimiter' })
  highlight(0, 'luaTableBlock',           { link = 'Delimiter' })
  highlight(0, 'luaTableConstructor',     { link = 'Delimiter' })
  highlight(0, 'luaTableField',           { link = '@property' })
  highlight(0, 'luaSpecialTable',         { link = '@variable.builtin' })

  -- Comments (link to Comment)
  highlight(0, 'luaComment',              { link = "Comment" })
  highlight(0, 'luaCommentDelimiter',     { link = "Delimiter" })
  highlight(0, 'luaCommentLong',          { link = "Comment" })
  highlight(0, 'luaCommentLongTag',       { link = "Comment" })
  highlight(0, 'luaCommentTodo',          { link = "Comment" })
  highlight(0, 'luaTodo',                 { link = 'Todo' })

  -- Punctuation (link to Delimiter)
  highlight(0, 'luaParens',               { link = 'Delimiter' })
  highlight(0, 'luaBraces',               { link = 'Delimiter' })
  highlight(0, 'luaBrackets',             { link = 'Delimiter' })
  highlight(0, 'luaComma',                { link = 'Delimiter' })
  highlight(0, 'luaSemiCol',              { link = 'Delimiter' })
  highlight(0, 'luaNoise',                { link = 'Delimiter' })

  -- Errors (link to Error)
  highlight(0, 'luaError',                { link = 'Error' })
  highlight(0, 'luaParenError',           { link = 'Error' })
  highlight(0, 'luaBraceError',           { link = 'Error' })


  -----------------------------------------------------------------------------
  -- Treesitter Captures (link to base @xxx groups)
  -----------------------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.lua',                     { link = "Variable" })
  highlight(0, '@variable.builtin.lua',             { link = "Variable" })
  highlight(0, '@variable.member.lua',              { link = "Variable" })
  highlight(0, '@variable.parameter.lua',           { link = "Variable" })
  highlight(0, '@variable.parameter.builtin.lua',   { link = "Variable" })

  -- Constants
  highlight(0, '@constant.lua',                     { link = "Constant" })
  highlight(0, '@constant.builtin.lua',             { link = "Constant" })

  -- Booleans
  highlight(0, '@boolean.lua',                      { link = "Boolean" })

  -- Numbers
  highlight(0, '@number.lua',                       { link = "Number" })
  highlight(0, '@number.float.lua',                 { link = "Number" })

  -- Strings
  highlight(0, '@string.lua',                       { link = "String" })
  highlight(0, '@string.escape.lua',                { link = "String" })
  highlight(0, '@string.special.lua',               { link = "String" })

  -- Functions
  highlight(0, '@function.lua',                     { link = "Function" })
  highlight(0, '@function.builtin.lua',             { link = "Function" })
  highlight(0, '@function.call.lua',                { link = "Function" })
  highlight(0, '@function.method.lua',              { link = "Function" })
  highlight(0, '@function.method.call.lua',         { link = "Function" })

  -- Keywords
  highlight(0, '@keyword.lua',                      { link = "Keyword" })
  highlight(0, '@keyword.function.lua',             { link = "Keyword" })
  highlight(0, '@keyword.operator.lua',             { link = "Operator" })
  highlight(0, '@keyword.return.lua',               { link = "Keyword" })
  highlight(0, '@keyword.repeat.lua',               { link = "Keyword" })
  highlight(0, '@keyword.conditional.lua',          { link = "Conditional" })

  -- Types
  highlight(0, '@type.lua',                         { link = "Type" })
  highlight(0, '@type.builtin.lua',                 { link = "Type" })

  -- Comments
  highlight(0, '@comment.lua',                      { link = "Comment" })
  highlight(0, '@comment.documentation.lua',        { link = "Comment" })

  -- Operators
  highlight(0, '@operator.lua',                     { link = "Operator" })

  -- Punctuation
  highlight(0, '@punctuation.delimiter.lua',        { link = "Delimiter" })
  highlight(0, '@punctuation.bracket.lua',          { link = '@punctuation.bracket' })
  highlight(0, '@punctuation.special.lua',          { link = '@punctuation.special' })

  -- Properties
  highlight(0, '@property.lua',                     { link = '@property' })

  -- Modules
  highlight(0, '@module.lua',                       { link = '@module' })
  highlight(0, '@module.builtin.lua',               { link = '@module.builtin' })

  -- Labels
  highlight(0, '@label.lua',                        { link = '@label' })

  -- Constructors
  highlight(0, '@constructor.lua',                  { link = '@constructor' })


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (link to base @lsp.type groups)
  -----------------------------------------------------------------------------

  highlight(0, '@lsp.type.variable.lua',            { link = "Variable" })
  highlight(0, '@lsp.type.parameter.lua',           { link = '@lsp.type.parameter' })
  highlight(0, '@lsp.type.property.lua',            { link = '@lsp.type.property' })
  highlight(0, '@lsp.type.function.lua',            { link = '@lsp.type.function' })
  highlight(0, '@lsp.type.method.lua',              { link = '@lsp.type.method' })
  highlight(0, '@lsp.type.keyword.lua',             { link = "Keyword" })
  highlight(0, '@lsp.type.comment.lua',             { link = "Comment" })
  highlight(0, '@lsp.type.string.lua',              { link = "String" })
  highlight(0, '@lsp.type.number.lua',              { link = "Number" })
  highlight(0, '@lsp.type.type.lua',                { link = '@lsp.type.type' })
  highlight(0, '@lsp.type.class.lua',               { link = '@lsp.type.class' })
  highlight(0, '@lsp.type.namespace.lua',           { link = '@lsp.type.namespace' })

end

return lua
