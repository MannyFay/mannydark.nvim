-------------------------------------------------------------------------------
-- JSON Files
-- Highlighting for .json, .jsonc, .json5 files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local json      = {}


-------------------------------------------------------------------------------
-- Settings

json.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keys / Properties
  highlight(0, 'jsonKeyword',           { link = "Keyword" })  -- Property names (keys)
  highlight(0, 'jsonKeywordMatch',      { link = "Keyword" })  -- Colon after key

  -- Strings
  highlight(0, 'jsonString',            { link = "String" })  -- String values
  highlight(0, 'jsonQuote',             { fg = colors.redLight,   bg = 'NONE' })  -- Quote characters

  -- Escape Sequences
  highlight(0, 'jsonEscape',            { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, \\, \", etc.

  -- Numbers
  highlight(0, 'jsonNumber',            { link = "Number" })  -- Numeric values

  -- Booleans
  highlight(0, 'jsonBoolean',           { link = "Boolean" })  -- true, false

  -- Null
  highlight(0, 'jsonNull',              { fg = colors.blue,       bg = 'NONE' })  -- null

  -- Punctuation
  highlight(0, 'jsonNoise',             { fg = colors.white,      bg = 'NONE' })  -- Colons and commas
  highlight(0, 'jsonBraces',            { fg = colors.white,      bg = 'NONE' })  -- { } [ ]
  highlight(0, 'jsonComma',             { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'jsonColon',             { fg = colors.white,      bg = 'NONE' })  -- :
  highlight(0, 'jsonBracket',           { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'jsonCurly',             { fg = colors.white,      bg = 'NONE' })  -- { }

  -- JSONP Padding
  highlight(0, 'jsonPadding',           { fg = colors.orange,     bg = 'NONE' })  -- JSONP function wrapper

  -- Folding
  highlight(0, 'jsonFold',              { fg = colors.gray,       bg = 'NONE' })  -- Folded regions


  -----------------------------------------------------------------------------
  -- JSON Error Highlighting (when warnings enabled)

  highlight(0, 'jsonNoQuotesError',         { fg = colors.red, bg = 'NONE', undercurl = true })  -- Unquoted strings
  highlight(0, 'jsonTripleQuotesError',     { fg = colors.red, bg = 'NONE', undercurl = true })  -- Triple quotes
  highlight(0, 'jsonNumError',              { fg = colors.red, bg = 'NONE', undercurl = true })  -- Invalid numbers
  highlight(0, 'jsonCommentError',          { link = "Comment" })  -- Comments in strict JSON
  highlight(0, 'jsonSemicolonError',        { fg = colors.red, bg = 'NONE', undercurl = true })  -- Semicolons
  highlight(0, 'jsonTrailingCommaError',    { fg = colors.red, bg = 'NONE', undercurl = true })  -- Trailing commas
  highlight(0, 'jsonMissingCommaError',     { fg = colors.red, bg = 'NONE', undercurl = true })  -- Missing commas
  highlight(0, 'jsonStringSQError',         { link = "String" })  -- Single-quoted strings


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.json)

  -- Properties (Keys)
  highlight(0, '@property.json',            { fg = colors.blue,       bg = 'NONE' })  -- Object keys

  -- Strings
  highlight(0, '@string.json',              { link = "String" })  -- String values
  highlight(0, '@string.escape.json',       { link = "String" })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.json',              { link = "Number" })  -- Numbers
  highlight(0, '@number.float.json',        { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.json',             { link = "Boolean" })  -- true, false

  -- Constants
  highlight(0, '@constant.json',            { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.json',    { link = "Constant" })  -- null

  -- Punctuation
  highlight(0, '@punctuation.delimiter.json', { link = "Delimiter" })  -- , :
  highlight(0, '@punctuation.bracket.json', { fg = colors.white,      bg = 'NONE' })  -- [ ] { }

  -- Conceal (for hiding quotes)
  highlight(0, '@conceal.json',             { fg = colors.gray,       bg = 'NONE' })  -- Concealed characters


  -----------------------------------------------------------------------------
  -- JSONC (JSON with Comments) - Treesitter Groups

  -- Comments
  highlight(0, '@comment.jsonc',            { link = "Comment" })  -- // and /* */ comments

  -- Properties (Keys)
  highlight(0, '@property.jsonc',           { fg = colors.blue,       bg = 'NONE' })  -- Object keys

  -- Strings
  highlight(0, '@string.jsonc',             { link = "String" })  -- String values
  highlight(0, '@string.escape.jsonc',      { link = "String" })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.jsonc',             { link = "Number" })  -- Numbers

  -- Booleans
  highlight(0, '@boolean.jsonc',            { link = "Boolean" })  -- true, false

  -- Constants
  highlight(0, '@constant.builtin.jsonc',   { link = "Constant" })  -- null

  -- Punctuation
  highlight(0, '@punctuation.delimiter.jsonc', { link = "Delimiter" })  -- , :
  highlight(0, '@punctuation.bracket.jsonc', { fg = colors.white,     bg = 'NONE' })  -- [ ] { }


  -----------------------------------------------------------------------------
  -- JSON5 - Treesitter Groups

  -- Comments
  highlight(0, '@comment.json5',            { link = "Comment" })  -- // and /* */ comments

  -- Properties (Keys) - JSON5 allows unquoted keys
  highlight(0, '@property.json5',           { fg = colors.blue,       bg = 'NONE' })  -- Object keys

  -- Strings - JSON5 allows single quotes
  highlight(0, '@string.json5',             { link = "String" })  -- String values
  highlight(0, '@string.escape.json5',      { link = "String" })  -- Escape sequences

  -- Numbers - JSON5 allows Infinity, NaN, hex
  highlight(0, '@number.json5',             { link = "Number" })  -- Numbers

  -- Booleans
  highlight(0, '@boolean.json5',            { link = "Boolean" })  -- true, false

  -- Constants
  highlight(0, '@constant.builtin.json5',   { link = "Constant" })  -- null, Infinity, NaN

  -- Punctuation
  highlight(0, '@punctuation.delimiter.json5', { link = "Delimiter" })  -- , :
  highlight(0, '@punctuation.bracket.json5', { fg = colors.white,     bg = 'NONE' })  -- [ ] { }


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.json)

  highlight(0, '@lsp.type.property.json',   { fg = colors.blue,       bg = 'NONE' })  -- Keys
  highlight(0, '@lsp.type.string.json',     { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.json',     { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.json',    { link = "Keyword" })  -- true, false, null
  highlight(0, '@lsp.type.comment.json',    { link = "Comment" })  -- Comments (JSONC)

  -- JSONC LSP
  highlight(0, '@lsp.type.property.jsonc',  { fg = colors.blue,       bg = 'NONE' })  -- Keys
  highlight(0, '@lsp.type.string.jsonc',    { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.jsonc',    { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.jsonc',   { link = "Keyword" })  -- true, false, null
  highlight(0, '@lsp.type.comment.jsonc',   { link = "Comment" })  -- Comments


  -----------------------------------------------------------------------------
  -- Vim Legacy JSONC Groups

  -- Comments (for JSONC - JSON with Comments)
  highlight(0, 'jsoncComment',              { link = "Comment" })  -- Comments
  highlight(0, 'jsoncLineComment',          { link = "Comment" })  -- // line comments
  highlight(0, 'jsoncBlockComment',         { link = "Comment" })  -- /* block comments */
  highlight(0, 'jsoncTodo',                 { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO in comments

  -- JSONC Keys and Values
  highlight(0, 'jsoncKeyword',              { link = "Keyword" })  -- Property names
  highlight(0, 'jsoncString',               { link = "String" })  -- String values
  highlight(0, 'jsoncNumber',               { link = "Number" })  -- Numbers
  highlight(0, 'jsoncBoolean',              { link = "Boolean" })  -- true, false
  highlight(0, 'jsoncNull',                 { fg = colors.blue,       bg = 'NONE' })  -- null
  highlight(0, 'jsoncBraces',               { fg = colors.white,      bg = 'NONE' })  -- { } [ ]


  -----------------------------------------------------------------------------
  -- Package.json Specific (npm/node)

  highlight(0, 'jsonPackageName',           { fg = colors.turquoise,  bg = 'NONE' })  -- "name" value
  highlight(0, 'jsonPackageVersion',        { fg = colors.greenLight, bg = 'NONE' })  -- Version strings
  highlight(0, 'jsonPackageDep',            { fg = colors.blue,       bg = 'NONE' })  -- dependencies key
  highlight(0, 'jsonPackageDevDep',         { fg = colors.blue,       bg = 'NONE' })  -- devDependencies key
  highlight(0, 'jsonPackageScript',         { fg = colors.orange,     bg = 'NONE' })  -- scripts key
  highlight(0, 'jsonPackageScriptName',     { fg = colors.turquoise,  bg = 'NONE' })  -- Script names
  highlight(0, 'jsonPackageScriptCmd',      { fg = colors.redLight,   bg = 'NONE' })  -- Script commands


  -----------------------------------------------------------------------------
  -- TSConfig/JSConfig Specific

  highlight(0, 'jsonTsConfigOption',        { fg = colors.blue,       bg = 'NONE' })  -- compilerOptions keys
  highlight(0, 'jsonTsConfigPath',          { fg = colors.redLight,   bg = 'NONE' })  -- Path values


  -----------------------------------------------------------------------------
  -- VSCode Settings JSON

  highlight(0, 'jsonVSCodeSetting',         { fg = colors.blue,       bg = 'NONE' })  -- Setting keys
  highlight(0, 'jsonVSCodeValue',           { fg = colors.redLight,   bg = 'NONE' })  -- Setting values


  -----------------------------------------------------------------------------
  -- JSON Schema

  highlight(0, 'jsonSchema',                { fg = colors.turquoise,  bg = 'NONE' })  -- $schema
  highlight(0, 'jsonSchemaRef',             { fg = colors.turquoise,  bg = 'NONE' })  -- $ref
  highlight(0, 'jsonSchemaType',            { link = "Type" })  -- type definitions
  highlight(0, 'jsonSchemaProperties',      { fg = colors.blue,       bg = 'NONE' })  -- properties
  highlight(0, 'jsonSchemaRequired',        { fg = colors.pink,       bg = 'NONE' })  -- required
end

return json
