-------------------------------------------------------------------------------
-- TOML Files
-- Highlighting for .toml files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local toml      = {}


-------------------------------------------------------------------------------
-- Settings

toml.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keys
  highlight(0, 'tomlKey',               { fg = colors.blue,       bg = 'NONE' })  -- Key names
  highlight(0, 'tomlKeyDq',             { fg = colors.blue,       bg = 'NONE' })  -- Double-quoted keys
  highlight(0, 'tomlKeySq',             { fg = colors.blue,       bg = 'NONE' })  -- Single-quoted keys
  highlight(0, 'tomlKeyDot',            { fg = colors.white,      bg = 'NONE' })  -- Dot in dotted keys

  -- Tables
  highlight(0, 'tomlTable',             { fg = colors.turquoise,  bg = 'NONE' })  -- [table] headers
  highlight(0, 'tomlTableArray',        { fg = colors.turquoise,  bg = 'NONE' })  -- [[array_of_tables]] headers

  -- Strings
  highlight(0, 'tomlString',            { fg = colors.redLight,   bg = 'NONE' })  -- Basic strings "..."
  highlight(0, 'tomlStringBasic',       { fg = colors.redLight,   bg = 'NONE' })  -- Basic strings
  highlight(0, 'tomlStringBasicMulti',  { fg = colors.redLight,   bg = 'NONE' })  -- Multi-line basic """..."""
  highlight(0, 'tomlStringLiteral',     { fg = colors.redLight,   bg = 'NONE' })  -- Literal strings '...'
  highlight(0, 'tomlStringLiteralMulti',{ fg = colors.redLight,   bg = 'NONE' })  -- Multi-line literal '''...'''

  -- Escape Sequences
  highlight(0, 'tomlEscape',            { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, \\, \", etc.
  highlight(0, 'tomlEscapeUnicode',     { fg = colors.pink,       bg = 'NONE' })  -- \uXXXX, \UXXXXXXXX
  highlight(0, 'tomlLineEscape',        { fg = colors.pink,       bg = 'NONE' })  -- \ at end of line

  -- Numbers
  highlight(0, 'tomlInteger',           { fg = colors.greenLight, bg = 'NONE' })  -- Decimal integers
  highlight(0, 'tomlIntegerHex',        { fg = colors.greenLight, bg = 'NONE' })  -- 0x... hexadecimal
  highlight(0, 'tomlIntegerOct',        { fg = colors.greenLight, bg = 'NONE' })  -- 0o... octal
  highlight(0, 'tomlIntegerBin',        { fg = colors.greenLight, bg = 'NONE' })  -- 0b... binary
  highlight(0, 'tomlFloat',             { fg = colors.greenLight, bg = 'NONE' })  -- Floating point numbers
  highlight(0, 'tomlFloatExp',          { fg = colors.greenLight, bg = 'NONE' })  -- Scientific notation (e, E)

  -- Special Float Values
  highlight(0, 'tomlInf',               { fg = colors.greenLight, bg = 'NONE' })  -- inf, +inf, -inf
  highlight(0, 'tomlNan',               { fg = colors.greenLight, bg = 'NONE' })  -- nan, +nan, -nan

  -- Booleans
  highlight(0, 'tomlBoolean',           { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Date/Time
  highlight(0, 'tomlDate',              { fg = colors.greenLight, bg = 'NONE' })  -- Date (YYYY-MM-DD)
  highlight(0, 'tomlTime',              { fg = colors.greenLight, bg = 'NONE' })  -- Time (HH:MM:SS)
  highlight(0, 'tomlDateTime',          { fg = colors.greenLight, bg = 'NONE' })  -- DateTime (RFC 3339)
  highlight(0, 'tomlDateTimeOffset',    { fg = colors.greenLight, bg = 'NONE' })  -- Offset datetime
  highlight(0, 'tomlLocalDateTime',     { fg = colors.greenLight, bg = 'NONE' })  -- Local datetime
  highlight(0, 'tomlLocalDate',         { fg = colors.greenLight, bg = 'NONE' })  -- Local date
  highlight(0, 'tomlLocalTime',         { fg = colors.greenLight, bg = 'NONE' })  -- Local time

  -- Arrays
  highlight(0, 'tomlArray',             { fg = colors.white,      bg = 'NONE' })  -- Array brackets []
  highlight(0, 'tomlArrayDelim',        { fg = colors.white,      bg = 'NONE' })  -- Commas in arrays

  -- Inline Tables
  highlight(0, 'tomlInlineTable',       { fg = colors.white,      bg = 'NONE' })  -- Inline table { }
  highlight(0, 'tomlInlineTableDelim',  { fg = colors.white,      bg = 'NONE' })  -- Commas in inline tables

  -- Operators
  highlight(0, 'tomlAssignment',        { fg = colors.white,      bg = 'NONE' })  -- = operator

  -- Delimiters / Punctuation
  highlight(0, 'tomlDelimiter',         { fg = colors.white,      bg = 'NONE' })  -- General delimiters
  highlight(0, 'tomlBracket',           { fg = colors.white,      bg = 'NONE' })  -- [ ] brackets
  highlight(0, 'tomlBrace',             { fg = colors.white,      bg = 'NONE' })  -- { } braces
  highlight(0, 'tomlComma',             { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'tomlDot',               { fg = colors.white,      bg = 'NONE' })  -- .

  -- Comments
  highlight(0, 'tomlComment',           { fg = colors.red,        bg = 'NONE' })  -- # comments
  highlight(0, 'tomlTodo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX, BUG

  -- Errors
  highlight(0, 'tomlError',             { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.toml)

  -- Properties (Keys)
  highlight(0, '@property.toml',            { fg = colors.blue,       bg = 'NONE' })  -- Key names

  -- Strings
  highlight(0, '@string.toml',              { fg = colors.redLight,   bg = 'NONE' })  -- String values
  highlight(0, '@string.escape.toml',       { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special.toml',      { fg = colors.turquoise,  bg = 'NONE' })  -- Table/array headers

  -- Numbers
  highlight(0, '@number.toml',              { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.toml',        { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.toml',             { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Constants
  highlight(0, '@constant.toml',            { fg = colors.blue,       bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.toml',    { fg = colors.greenLight, bg = 'NONE' })  -- inf, nan

  -- Comments
  highlight(0, '@comment.toml',             { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Operators
  highlight(0, '@operator.toml',            { fg = colors.white,      bg = 'NONE' })  -- = operator

  -- Punctuation
  highlight(0, '@punctuation.delimiter.toml', { fg = colors.white,    bg = 'NONE' })  -- , .
  highlight(0, '@punctuation.bracket.toml', { fg = colors.white,      bg = 'NONE' })  -- [ ] { }

  -- Types (for table headers)
  highlight(0, '@type.toml',                { fg = colors.turquoise,  bg = 'NONE' })  -- Table names

  -- Variables
  highlight(0, '@variable.toml',            { fg = colors.white,      bg = 'NONE' })  -- Values


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.toml)

  highlight(0, '@lsp.type.property.toml',   { fg = colors.blue,       bg = 'NONE' })  -- Keys
  highlight(0, '@lsp.type.string.toml',     { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.toml',     { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.keyword.toml',    { fg = colors.blue,       bg = 'NONE' })  -- true, false
  highlight(0, '@lsp.type.comment.toml',    { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.namespace.toml',  { fg = colors.turquoise,  bg = 'NONE' })  -- Table names


  -----------------------------------------------------------------------------
  -- Cargo.toml Specific (Rust)

  -- Common sections
  highlight(0, 'tomlCargoPackage',      { fg = colors.turquoise,  bg = 'NONE' })  -- [package]
  highlight(0, 'tomlCargoDependencies', { fg = colors.turquoise,  bg = 'NONE' })  -- [dependencies]
  highlight(0, 'tomlCargoDevDeps',      { fg = colors.turquoise,  bg = 'NONE' })  -- [dev-dependencies]
  highlight(0, 'tomlCargoBuildDeps',    { fg = colors.turquoise,  bg = 'NONE' })  -- [build-dependencies]
  highlight(0, 'tomlCargoFeatures',     { fg = colors.turquoise,  bg = 'NONE' })  -- [features]
  highlight(0, 'tomlCargoWorkspace',    { fg = colors.turquoise,  bg = 'NONE' })  -- [workspace]
  highlight(0, 'tomlCargoBin',          { fg = colors.turquoise,  bg = 'NONE' })  -- [[bin]]
  highlight(0, 'tomlCargoLib',          { fg = colors.turquoise,  bg = 'NONE' })  -- [lib]
  highlight(0, 'tomlCargoProfile',      { fg = colors.turquoise,  bg = 'NONE' })  -- [profile.release]

  -- Common keys
  highlight(0, 'tomlCargoName',         { fg = colors.blue,       bg = 'NONE' })  -- name
  highlight(0, 'tomlCargoVersion',      { fg = colors.blue,       bg = 'NONE' })  -- version
  highlight(0, 'tomlCargoEdition',      { fg = colors.blue,       bg = 'NONE' })  -- edition
  highlight(0, 'tomlCargoAuthors',      { fg = colors.blue,       bg = 'NONE' })  -- authors
  highlight(0, 'tomlCargoPath',         { fg = colors.blue,       bg = 'NONE' })  -- path
  highlight(0, 'tomlCargoGit',          { fg = colors.blue,       bg = 'NONE' })  -- git
  highlight(0, 'tomlCargoBranch',       { fg = colors.blue,       bg = 'NONE' })  -- branch
  highlight(0, 'tomlCargoOptional',     { fg = colors.blue,       bg = 'NONE' })  -- optional


  -----------------------------------------------------------------------------
  -- pyproject.toml Specific (Python)

  -- Common sections
  highlight(0, 'tomlPyProject',         { fg = colors.turquoise,  bg = 'NONE' })  -- [project]
  highlight(0, 'tomlPyBuildSystem',     { fg = colors.turquoise,  bg = 'NONE' })  -- [build-system]
  highlight(0, 'tomlPyTool',            { fg = colors.turquoise,  bg = 'NONE' })  -- [tool.*]
  highlight(0, 'tomlPyPoetry',          { fg = colors.turquoise,  bg = 'NONE' })  -- [tool.poetry]
  highlight(0, 'tomlPyBlack',           { fg = colors.turquoise,  bg = 'NONE' })  -- [tool.black]
  highlight(0, 'tomlPyRuff',            { fg = colors.turquoise,  bg = 'NONE' })  -- [tool.ruff]
  highlight(0, 'tomlPyPytest',          { fg = colors.turquoise,  bg = 'NONE' })  -- [tool.pytest]
  highlight(0, 'tomlPyMypy',            { fg = colors.turquoise,  bg = 'NONE' })  -- [tool.mypy]


  -----------------------------------------------------------------------------
  -- Hugo/Jekyll Config (config.toml)

  highlight(0, 'tomlHugoBaseURL',       { fg = colors.blue,       bg = 'NONE' })  -- baseURL
  highlight(0, 'tomlHugoTitle',         { fg = colors.blue,       bg = 'NONE' })  -- title
  highlight(0, 'tomlHugoTheme',         { fg = colors.blue,       bg = 'NONE' })  -- theme
  highlight(0, 'tomlHugoParams',        { fg = colors.turquoise,  bg = 'NONE' })  -- [params]
  highlight(0, 'tomlHugoMenu',          { fg = colors.turquoise,  bg = 'NONE' })  -- [[menu.*]]


  -----------------------------------------------------------------------------
  -- .taplo.toml / Formatter Config

  highlight(0, 'tomlTaploConfig',       { fg = colors.turquoise,  bg = 'NONE' })  -- [formatting]
  highlight(0, 'tomlTaploSchema',       { fg = colors.turquoise,  bg = 'NONE' })  -- [schema]


  -----------------------------------------------------------------------------
  -- Netlify/Vercel Config (netlify.toml)

  highlight(0, 'tomlNetlifyBuild',      { fg = colors.turquoise,  bg = 'NONE' })  -- [build]
  highlight(0, 'tomlNetlifyContext',    { fg = colors.turquoise,  bg = 'NONE' })  -- [context.*]
  highlight(0, 'tomlNetlifyRedirects',  { fg = colors.turquoise,  bg = 'NONE' })  -- [[redirects]]
  highlight(0, 'tomlNetlifyHeaders',    { fg = colors.turquoise,  bg = 'NONE' })  -- [[headers]]


  -----------------------------------------------------------------------------
  -- deno.toml

  highlight(0, 'tomlDenoTasks',         { fg = colors.turquoise,  bg = 'NONE' })  -- [tasks]
  highlight(0, 'tomlDenoImports',       { fg = colors.turquoise,  bg = 'NONE' })  -- [imports]
  highlight(0, 'tomlDenoFmt',           { fg = colors.turquoise,  bg = 'NONE' })  -- [fmt]
  highlight(0, 'tomlDenoLint',          { fg = colors.turquoise,  bg = 'NONE' })  -- [lint]


  -----------------------------------------------------------------------------
  -- GitHub Actions (action.toml)

  highlight(0, 'tomlGHAction',          { fg = colors.turquoise,  bg = 'NONE' })  -- [action]
  highlight(0, 'tomlGHInputs',          { fg = colors.turquoise,  bg = 'NONE' })  -- [inputs]
  highlight(0, 'tomlGHOutputs',         { fg = colors.turquoise,  bg = 'NONE' })  -- [outputs]
end

return toml
