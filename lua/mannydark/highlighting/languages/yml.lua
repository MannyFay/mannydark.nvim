-------------------------------------------------------------------------------
-- YAML Files
-- Highlighting for .yml and .yaml files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local yml       = {}


-------------------------------------------------------------------------------
-- Settings

yml.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keys
  highlight(0, 'yamlBlockMappingKey',       { link = "Special" })  -- Unquoted mapping keys
  highlight(0, 'yamlBlockMappingKeyString', { link = "String" })  -- Quoted mapping keys
  highlight(0, 'yamlFlowMappingKey',        { fg = colors.blue,       bg = 'NONE' })  -- Flow context mapping keys
  highlight(0, 'yamlMappingKeyStart',       { fg = colors.white,      bg = 'NONE' })  -- Explicit key indicator (?)

  -- Values / Scalars
  highlight(0, 'yamlPlainScalar',           { fg = colors.white,      bg = 'NONE' })  -- Unquoted scalar values
  highlight(0, 'yamlBlockScalar',           { fg = colors.redLight,   bg = 'NONE' })  -- Block scalar content
  highlight(0, 'yamlBlockString',           { link = "String" })  -- Multi-line block strings
  highlight(0, 'yamlBlockScalarHeader',     { fg = colors.white,      bg = 'NONE' })  -- Block scalar indicators (|, >)

  -- Strings
  highlight(0, 'yamlString',                { link = "String" })  -- General strings
  highlight(0, 'yamlFlowString',            { link = "String" })  -- Double/single quoted strings
  highlight(0, 'yamlFlowStringDelimiter',   { link = "Delimiter" })  -- Quote boundaries

  -- Escape Sequences
  highlight(0, 'yamlEscape',                { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences \n, \t, etc.
  highlight(0, 'yamlSingleEscape',          { fg = colors.pink,       bg = 'NONE' })  -- Single quote escaping ''

  -- Numbers
  highlight(0, 'yamlInteger',               { fg = colors.greenLight, bg = 'NONE' })  -- Integer numbers
  highlight(0, 'yamlFloat',                 { fg = colors.greenLight, bg = 'NONE' })  -- Float numbers

  -- Booleans
  highlight(0, 'yamlBool',                  { fg = colors.blue,       bg = 'NONE' })  -- true, false, yes, no

  -- Null
  highlight(0, 'yamlNull',                  { fg = colors.blue,       bg = 'NONE' })  -- null, Null, NULL, ~

  -- Constants
  highlight(0, 'yamlConstant',              { link = "Constant" })  -- Built-in constants

  -- Timestamp
  highlight(0, 'yamlTimestamp',             { fg = colors.greenLight, bg = 'NONE' })  -- ISO 8601 timestamps

  -- Anchors and Aliases
  highlight(0, 'yamlAnchor',                { fg = colors.pink,       bg = 'NONE' })  -- Anchor definitions (&name)
  highlight(0, 'yamlAlias',                 { fg = colors.pink,       bg = 'NONE' })  -- Alias references (*name)

  -- Merge Key
  highlight(0, 'yamlBlockMappingMerge',     { fg = colors.pink,       bg = 'NONE' })  -- Merge key (<<)
  highlight(0, 'yamlFlowMappingMerge',      { fg = colors.pink,       bg = 'NONE' })  -- Flow merge key

  -- Tags
  highlight(0, 'yamlNodeTag',               { fg = colors.turquoise,  bg = 'NONE' })  -- Tag properties (!tag)
  highlight(0, 'yamlTagHandle',             { fg = colors.turquoise,  bg = 'NONE' })  -- Tag handles (!name!, !!, !)
  highlight(0, 'yamlTagPrefix',             { fg = colors.turquoise,  bg = 'NONE' })  -- Tag prefixes

  -- Directives
  highlight(0, 'yamlDirective',             { fg = colors.pink,       bg = 'NONE' })  -- Document directives (%)
  highlight(0, 'yamlDirectiveName',         { fg = colors.pink,       bg = 'NONE' })  -- Directive name
  highlight(0, 'yamlTAGDirective',          { fg = colors.pink,       bg = 'NONE' })  -- %TAG directive
  highlight(0, 'yamlYAMLDirective',         { fg = colors.pink,       bg = 'NONE' })  -- %YAML directive
  highlight(0, 'yamlReservedDirective',     { fg = colors.pink,       bg = 'NONE' })  -- Non-standard directives
  highlight(0, 'yamlYAMLVersion',           { fg = colors.greenLight, bg = 'NONE' })  -- Version numbers (1.2)

  -- Document Markers
  highlight(0, 'yamlDocumentStart',         { fg = colors.gray,       bg = 'NONE' })  -- Document start (---)
  highlight(0, 'yamlDocumentEnd',           { fg = colors.gray,       bg = 'NONE' })  -- Document end (...)

  -- Collections / Flow
  highlight(0, 'yamlFlowCollection',        { fg = colors.white,      bg = 'NONE' })  -- Commas in flow collections
  highlight(0, 'yamlFlowMapping',           { fg = colors.white,      bg = 'NONE' })  -- Brace-enclosed mappings {}
  highlight(0, 'yamlFlowIndicator',         { fg = colors.white,      bg = 'NONE' })  -- Flow syntax characters [], {}
  highlight(0, 'yamlFlowMappingDelimiter',  { link = "Delimiter" })  -- Flow mapping colons

  -- Block Collections
  highlight(0, 'yamlBlockCollectionItemStart', { fg = colors.white,   bg = 'NONE' })  -- List indicator (-)

  -- Delimiters
  highlight(0, 'yamlKeyValueDelimiter',     { link = "Delimiter" })  -- Colon between key and value
  highlight(0, 'yamlBlockMappingDelimiter', { link = "Delimiter" })  -- Block mapping colons

  -- Comments
  highlight(0, 'yamlComment',               { link = "Comment" })  -- # comments
  highlight(0, 'yamlTodo',                  { link = "Todo"})  -- TODO, FIXME, XXX, NOTE


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.yaml)

  -- Properties (Keys)
  highlight(0, '@property.yaml',            { link = "Variable"})  -- Mapping keys

  -- Strings
  highlight(0, '@string.yaml',              { link = "String" })  -- Quoted and block strings
  highlight(0, '@string.escape.yaml',       { link = "String" })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.yaml',              { link = "Number" })  -- Integer and float scalars
  highlight(0, '@number.float.yaml',        { link = "Number" })  -- Float numbers

  -- Booleans
  highlight(0, '@boolean.yaml',             { link = "Boolean" })  -- true, false, yes, no

  -- Constants
  highlight(0, '@constant.yaml',            { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.yaml',    { link = "Constant" })  -- null, ~

  -- Labels (Anchors/Aliases)
  highlight(0, '@label.yaml',               { link = "Type"})  -- Anchor and alias names

  -- Types (Tags)
  highlight(0, '@type.yaml',                { link = "Type" })  -- Tags (!tag)

  -- Keywords/Directives
  highlight(0, '@keyword.yaml',             { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.directive.yaml',   { link = "Keyword" })  -- %YAML, %TAG directives

  -- Comments
  highlight(0, '@comment.yaml',             { link = "Comment" })  -- Comments

  -- Punctuation
  highlight(0, '@punctuation.delimiter.yaml', { link = "Delimiter" })  -- , - : etc.
  highlight(0, '@punctuation.bracket.yaml', { fg = colors.white,      bg = 'NONE' })  -- [], {}
  highlight(0, '@punctuation.special.yaml', { link = "Keyword"})  -- *, &, ---, ...

  -- Variables (for interpolation in some YAML variants)
  highlight(0, '@variable.yaml',            { link = "Variable" })  -- Variables


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.yaml)

  highlight(0, '@lsp.type.property.yaml',   { fg = colors.blue,       bg = 'NONE' })  -- Keys
  highlight(0, '@lsp.type.string.yaml',     { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.yaml',     { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.yaml',    { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.comment.yaml',    { link = "Comment" })  -- Comments


  -----------------------------------------------------------------------------
  -- Special YAML Constructs

  -- Multi-document Files
  highlight(0, 'yamlDocumentMarker',        { fg = colors.gray,       bg = 'NONE' })  -- --- and ...

  -- Folded/Literal Block Scalars
  highlight(0, 'yamlLiteralBlock',          { fg = colors.redLight,   bg = 'NONE' })  -- | literal block
  highlight(0, 'yamlFoldedBlock',           { fg = colors.redLight,   bg = 'NONE' })  -- > folded block
  highlight(0, 'yamlBlockIndicator',        { fg = colors.white,      bg = 'NONE' })  -- | or >
  highlight(0, 'yamlChompingIndicator',     { fg = colors.white,      bg = 'NONE' })  -- +, - chomping modifiers

  -- Indentation Indicator
  highlight(0, 'yamlIndentIndicator',       { fg = colors.white,      bg = 'NONE' })  -- Explicit indentation (|2)

  -- Complex Keys
  highlight(0, 'yamlComplexKey',            { fg = colors.blue,       bg = 'NONE' })  -- ? explicit key
  highlight(0, 'yamlComplexValue',          { fg = colors.white,      bg = 'NONE' })  -- : after complex key

  -- Special Values
  highlight(0, 'yamlInfinity',              { fg = colors.greenLight, bg = 'NONE' })  -- .inf, -.inf
  highlight(0, 'yamlNaN',                   { fg = colors.greenLight, bg = 'NONE' })  -- .nan

  -- Binary Data
  highlight(0, 'yamlBinary',                { fg = colors.redLight,   bg = 'NONE' })  -- !!binary base64 data


  -----------------------------------------------------------------------------
  -- Common YAML Usage Patterns

  -- Environment Variables (in Docker/K8s YAML)
  highlight(0, 'yamlEnvVar',                { link = "Variable" })  -- ${VAR}, $VAR

  -- Template Variables (Helm, Ansible, etc.)
  highlight(0, 'yamlTemplateVar',           { link = "Variable" })  -- {{ variable }}
  highlight(0, 'yamlTemplateDelim',         { link = "Delimiter" })  -- {{ and }}

  -- Jinja2 (Ansible)
  highlight(0, 'yamlJinja',                 { fg = colors.purple,     bg = 'NONE' })  -- Jinja expressions
  highlight(0, 'yamlJinjaDelim',            { link = "Delimiter" })  -- {% %}, {{ }}
  highlight(0, 'yamlJinjaFilter',           { fg = colors.orange,     bg = 'NONE' })  -- | filter

  -- Helm Templates
  highlight(0, 'yamlHelmTemplate',          { fg = colors.purple,     bg = 'NONE' })  -- .Values, .Release
  highlight(0, 'yamlHelmFunction',          { link = "Function" })  -- include, toYaml, etc.


  -----------------------------------------------------------------------------
  -- Kubernetes-specific

  highlight(0, 'yamlK8sKind',               { fg = colors.turquoise,  bg = 'NONE' })  -- Deployment, Service, etc.
  highlight(0, 'yamlK8sApiVersion',         { fg = colors.turquoise,  bg = 'NONE' })  -- apiVersion value
  highlight(0, 'yamlK8sMetadata',           { fg = colors.blue,       bg = 'NONE' })  -- metadata key
  highlight(0, 'yamlK8sSpec',               { fg = colors.blue,       bg = 'NONE' })  -- spec key


  -----------------------------------------------------------------------------
  -- Docker Compose-specific

  highlight(0, 'yamlDockerService',         { fg = colors.turquoise,  bg = 'NONE' })  -- Service names
  highlight(0, 'yamlDockerImage',           { fg = colors.redLight,   bg = 'NONE' })  -- image values
  highlight(0, 'yamlDockerPort',            { fg = colors.greenLight, bg = 'NONE' })  -- Port mappings


  -----------------------------------------------------------------------------
  -- GitHub Actions-specific

  highlight(0, 'yamlGHAJob',                { fg = colors.turquoise,  bg = 'NONE' })  -- Job names
  highlight(0, 'yamlGHAStep',               { fg = colors.blue,       bg = 'NONE' })  -- Step definitions
  highlight(0, 'yamlGHAUses',               { fg = colors.orange,     bg = 'NONE' })  -- uses: action
  highlight(0, 'yamlGHARun',                { fg = colors.redLight,   bg = 'NONE' })  -- run: commands
  highlight(0, 'yamlGHAExpression',         { fg = colors.purple,     bg = 'NONE' })  -- ${{ expression }}


  -----------------------------------------------------------------------------
  -- CI/CD Common

  highlight(0, 'yamlCIStage',               { fg = colors.turquoise,  bg = 'NONE' })  -- Stage names
  highlight(0, 'yamlCIScript',              { fg = colors.redLight,   bg = 'NONE' })  -- Script content
  highlight(0, 'yamlCIVariable',            { link = "Variable" })  -- CI/CD variables
end

return yml
