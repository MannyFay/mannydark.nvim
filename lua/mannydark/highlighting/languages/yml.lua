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
  highlight(0, 'yamlBlockMappingKey',       { fg = colors.blue,       bg = 'NONE' })  -- Unquoted mapping keys
  highlight(0, 'yamlBlockMappingKeyString', { fg = colors.blue,       bg = 'NONE' })  -- Quoted mapping keys
  highlight(0, 'yamlFlowMappingKey',        { fg = colors.blue,       bg = 'NONE' })  -- Flow context mapping keys
  highlight(0, 'yamlMappingKeyStart',       { fg = colors.white,      bg = 'NONE' })  -- Explicit key indicator (?)

  -- Values / Scalars
  highlight(0, 'yamlPlainScalar',           { fg = colors.white,      bg = 'NONE' })  -- Unquoted scalar values
  highlight(0, 'yamlBlockScalar',           { fg = colors.redLight,   bg = 'NONE' })  -- Block scalar content
  highlight(0, 'yamlBlockString',           { fg = colors.redLight,   bg = 'NONE' })  -- Multi-line block strings
  highlight(0, 'yamlBlockScalarHeader',     { fg = colors.white,      bg = 'NONE' })  -- Block scalar indicators (|, >)

  -- Strings
  highlight(0, 'yamlString',                { fg = colors.redLight,   bg = 'NONE' })  -- General strings
  highlight(0, 'yamlFlowString',            { fg = colors.redLight,   bg = 'NONE' })  -- Double/single quoted strings
  highlight(0, 'yamlFlowStringDelimiter',   { fg = colors.redLight,   bg = 'NONE' })  -- Quote boundaries

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
  highlight(0, 'yamlConstant',              { fg = colors.blue,       bg = 'NONE' })  -- Built-in constants

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
  highlight(0, 'yamlFlowMappingDelimiter',  { fg = colors.white,      bg = 'NONE' })  -- Flow mapping colons

  -- Block Collections
  highlight(0, 'yamlBlockCollectionItemStart', { fg = colors.white,   bg = 'NONE' })  -- List indicator (-)

  -- Delimiters
  highlight(0, 'yamlKeyValueDelimiter',     { fg = colors.white,      bg = 'NONE' })  -- Colon between key and value
  highlight(0, 'yamlBlockMappingDelimiter', { fg = colors.white,      bg = 'NONE' })  -- Block mapping colons

  -- Comments
  highlight(0, 'yamlComment',               { fg = colors.red,        bg = 'NONE' })  -- # comments
  highlight(0, 'yamlTodo',                  { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX, NOTE


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.yaml)

  -- Properties (Keys)
  highlight(0, '@property.yaml',            { fg = colors.blue,       bg = 'NONE' })  -- Mapping keys

  -- Strings
  highlight(0, '@string.yaml',              { fg = colors.redLight,   bg = 'NONE' })  -- Quoted and block strings
  highlight(0, '@string.escape.yaml',       { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.yaml',              { fg = colors.greenLight, bg = 'NONE' })  -- Integer and float scalars
  highlight(0, '@number.float.yaml',        { fg = colors.greenLight, bg = 'NONE' })  -- Float numbers

  -- Booleans
  highlight(0, '@boolean.yaml',             { fg = colors.blue,       bg = 'NONE' })  -- true, false, yes, no

  -- Constants
  highlight(0, '@constant.yaml',            { fg = colors.blue,       bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.yaml',    { fg = colors.blue,       bg = 'NONE' })  -- null, ~

  -- Labels (Anchors/Aliases)
  highlight(0, '@label.yaml',               { fg = colors.pink,       bg = 'NONE' })  -- Anchor and alias names

  -- Types (Tags)
  highlight(0, '@type.yaml',                { fg = colors.turquoise,  bg = 'NONE' })  -- Tags (!tag)

  -- Keywords/Directives
  highlight(0, '@keyword.yaml',             { fg = colors.pink,       bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.directive.yaml',   { fg = colors.pink,       bg = 'NONE' })  -- %YAML, %TAG directives

  -- Comments
  highlight(0, '@comment.yaml',             { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Punctuation
  highlight(0, '@punctuation.delimiter.yaml', { fg = colors.white,    bg = 'NONE' })  -- , - : etc.
  highlight(0, '@punctuation.bracket.yaml', { fg = colors.white,      bg = 'NONE' })  -- [], {}
  highlight(0, '@punctuation.special.yaml', { fg = colors.pink,       bg = 'NONE' })  -- *, &, ---, ...

  -- Variables (for interpolation in some YAML variants)
  highlight(0, '@variable.yaml',            { fg = colors.white,      bg = 'NONE' })  -- Variables


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.yaml)

  highlight(0, '@lsp.type.property.yaml',   { fg = colors.blue,       bg = 'NONE' })  -- Keys
  highlight(0, '@lsp.type.string.yaml',     { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.yaml',     { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.keyword.yaml',    { fg = colors.pink,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.comment.yaml',    { fg = colors.red,        bg = 'NONE' })  -- Comments


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
  highlight(0, 'yamlEnvVar',                { fg = colors.purple,     bg = 'NONE' })  -- ${VAR}, $VAR

  -- Template Variables (Helm, Ansible, etc.)
  highlight(0, 'yamlTemplateVar',           { fg = colors.purple,     bg = 'NONE' })  -- {{ variable }}
  highlight(0, 'yamlTemplateDelim',         { fg = colors.pink,       bg = 'NONE' })  -- {{ and }}

  -- Jinja2 (Ansible)
  highlight(0, 'yamlJinja',                 { fg = colors.purple,     bg = 'NONE' })  -- Jinja expressions
  highlight(0, 'yamlJinjaDelim',            { fg = colors.pink,       bg = 'NONE' })  -- {% %}, {{ }}
  highlight(0, 'yamlJinjaFilter',           { fg = colors.orange,     bg = 'NONE' })  -- | filter

  -- Helm Templates
  highlight(0, 'yamlHelmTemplate',          { fg = colors.purple,     bg = 'NONE' })  -- .Values, .Release
  highlight(0, 'yamlHelmFunction',          { fg = colors.orange,     bg = 'NONE' })  -- include, toYaml, etc.


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
  highlight(0, 'yamlCIVariable',            { fg = colors.purple,     bg = 'NONE' })  -- CI/CD variables
end

return yml
