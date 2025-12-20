-------------------------------------------------------------------------------
-- Docker Compose (and YAML)
-------------------------------------------------------------------------------

local colors        = require('mannydark.palette')
local highlight     = vim.api.nvim_set_hl
local dockerCompose = {}


-------------------------------------------------------------------------------
-- Settings

dockerCompose.setupHighlighting = function()


  -------------------------------------------------------------------------
  -- Docker Compose Specific Syntax Groups (vim-docker-compose-syntax)
  -------------------------------------------------------------------------

  -- Source/Image Configuration
  highlight(0, 'DockerComposeSrc',            { fg = colors.blue,       bg = 'NONE'            })  -- build, image
  highlight(0, 'DockerComposeCtx',            { fg = colors.blue,       bg = 'NONE'            })  -- context, dockerfile, args

  -- Service References
  highlight(0, 'DockerComposeRef',            { fg = colors.turquoise,  bg = 'NONE'            })  -- cgroup_parent, container_name, depends_on, extends
  highlight(0, 'DockerComposeCap',            { fg = colors.orange,     bg = 'NONE'            })  -- cap_add, cap_drop

  -- External Resources
  highlight(0, 'DockerComposeExt',            { fg = colors.orange,     bg = 'NONE'            })  -- ports, devices, tmpfs, expose

  -- Networking
  highlight(0, 'DockerComposeNet',            { fg = colors.turquoise,  bg = 'NONE'            })  -- networks, dns, dns_search, extra_hosts, external_links

  -- Runtime
  highlight(0, 'DockerComposeRun',            { fg = colors.blue,       bg = 'NONE'            })  -- entrypoint, command, env_file

  -- Miscellaneous
  highlight(0, 'DockerComposeEtc',            { fg = colors.purple,     bg = 'NONE'            })  -- aliases

  -- Additional Docker Compose Keys (v2/v3 spec)
  highlight(0, 'DockerComposeVersion',        { fg = colors.blue,       bg = 'NONE'            })  -- version
  highlight(0, 'DockerComposeServices',       { fg = colors.blue,       bg = 'NONE'            })  -- services
  highlight(0, 'DockerComposeNetworks',       { fg = colors.blue,       bg = 'NONE'            })  -- networks (top-level)
  highlight(0, 'DockerComposeVolumes',        { fg = colors.blue,       bg = 'NONE'            })  -- volumes (top-level)
  highlight(0, 'DockerComposeConfigs',        { fg = colors.blue,       bg = 'NONE'            })  -- configs
  highlight(0, 'DockerComposeSecrets',        { fg = colors.pink,       bg = 'NONE'            })  -- secrets
  highlight(0, 'DockerComposeEnv',            { fg = colors.purple,     bg = 'NONE'            })  -- environment
  highlight(0, 'DockerComposeLabels',         { fg = colors.purple,     bg = 'NONE'            })  -- labels
  highlight(0, 'DockerComposeDeploy',         { fg = colors.blue,       bg = 'NONE'            })  -- deploy
  highlight(0, 'DockerComposeHealthcheck',    { fg = colors.turquoise,  bg = 'NONE'            })  -- healthcheck
  highlight(0, 'DockerComposeLogging',        { fg = colors.turquoise,  bg = 'NONE'            })  -- logging
  highlight(0, 'DockerComposeRestart',        { fg = colors.orange,     bg = 'NONE'            })  -- restart
  highlight(0, 'DockerComposeUser',           { fg = colors.purple,     bg = 'NONE'            })  -- user
  highlight(0, 'DockerComposeWorkingDir',     { fg = colors.turquoise,  bg = 'NONE'            })  -- working_dir
  highlight(0, 'DockerComposePrivileged',     { fg = colors.orange,     bg = 'NONE'            })  -- privileged
  highlight(0, 'DockerComposeReadOnly',       { fg = colors.orange,     bg = 'NONE'            })  -- read_only
  highlight(0, 'DockerComposeTty',            { fg = colors.turquoise,  bg = 'NONE'            })  -- tty, stdin_open


  -------------------------------------------------------------------------
  -- Legacy dockercompose Groups
  -------------------------------------------------------------------------

  highlight(0, 'dockercomposeComment',        { link = "Comment" })  -- Comments
  highlight(0, 'dockercomposeKeywords',       { link = "Keyword" })  -- Keywords
  highlight(0, 'dockercomposeString',         { link = "String" })  -- Double-quoted strings
  highlight(0, 'dockercomposeString1',        { link = "String" })  -- Single-quoted strings
  highlight(0, 'dockercomposeTodo',           { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO comments
  highlight(0, 'dockercomposeEmail',          { fg = colors.turquoise,  bg = 'NONE', underline = true })  -- Email addresses
  highlight(0, 'dockercomposeUrl',            { fg = colors.turquoise,  bg = 'NONE', underline = true })  -- URLs


  -------------------------------------------------------------------------
  -- YAML Syntax Groups (Docker Compose files are YAML)
  -------------------------------------------------------------------------

  -- Document Structure
  highlight(0, 'yamlDocumentStart',           { fg = colors.white,      bg = 'NONE'            })  -- ---
  highlight(0, 'yamlDocumentEnd',             { fg = colors.white,      bg = 'NONE'            })  -- ...

  -- Directives
  highlight(0, 'yamlDirective',               { fg = colors.blue,       bg = 'NONE'            })  -- %YAML, %TAG
  highlight(0, 'yamlDirectiveName',           { fg = colors.blue,       bg = 'NONE'            })  -- Directive name
  highlight(0, 'yamlTAGDirective',            { fg = colors.blue,       bg = 'NONE'            })  -- %TAG directive
  highlight(0, 'yamlTagHandle',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Tag handle (!!, !)
  highlight(0, 'yamlTagPrefix',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Tag prefix
  highlight(0, 'yamlYAMLDirective',           { fg = colors.blue,       bg = 'NONE'            })  -- %YAML directive
  highlight(0, 'yamlYAMLVersion',             { fg = colors.greenLight, bg = 'NONE'            })  -- YAML version (1.1, 1.2)
  highlight(0, 'yamlReservedDirective',       { fg = colors.orange,     bg = 'NONE'            })  -- Reserved directives

  -- Keys and Mappings
  highlight(0, 'yamlMappingKey',              { fg = colors.blue,       bg = 'NONE'            })  -- Mapping key
  highlight(0, 'yamlMappingKeyStart',         { fg = colors.white,      bg = 'NONE'            })  -- ? for explicit key
  highlight(0, 'yamlMappingMerge',            { fg = colors.pink,       bg = 'NONE'            })  -- << merge key
  highlight(0, 'yamlKeyValueDelimiter',       { link = "Delimiter" })  -- : delimiter

  -- Block Mappings
  highlight(0, 'yamlBlockMappingKey',         { fg = colors.blue,       bg = 'NONE'            })  -- Block mapping key
  highlight(0, 'yamlBlockMappingKeyString',   { link = "String" })  -- Block mapping key string
  highlight(0, 'yamlBlockMappingMerge',       { fg = colors.pink,       bg = 'NONE'            })  -- << in block mapping
  highlight(0, 'yamlBlockMappingDelimiter',   { link = "Delimiter" })  -- : in block mapping
  highlight(0, 'yamlBlockMappingKeyStart',    { fg = colors.white,      bg = 'NONE'            })  -- ? in block mapping
  highlight(0, 'yamlBlockCollectionItemStart', { fg = colors.white,     bg = 'NONE'            })  -- - for list items
  highlight(0, 'yamlBlockNodeProperties',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Node properties in block
  highlight(0, 'yamlBlockScalarHeader',       { fg = colors.orange,     bg = 'NONE'            })  -- | or > scalar header
  highlight(0, 'yamlBlockString',             { link = "String" })  -- Block scalar string
  highlight(0, 'yamlBlockNode',               { fg = colors.white,      bg = 'NONE'            })  -- Block node

  -- Flow Mappings
  highlight(0, 'yamlFlowMappingKey',          { fg = colors.blue,       bg = 'NONE'            })  -- Flow mapping key
  highlight(0, 'yamlFlowMappingKeyStart',     { fg = colors.white,      bg = 'NONE'            })  -- ? in flow mapping
  highlight(0, 'yamlFlowMappingMerge',        { fg = colors.pink,       bg = 'NONE'            })  -- << in flow mapping
  highlight(0, 'yamlFlowMappingDelimiter',    { link = "Delimiter" })  -- : in flow mapping
  highlight(0, 'yamlFlowNodeProperties',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Node properties in flow
  highlight(0, 'yamlFlowMapping',             { fg = colors.white,      bg = 'NONE'            })  -- Flow mapping { }
  highlight(0, 'yamlFlowCollection',          { fg = colors.white,      bg = 'NONE'            })  -- Flow collection [ ]
  highlight(0, 'yamlFlowNode',                { fg = colors.white,      bg = 'NONE'            })  -- Flow node
  highlight(0, 'yamlFlowIndicator',           { fg = colors.white,      bg = 'NONE'            })  -- { } [ ] ,

  -- Strings
  highlight(0, 'yamlString',                  { link = "String" })  -- Generic string
  highlight(0, 'yamlFlowString',              { link = "String" })  -- Flow string
  highlight(0, 'yamlFlowStringDelimiter',     { link = "Delimiter" })  -- String delimiters
  highlight(0, 'yamlPlainScalar',             { fg = colors.white,      bg = 'NONE'            })  -- Plain scalar (unquoted)
  highlight(0, 'yamlScalarWithSpecials',      { fg = colors.white,      bg = 'NONE'            })  -- Scalar with special chars

  -- Escapes
  highlight(0, 'yamlEscape',                  { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, 'yamlSingleEscape',            { fg = colors.pink,       bg = 'NONE'            })  -- '' escape in single-quoted

  -- Constants and Values
  highlight(0, 'yamlConstant',                { link = "Constant" })  -- Constants
  highlight(0, 'yamlBool',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false, yes, no, on, off
  highlight(0, 'yamlNull',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- null, ~

  -- Numbers
  highlight(0, 'yamlInteger',                 { fg = colors.greenLight, bg = 'NONE'            })  -- Integer values
  highlight(0, 'yamlFloat',                   { fg = colors.greenLight, bg = 'NONE'            })  -- Float values
  highlight(0, 'yamlTimestamp',               { fg = colors.greenLight, bg = 'NONE'            })  -- Timestamp values

  -- Tags and Anchors
  highlight(0, 'yamlNodeTag',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- !tag
  highlight(0, 'yamlAlias',                   { fg = colors.purple,     bg = 'NONE'            })  -- *alias
  highlight(0, 'yamlAnchor',                  { fg = colors.purple,     bg = 'NONE'            })  -- &anchor

  -- Comments
  highlight(0, 'yamlComment',                 { link = "Comment" })  -- # comment
  highlight(0, 'yamlTodo',                    { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, etc.


  -------------------------------------------------------------------------
  -- Treesitter YAML Captures
  -------------------------------------------------------------------------

  -- Boolean and Constants
  highlight(0, '@boolean.yaml',               { link = "Boolean" })  -- true, false
  highlight(0, '@constant.builtin.yaml',      { link = "Constant" })  -- null, ~

  -- Strings
  highlight(0, '@string.yaml',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.yaml',         { link = "String" })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.yaml',                { link = "Number" })  -- Numbers

  -- Comments
  highlight(0, '@comment.yaml',               { link = "Comment" })  -- Comments
  highlight(0, '@spell.yaml',                 { link = '@comment.yaml'                         })  -- Spell check

  -- Labels and Keys
  highlight(0, '@label.yaml',                 { fg = colors.blue,       bg = 'NONE'            })  -- Anchors and aliases
  highlight(0, '@property.yaml',              { fg = colors.blue,       bg = 'NONE'            })  -- Keys/properties

  -- Types and Directives
  highlight(0, '@type.yaml',                  { link = "Type" })  -- Tags
  highlight(0, '@keyword.directive.yaml',     { link = "Keyword" })  -- %YAML, %TAG

  -- Punctuation
  highlight(0, '@punctuation.delimiter.yaml', { link = "Delimiter" })  -- : , -
  highlight(0, '@punctuation.bracket.yaml',   { fg = colors.white,      bg = 'NONE'            })  -- [ ] { }


  -------------------------------------------------------------------------
  -- Treesitter Docker Compose Captures (if docker-compose parser exists)
  -------------------------------------------------------------------------

  -- These use the generic YAML captures since docker-compose is YAML
  -- But we provide docker-compose specific captures for future compatibility

  highlight(0, '@property.docker-compose',    { fg = colors.blue,       bg = 'NONE'            })  -- Keys
  highlight(0, '@string.docker-compose',      { link = "String" })  -- Strings
  highlight(0, '@number.docker-compose',      { link = "Number" })  -- Numbers
  highlight(0, '@boolean.docker-compose',     { link = "Boolean" })  -- Booleans
  highlight(0, '@constant.builtin.docker-compose', { link = "Constant" })  -- null
  highlight(0, '@comment.docker-compose',     { link = "Comment" })  -- Comments
  highlight(0, '@punctuation.delimiter.docker-compose', { link = "Delimiter" })  -- Delimiters
  highlight(0, '@punctuation.bracket.docker-compose', { fg = colors.white, bg = 'NONE'         })  -- Brackets
  highlight(0, '@punctuation.special.docker-compose', { fg = colors.orange, bg = 'NONE'        })  -- Special chars


  -------------------------------------------------------------------------
  -- LSP Semantic Tokens for YAML/Docker Compose
  -------------------------------------------------------------------------

  highlight(0, '@lsp.type.property.yaml',     { fg = colors.blue,       bg = 'NONE'            })  -- Properties/keys
  highlight(0, '@lsp.type.string.yaml',       { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.yaml',       { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.yaml',      { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.comment.yaml',      { link = "Comment" })  -- Comments

  highlight(0, '@lsp.type.property.dockercompose', { fg = colors.blue,  bg = 'NONE'            })  -- Properties
  highlight(0, '@lsp.type.string.dockercompose', { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.dockercompose', { link = "Number" })  -- Numbers

end

return dockerCompose
