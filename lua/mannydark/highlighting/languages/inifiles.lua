-------------------------------------------------------------------------------
-- INI and Configuration Files
-- Highlighting for .ini, .cfg, .conf, .editorconfig, .gitconfig, .desktop,
-- .properties, .npmrc, .pip.conf, systemd units, ssh_config, and similar files.
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local ini       = {}


-------------------------------------------------------------------------------
-- Settings

ini.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups - Generic INI (dosini)

  -- Sections
  highlight(0, 'dosiniHeader',         { fg = colors.blue,       bg = 'NONE', bold = true })  -- [Section]
  highlight(0, 'dosiniSection',        { fg = colors.white,      bg = 'NONE'            })  -- Section content

  -- Keys and Values
  highlight(0, 'dosiniLabel',          { fg = colors.turquoise,  bg = 'NONE'            })  -- key =
  highlight(0, 'dosiniValue',          { fg = colors.redLight,   bg = 'NONE'            })  -- = value
  highlight(0, 'dosiniNumber',         { link = "Number" })  -- Numeric values
  highlight(0, 'dosiniString',         { link = "String" })  -- "string values"

  -- Comments
  highlight(0, 'dosiniComment',        { link = "Comment" })  -- ; comment or # comment
  highlight(0, 'dosiniTodo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Operators
  highlight(0, 'dosiniEqual',          { fg = colors.white,      bg = 'NONE'            })  -- = sign
  highlight(0, 'dosiniDelimiter',      { link = "Delimiter" })  -- Delimiters

  -- Special
  highlight(0, 'dosiniInterpolation',  { fg = colors.purple,     bg = 'NONE'            })  -- ${variable} or %(variable)s
  highlight(0, 'dosiniEscape',         { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences


  -----------------------------------------------------------------------------
  -- Git Config (.gitconfig, .git/config, .gitmodules)

  -- Structure
  highlight(0, 'gitconfigSection',     { fg = colors.blue,       bg = 'NONE', bold = true })  -- [section]
  highlight(0, 'gitconfigSubsection',  { fg = colors.pink,       bg = 'NONE'            })  -- [section "subsection"]

  -- Keys and Values
  highlight(0, 'gitconfigVariable',    { link = "Variable" })  -- Variable names
  highlight(0, 'gitconfigAssignment',  { fg = colors.redLight,   bg = 'NONE'            })  -- Assignment values
  highlight(0, 'gitconfigString',      { link = "String" })  -- "string values"
  highlight(0, 'gitconfigBoolean',     { link = "Boolean" })  -- true, false
  highlight(0, 'gitconfigNumber',      { link = "Number" })  -- Numbers

  -- Syntax
  highlight(0, 'gitconfigDelim',       { link = "Delimiter" })  -- Delimiters
  highlight(0, 'gitconfigEscape',      { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, 'gitconfigEscapeError', { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Invalid escapes

  -- Comments
  highlight(0, 'gitconfigComment',     { link = "Comment" })  -- # or ; comments

  -- Errors
  highlight(0, 'gitconfigError',       { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'gitconfigNone',        { fg = colors.white,      bg = 'NONE'            })  -- Default


  -----------------------------------------------------------------------------
  -- SSH Config (ssh_config, sshd_config, ~/.ssh/config)

  -- Keywords
  highlight(0, 'sshconfigKeyword',     { link = "Keyword" })  -- Config directives
  highlight(0, 'sshconfigDeprecated',  { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- Deprecated options

  -- Sections
  highlight(0, 'sshconfigHostSect',    { fg = colors.blue,       bg = 'NONE', bold = true })  -- Host section
  highlight(0, 'sshconfigMatch',       { fg = colors.blue,       bg = 'NONE', bold = true })  -- Match section

  -- Values
  highlight(0, 'sshconfigYesNo',       { fg = colors.blue,       bg = 'NONE'            })  -- yes/no
  highlight(0, 'sshconfigNumber',      { link = "Number" })  -- Numbers
  highlight(0, 'sshconfigHostPort',    { fg = colors.greenLight, bg = 'NONE'            })  -- host:port
  highlight(0, 'sshconfigConstant',    { link = "Constant" })  -- Constants
  highlight(0, 'sshconfigEnum',        { fg = colors.purple,     bg = 'NONE'            })  -- Enum values

  -- Cryptographic Options
  highlight(0, 'sshconfigCipher',      { fg = colors.orange,     bg = 'NONE'            })  -- Single cipher
  highlight(0, 'sshconfigCiphers',     { fg = colors.orange,     bg = 'NONE'            })  -- Cipher list
  highlight(0, 'sshconfigMAC',         { fg = colors.orange,     bg = 'NONE'            })  -- MAC algorithms
  highlight(0, 'sshconfigHostKeyAlgo', { fg = colors.orange,     bg = 'NONE'            })  -- Host key algorithms
  highlight(0, 'sshconfigKexAlgo',     { fg = colors.orange,     bg = 'NONE'            })  -- Key exchange algorithms

  -- Authentication
  highlight(0, 'sshconfigPreferredAuth', { fg = colors.purple,   bg = 'NONE'            })  -- Auth methods
  highlight(0, 'sshconfigKbdInteractive', { fg = colors.purple,  bg = 'NONE'            })  -- Keyboard-interactive

  -- Logging
  highlight(0, 'sshconfigLogLevel',    { fg = colors.purple,     bg = 'NONE'            })  -- Log levels
  highlight(0, 'sshconfigSysLogFacility', { fg = colors.purple,  bg = 'NONE'            })  -- Syslog facility

  -- Network
  highlight(0, 'sshconfigAddressFamily', { fg = colors.purple,   bg = 'NONE'            })  -- any/inet/inet6
  highlight(0, 'sshconfigIPQoS',       { fg = colors.purple,     bg = 'NONE'            })  -- IP QoS settings
  highlight(0, 'sshconfigTunnel',      { fg = colors.purple,     bg = 'NONE'            })  -- Tunnel mode

  -- Special
  highlight(0, 'sshconfigVar',         { link = "Variable" })  -- %h, %p, etc.
  highlight(0, 'sshconfigSpecial',     { fg = colors.pink,       bg = 'NONE'            })  -- Special chars

  -- Comments
  highlight(0, 'sshconfigComment',     { link = "Comment" })  -- # comments
  highlight(0, 'sshconfigTodo',        { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME


  -----------------------------------------------------------------------------
  -- Desktop Entry Files (.desktop, .directory)

  -- Structure
  highlight(0, 'dtGroup',              { fg = colors.blue,       bg = 'NONE', bold = true })  -- [Desktop Entry]
  highlight(0, 'dtDelim',              { link = "Delimiter" })  -- = sign

  -- Standard Keys
  highlight(0, 'dtTypeKey',            { link = "Type" })  -- Type=
  highlight(0, 'dtVersionKey',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Version=
  highlight(0, 'dtStringKey',          { link = "String" })  -- Name=, Comment=, etc.
  highlight(0, 'dtLocalestringKey',    { link = "String" })  -- Name[en]=
  highlight(0, 'dtIconstringKey',      { link = "String" })  -- Icon=
  highlight(0, 'dtBooleanKey',         { link = "Boolean" })  -- NoDisplay=, Hidden=
  highlight(0, 'dtNumericKey',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Numeric keys
  highlight(0, 'dtExecKey',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Exec=, TryExec=
  highlight(0, 'dtCategoriesKey',      { fg = colors.turquoise,  bg = 'NONE'            })  -- Categories=

  -- Values
  highlight(0, 'dtTypeValue',          { link = "Type" })  -- Application, Directory, Link
  highlight(0, 'dtVersionValue',       { fg = colors.greenLight, bg = 'NONE'            })  -- 1.0, 1.1, etc.
  highlight(0, 'dtBooleanValue',       { link = "Boolean" })  -- true, false
  highlight(0, 'dtNumericDecimal',     { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'dtCategoriesValue',    { fg = colors.orange,     bg = 'NONE'            })  -- Category names
  highlight(0, 'dtExecParam',          { fg = colors.pink,       bg = 'NONE'            })  -- %f, %u, %F, %U, etc.
  highlight(0, 'dtLocaleSuffix',       { fg = colors.gray,       bg = 'NONE'            })  -- [en_US]

  -- Extensions
  highlight(0, 'dtXExtensionKey',      { fg = colors.purple,     bg = 'NONE'            })  -- X-* keys
  highlight(0, 'dtXExtension',         { fg = colors.purple,     bg = 'NONE'            })  -- X-* values
  highlight(0, 'dtNonStdLabelKey',     { fg = colors.gray,       bg = 'NONE'            })  -- Non-standard keys

  -- Comments and Errors
  highlight(0, 'dtComment',            { link = "Comment" })  -- # comments
  highlight(0, 'dtError',              { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Errors


  -----------------------------------------------------------------------------
  -- Systemd Unit Files (.service, .socket, .timer, .mount, .target, etc.)

  -- Sections
  highlight(0, 'sdHeader',             { fg = colors.blue,       bg = 'NONE', bold = true })  -- [Unit], [Service], etc.

  -- Section Keys
  highlight(0, 'sdUnitKey',            { fg = colors.turquoise,  bg = 'NONE'            })  -- [Unit] keys
  highlight(0, 'sdInstallKey',         { fg = colors.turquoise,  bg = 'NONE'            })  -- [Install] keys
  highlight(0, 'sdServiceKey',         { fg = colors.turquoise,  bg = 'NONE'            })  -- [Service] keys
  highlight(0, 'sdSocketKey',          { fg = colors.turquoise,  bg = 'NONE'            })  -- [Socket] keys
  highlight(0, 'sdTimerKey',           { fg = colors.turquoise,  bg = 'NONE'            })  -- [Timer] keys
  highlight(0, 'sdMountKey',           { fg = colors.turquoise,  bg = 'NONE'            })  -- [Mount] keys
  highlight(0, 'sdSwapKey',            { fg = colors.turquoise,  bg = 'NONE'            })  -- [Swap] keys
  highlight(0, 'sdPathKey',            { fg = colors.turquoise,  bg = 'NONE'            })  -- [Path] keys
  highlight(0, 'sdExecKey',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Exec* keys
  highlight(0, 'sdKillKey',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Kill* keys
  highlight(0, 'sdResCtlKey',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Resource control keys

  -- Values
  highlight(0, 'sdBool',               { fg = colors.blue,       bg = 'NONE'            })  -- yes/no, true/false, on/off
  highlight(0, 'sdInt',                { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'sdUInt',               { fg = colors.greenLight, bg = 'NONE'            })  -- Unsigned integers
  highlight(0, 'sdOctal',              { fg = colors.greenLight, bg = 'NONE'            })  -- Octal values (0755)
  highlight(0, 'sdDuration',           { fg = colors.greenLight, bg = 'NONE'            })  -- 5min, 30s, 1h
  highlight(0, 'sdDatasize',           { fg = colors.greenLight, bg = 'NONE'            })  -- 1K, 50M, 2G
  highlight(0, 'sdPercent',            { fg = colors.greenLight, bg = 'NONE'            })  -- 50%
  highlight(0, 'sdFilename',           { fg = colors.redLight,   bg = 'NONE'            })  -- /path/to/file

  -- Special Values
  highlight(0, 'sdServiceType',        { link = "Type" })  -- simple, exec, forking, etc.
  highlight(0, 'sdRestartType',        { link = "Type" })  -- always, on-failure, etc.
  highlight(0, 'sdSignalName',         { fg = colors.orange,     bg = 'NONE'            })  -- SIGHUP, SIGTERM, etc.
  highlight(0, 'sdExitStatusName',     { fg = colors.orange,     bg = 'NONE'            })  -- EXIT_SUCCESS, etc.
  highlight(0, 'sdCapName',            { fg = colors.orange,     bg = 'NONE'            })  -- CAP_NET_ADMIN, etc.

  -- Symbols
  highlight(0, 'sdSymbol',             { fg = colors.pink,       bg = 'NONE'            })  -- Special symbols
  highlight(0, 'sdEnvVar',             { link = "Variable" })  -- ${VARIABLE}
  highlight(0, 'sdSpecifier',          { fg = colors.purple,     bg = 'NONE'            })  -- %n, %i, %u, etc.

  -- Directives
  highlight(0, 'sdInclude',            { fg = colors.pink,       bg = 'NONE'            })  -- .include

  -- Comments and Errors
  highlight(0, 'sdComment',            { link = "Comment" })  -- # or ; comments
  highlight(0, 'sdTodo',               { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME
  highlight(0, 'sdErr',                { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Errors


  -----------------------------------------------------------------------------
  -- Generic Conf Files (.conf, config files)

  highlight(0, 'confComment',          { link = "Comment" })  -- # comments
  highlight(0, 'confTodo',             { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX
  highlight(0, 'confString',           { link = "String" })  -- "strings"


  -----------------------------------------------------------------------------
  -- EditorConfig (.editorconfig)

  -- Sections
  highlight(0, 'editorconfigSection',  { fg = colors.blue,       bg = 'NONE', bold = true })  -- [*.lua]
  highlight(0, 'editorconfigGlob',     { fg = colors.pink,       bg = 'NONE'            })  -- *.lua, **.py

  -- Properties
  highlight(0, 'editorconfigProperty', { fg = colors.turquoise,  bg = 'NONE'            })  -- Property names

  -- Standard Properties (specific highlighting)
  highlight(0, 'editorconfigIndentStyle', { fg = colors.turquoise, bg = 'NONE'          })  -- indent_style
  highlight(0, 'editorconfigIndentSize', { fg = colors.turquoise, bg = 'NONE'           })  -- indent_size
  highlight(0, 'editorconfigTabWidth', { fg = colors.turquoise,  bg = 'NONE'            })  -- tab_width
  highlight(0, 'editorconfigEndOfLine', { fg = colors.turquoise, bg = 'NONE'            })  -- end_of_line
  highlight(0, 'editorconfigCharset',  { fg = colors.turquoise,  bg = 'NONE'            })  -- charset
  highlight(0, 'editorconfigTrimWhitespace', { fg = colors.turquoise, bg = 'NONE'       })  -- trim_trailing_whitespace
  highlight(0, 'editorconfigInsertNewline', { fg = colors.turquoise, bg = 'NONE'        })  -- insert_final_newline
  highlight(0, 'editorconfigRoot',     { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- root

  -- Values
  highlight(0, 'editorconfigValue',    { fg = colors.redLight,   bg = 'NONE'            })  -- Values
  highlight(0, 'editorconfigBoolean',  { link = "Boolean" })  -- true, false
  highlight(0, 'editorconfigNumber',   { link = "Number" })  -- Numbers
  highlight(0, 'editorconfigIndentValue', { fg = colors.purple,  bg = 'NONE'            })  -- tab, space

  -- Comments
  highlight(0, 'editorconfigComment',  { link = "Comment" })  -- # or ; comments


  -----------------------------------------------------------------------------
  -- Java Properties Files (.properties)

  -- Keys and Values
  highlight(0, 'jpropertiesIdentifier', { fg = colors.turquoise, bg = 'NONE'            })  -- Property keys
  highlight(0, 'jpropertiesString',    { link = "String" })  -- Property values
  highlight(0, 'jpropertiesDelimiter', { link = "Delimiter" })  -- = or :

  -- Special
  highlight(0, 'jpropertiesSpecial',   { fg = colors.pink,       bg = 'NONE'            })  -- Unicode escapes \uXXXX
  highlight(0, 'jpropertiesSpecialError', { fg = colors.red,     bg = 'NONE', undercurl = true })  -- Invalid escapes

  -- Comments
  highlight(0, 'jpropertiesComment',   { link = "Comment" })  -- # or ! comments


  -----------------------------------------------------------------------------
  -- NPM/Yarn Configuration (.npmrc, .yarnrc)

  highlight(0, 'npmrcKey',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Config keys
  highlight(0, 'npmrcValue',           { fg = colors.redLight,   bg = 'NONE'            })  -- Config values
  highlight(0, 'npmrcUrl',             { fg = colors.blueLink,   bg = 'NONE'            })  -- Registry URLs
  highlight(0, 'npmrcBoolean',         { link = "Boolean" })  -- true, false
  highlight(0, 'npmrcComment',         { link = "Comment" })  -- # or ; comments
  highlight(0, 'npmrcVariable',        { link = "Variable" })  -- ${VAR} interpolation


  -----------------------------------------------------------------------------
  -- Pip Configuration (pip.conf, pip.ini)

  highlight(0, 'pipconfSection',       { fg = colors.blue,       bg = 'NONE', bold = true })  -- [global], [install]
  highlight(0, 'pipconfKey',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Config keys
  highlight(0, 'pipconfValue',         { fg = colors.redLight,   bg = 'NONE'            })  -- Config values
  highlight(0, 'pipconfUrl',           { fg = colors.blueLink,   bg = 'NONE'            })  -- Index URLs
  highlight(0, 'pipconfBoolean',       { link = "Boolean" })  -- true, false, yes, no
  highlight(0, 'pipconfComment',       { link = "Comment" })  -- # comments


  -----------------------------------------------------------------------------
  -- Windows Registry Files (.reg)

  highlight(0, 'registryHead',         { fg = colors.gray,       bg = 'NONE'            })  -- Windows Registry Editor Version
  highlight(0, 'registryPath',         { fg = colors.blue,       bg = 'NONE', bold = true })  -- [HKEY_...]
  highlight(0, 'registryKey',          { fg = colors.turquoise,  bg = 'NONE'            })  -- "ValueName"
  highlight(0, 'registryString',       { link = "String" })  -- "string value"
  highlight(0, 'registryDword',        { fg = colors.greenLight, bg = 'NONE'            })  -- dword:00000001
  highlight(0, 'registryHex',          { fg = colors.greenLight, bg = 'NONE'            })  -- hex:...
  highlight(0, 'registryDelete',       { fg = colors.red,        bg = 'NONE'            })  -- - (delete marker)
  highlight(0, 'registryComment',      { link = "Comment" })  -- ; comments


  -----------------------------------------------------------------------------
  -- Apache/Nginx-style Config

  highlight(0, 'apacheDeclaration',    { fg = colors.blue,       bg = 'NONE'            })  -- Directives
  highlight(0, 'apacheSection',        { fg = colors.blue,       bg = 'NONE', bold = true })  -- <Section>
  highlight(0, 'apacheOption',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Option names
  highlight(0, 'apacheString',         { link = "String" })  -- "strings"
  highlight(0, 'apacheComment',        { link = "Comment" })  -- # comments
  highlight(0, 'apacheVariable',       { link = "Variable" })  -- ${VAR}, %{VAR}


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.ini)

  -- Structure
  highlight(0, '@markup.heading.ini',          { fg = colors.blue,       bg = 'NONE', bold = true })  -- [Section]
  highlight(0, '@punctuation.bracket.ini',     { fg = colors.white,      bg = 'NONE' })  -- [ ]

  -- Keys and Values
  highlight(0, '@property.ini',                { fg = colors.turquoise,  bg = 'NONE' })  -- Keys
  highlight(0, '@string.ini',                  { link = "String" })  -- Values
  highlight(0, '@number.ini',                  { link = "Number" })  -- Numbers
  highlight(0, '@boolean.ini',                 { link = "Boolean" })  -- true/false

  -- Operators
  highlight(0, '@operator.ini',                { link = "Operator" })  -- =

  -- Comments
  highlight(0, '@comment.ini',                 { link = "Comment" })  -- Comments
  highlight(0, '@spell.ini',                   { fg = colors.red,        bg = 'NONE' })  -- Spell-check comments

  -- Special
  highlight(0, '@variable.ini',                { link = "Variable" })  -- Variables/Interpolation
  highlight(0, '@constant.ini',                { link = "Constant" })  -- Constants


  -----------------------------------------------------------------------------
  -- Treesitter Groups for specific formats

  -- Git config
  highlight(0, '@property.gitconfig',          { fg = colors.turquoise,  bg = 'NONE' })  -- Variable names
  highlight(0, '@string.gitconfig',            { link = "String" })  -- Values
  highlight(0, '@markup.heading.gitconfig',    { fg = colors.blue,       bg = 'NONE', bold = true })  -- [section]
  highlight(0, '@comment.gitconfig',           { link = "Comment" })  -- Comments

  -- SSH config
  highlight(0, '@keyword.sshconfig',           { link = "Keyword" })  -- Directives
  highlight(0, '@string.sshconfig',            { link = "String" })  -- Values
  highlight(0, '@markup.heading.sshconfig',    { fg = colors.blue,       bg = 'NONE', bold = true })  -- Host
  highlight(0, '@comment.sshconfig',           { link = "Comment" })  -- Comments

  -- Properties files
  highlight(0, '@property.properties',         { fg = colors.turquoise,  bg = 'NONE' })  -- Keys
  highlight(0, '@string.properties',           { link = "String" })  -- Values
  highlight(0, '@comment.properties',          { link = "Comment" })  -- Comments


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens

  highlight(0, '@lsp.type.property.ini',       { fg = colors.turquoise,  bg = 'NONE' })  -- Keys
  highlight(0, '@lsp.type.string.ini',         { link = "String" })  -- Values
  highlight(0, '@lsp.type.number.ini',         { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.ini',        { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.comment.ini',        { link = "Comment" })  -- Comments


  -----------------------------------------------------------------------------
  -- Links for common filetype aliases

  highlight(0, 'cfgSection',           { link = 'dosiniHeader'     })
  highlight(0, 'cfgLabel',             { link = 'dosiniLabel'      })
  highlight(0, 'cfgValue',             { link = 'dosiniValue'      })
  highlight(0, 'cfgComment',           { link = "Comment" })
  highlight(0, 'cfgString',            { link = "String" })

  highlight(0, 'iniSection',           { link = 'dosiniHeader'     })
  highlight(0, 'iniKey',               { link = 'dosiniLabel'      })
  highlight(0, 'iniValue',             { link = 'dosiniValue'      })
  highlight(0, 'iniComment',           { link = "Comment" })
end

return ini
