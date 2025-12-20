-------------------------------------------------------------------------------
-- HCL Files (HashiCorp Configuration Language)
-- Highlighting for .hcl, .tf, .tfvars files (Terraform, Packer, Vault, etc.)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local hcl       = {}


-------------------------------------------------------------------------------
-- Settings

hcl.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - HCL Base

  -- Comments
  highlight(0, 'hclComment',               { link = "Comment" })  -- # and // comments
  highlight(0, 'hclBlockComment',          { link = "Comment" })  -- /* */ block comments
  highlight(0, 'hclTodo',                  { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX, BUG

  -- Block Types
  highlight(0, 'hclBlockType',             { link = "Type" })  -- resource, variable, etc.
  highlight(0, 'hclBlockLabel',            { fg = colors.turquoise,  bg = 'NONE' })  -- Block labels/names
  highlight(0, 'hclBlockBody',             { fg = colors.white,      bg = 'NONE' })  -- Block content

  -- Attributes
  highlight(0, 'hclAttributeName',         { fg = colors.blue,       bg = 'NONE' })  -- Attribute/argument names
  highlight(0, 'hclAttributeAssignment',   { fg = colors.white,      bg = 'NONE' })  -- = operator

  -- Values
  highlight(0, 'hclValueBool',             { fg = colors.blue,       bg = 'NONE' })  -- true, false
  highlight(0, 'hclValueNull',             { fg = colors.blue,       bg = 'NONE' })  -- null
  highlight(0, 'hclValueDec',              { fg = colors.greenLight, bg = 'NONE' })  -- Decimal numbers
  highlight(0, 'hclValueHexaDec',          { fg = colors.greenLight, bg = 'NONE' })  -- Hexadecimal numbers
  highlight(0, 'hclValueFloat',            { fg = colors.greenLight, bg = 'NONE' })  -- Float numbers

  -- Strings
  highlight(0, 'hclValueString',           { link = "String" })  -- "string"
  highlight(0, 'hclStringInterp',          { link = "String" })  -- ${...} interpolation
  highlight(0, 'hclTemplateInterp',        { fg = colors.purple,     bg = 'NONE' })  -- %{...} template directive
  highlight(0, 'hclHereDoc',               { fg = colors.redLight,   bg = 'NONE' })  -- <<EOF heredocs
  highlight(0, 'hclHereDocText',           { fg = colors.redLight,   bg = 'NONE' })  -- Heredoc content
  highlight(0, 'hclHereDocDelim',          { link = "Delimiter" })  -- EOF delimiter

  -- Escape Sequences
  highlight(0, 'hclEscape',                { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, \\, etc.
  highlight(0, 'hclStringEscape',          { link = "String" })  -- Escape in strings

  -- Functions
  highlight(0, 'hclFunction',              { link = "Function" })  -- Function calls

  -- Brackets and Delimiters
  highlight(0, 'hclBraces',                { fg = colors.white,      bg = 'NONE' })  -- { } [ ]
  highlight(0, 'hclBracket',               { fg = colors.white,      bg = 'NONE' })  -- [ ]
  highlight(0, 'hclBrace',                 { fg = colors.white,      bg = 'NONE' })  -- { }
  highlight(0, 'hclParen',                 { fg = colors.white,      bg = 'NONE' })  -- ( )
  highlight(0, 'hclComma',                 { fg = colors.white,      bg = 'NONE' })  -- ,
  highlight(0, 'hclDot',                   { fg = colors.white,      bg = 'NONE' })  -- .
  highlight(0, 'hclColon',                 { fg = colors.white,      bg = 'NONE' })  -- :

  -- Operators
  highlight(0, 'hclArithmeticOp',          { fg = colors.white,      bg = 'NONE' })  -- + - * / %
  highlight(0, 'hclComparisonOp',          { fg = colors.white,      bg = 'NONE' })  -- == != < > <= >=
  highlight(0, 'hclLogicalOp',             { fg = colors.white,      bg = 'NONE' })  -- && || !
  highlight(0, 'hclTernary',               { fg = colors.pink,       bg = 'NONE' })  -- ? :
  highlight(0, 'hclSplat',                 { fg = colors.pink,       bg = 'NONE' })  -- [*] .*
  highlight(0, 'hclEllipsis',              { fg = colors.pink,       bg = 'NONE' })  -- ...
  highlight(0, 'hclArrow',                 { fg = colors.pink,       bg = 'NONE' })  -- =>

  -- Control Flow (for expressions)
  highlight(0, 'hclRepeat',                { fg = colors.pink,       bg = 'NONE' })  -- for, in
  highlight(0, 'hclConditional',           { link = "Conditional" })  -- if, else
  highlight(0, 'hclForExpr',               { fg = colors.pink,       bg = 'NONE' })  -- for expression
  highlight(0, 'hclEndfor',                { fg = colors.pink,       bg = 'NONE' })  -- endfor
  highlight(0, 'hclEndif',                 { fg = colors.pink,       bg = 'NONE' })  -- endif


  -----------------------------------------------------------------------------
  -- Terraform-specific Vim Syntax Groups

  -- Block Types (Terraform)
  highlight(0, 'terraBlockType',           { link = "Type" })  -- resource, data, module, etc.
  highlight(0, 'terraResourceType',        { link = "Type" })  -- aws_instance, google_compute, etc.
  highlight(0, 'terraResourceName',        { fg = colors.turquoise,  bg = 'NONE' })  -- Resource name label
  highlight(0, 'terraDataSource',          { fg = colors.turquoise,  bg = 'NONE' })  -- Data source type

  -- Types
  highlight(0, 'terraType',                { link = "Type" })  -- string, bool, number, etc.
  highlight(0, 'terraPrimitiveType',       { link = "Type" })  -- string, bool, number
  highlight(0, 'terraCollectionType',      { link = "Type" })  -- list, map, set
  highlight(0, 'terraStructuralType',      { link = "Type" })  -- object, tuple
  highlight(0, 'terraAnyType',             { link = "Type" })  -- any

  -- Built-in Variables
  highlight(0, 'terraBuiltinVar',          { link = "Variable" })  -- var, local, module, data
  highlight(0, 'terraVarRef',              { link = "Variable" })  -- var.name
  highlight(0, 'terraLocalRef',            { fg = colors.purple,     bg = 'NONE' })  -- local.name
  highlight(0, 'terraModuleRef',           { fg = colors.purple,     bg = 'NONE' })  -- module.name
  highlight(0, 'terraDataRef',             { fg = colors.purple,     bg = 'NONE' })  -- data.type.name
  highlight(0, 'terraPathRef',             { fg = colors.purple,     bg = 'NONE' })  -- path.module, path.root
  highlight(0, 'terraTerraformRef',        { fg = colors.purple,     bg = 'NONE' })  -- terraform.workspace

  -- Special Variables
  highlight(0, 'terraCount',               { fg = colors.purple,     bg = 'NONE' })  -- count, count.index
  highlight(0, 'terraEach',                { fg = colors.purple,     bg = 'NONE' })  -- each, each.key, each.value
  highlight(0, 'terraSelf',                { fg = colors.purple,     bg = 'NONE' })  -- self

  -- Meta-Arguments
  highlight(0, 'terraMetaArg',             { fg = colors.blue,       bg = 'NONE' })  -- depends_on, count, for_each
  highlight(0, 'terraDependsOn',           { fg = colors.blue,       bg = 'NONE' })  -- depends_on
  highlight(0, 'terraForEach',             { fg = colors.blue,       bg = 'NONE' })  -- for_each
  highlight(0, 'terraProvider',            { fg = colors.blue,       bg = 'NONE' })  -- provider (meta-arg)
  highlight(0, 'terraLifecycle',           { fg = colors.blue,       bg = 'NONE' })  -- lifecycle
  highlight(0, 'terraSource',              { fg = colors.blue,       bg = 'NONE' })  -- source
  highlight(0, 'terraVersion',             { fg = colors.blue,       bg = 'NONE' })  -- version

  -- Lifecycle Attributes
  highlight(0, 'terraLifecycleAttr',       { fg = colors.blue,       bg = 'NONE' })  -- create_before_destroy, etc.
  highlight(0, 'terraCreateBeforeDestroy', { fg = colors.blue,       bg = 'NONE' })  -- create_before_destroy
  highlight(0, 'terraPreventDestroy',      { fg = colors.blue,       bg = 'NONE' })  -- prevent_destroy
  highlight(0, 'terraIgnoreChanges',       { fg = colors.blue,       bg = 'NONE' })  -- ignore_changes
  highlight(0, 'terraReplaceTriggeredBy',  { fg = colors.blue,       bg = 'NONE' })  -- replace_triggered_by

  -- Variable Block Attributes
  highlight(0, 'terraVarAttr',             { fg = colors.blue,       bg = 'NONE' })  -- default, type, description
  highlight(0, 'terraVarDefault',          { fg = colors.blue,       bg = 'NONE' })  -- default
  highlight(0, 'terraVarType',             { link = "Type" })  -- type
  highlight(0, 'terraVarDescription',      { fg = colors.blue,       bg = 'NONE' })  -- description
  highlight(0, 'terraVarValidation',       { fg = colors.blue,       bg = 'NONE' })  -- validation
  highlight(0, 'terraVarSensitive',        { fg = colors.blue,       bg = 'NONE' })  -- sensitive
  highlight(0, 'terraVarNullable',         { link = "Variable" })  -- nullable
  highlight(0, 'terraVarEphemeral',        { fg = colors.blue,       bg = 'NONE' })  -- ephemeral (v1.10+)

  -- Output Block Attributes
  highlight(0, 'terraOutputAttr',          { fg = colors.blue,       bg = 'NONE' })  -- value, description, sensitive
  highlight(0, 'terraOutputValue',         { fg = colors.blue,       bg = 'NONE' })  -- value
  highlight(0, 'terraOutputSensitive',     { fg = colors.blue,       bg = 'NONE' })  -- sensitive

  -- Provisioners
  highlight(0, 'terraProvisioner',         { fg = colors.pink,       bg = 'NONE' })  -- provisioner block
  highlight(0, 'terraProvisionerType',     { link = "Type" })  -- local-exec, remote-exec
  highlight(0, 'terraProvisionerAttr',     { fg = colors.blue,       bg = 'NONE' })  -- command, inline, script
  highlight(0, 'terraConnection',          { fg = colors.pink,       bg = 'NONE' })  -- connection block


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.hcl)

  -- Comments
  highlight(0, '@comment.hcl',             { link = "Comment" })  -- Comments

  -- Keywords
  highlight(0, '@keyword.hcl',             { link = "Keyword" })  -- Block identifiers
  highlight(0, '@keyword.repeat.hcl',      { link = "Keyword" })  -- for, endfor, in
  highlight(0, '@keyword.conditional.hcl', { link = "Conditional" })  -- if, else, endif

  -- Types
  highlight(0, '@type.hcl',                { link = "Type" })  -- Nested block identifiers
  highlight(0, '@type.builtin.hcl',        { link = "Type" })  -- Type keywords

  -- Variables
  highlight(0, '@variable.hcl',            { link = "Variable" })  -- Identifiers
  highlight(0, '@variable.builtin.hcl',    { link = "Variable" })  -- var, data, local, module
  highlight(0, '@variable.member.hcl',     { link = "Variable" })  -- Object attributes/keys

  -- Functions
  highlight(0, '@function.hcl',            { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.hcl',    { link = "Function" })  -- Built-in functions

  -- Strings
  highlight(0, '@string.hcl',              { link = "String" })  -- String literals
  highlight(0, '@string.escape.hcl',       { link = "String" })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.hcl',              { link = "Number" })  -- Numbers
  highlight(0, '@number.float.hcl',        { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.hcl',             { link = "Boolean" })  -- true, false

  -- Constants
  highlight(0, '@constant.hcl',            { link = "Constant" })  -- null and other constants
  highlight(0, '@constant.builtin.hcl',    { link = "Constant" })  -- null

  -- Operators
  highlight(0, '@operator.hcl',            { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.hcl', { fg = colors.white,      bg = 'NONE' })  -- {}, [], ()
  highlight(0, '@punctuation.delimiter.hcl', { link = "Delimiter" })  -- , . [*] .*
  highlight(0, '@punctuation.special.hcl', { fg = colors.pink,       bg = 'NONE' })  -- ..., ?, =>

  -- None (assignment)
  highlight(0, '@none.hcl',                { fg = colors.white,      bg = 'NONE' })  -- : =


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.terraform)

  -- Comments
  highlight(0, '@comment.terraform',             { link = "Comment" })  -- Comments

  -- Keywords
  highlight(0, '@keyword.terraform',             { link = "Keyword" })  -- Block types
  highlight(0, '@keyword.repeat.terraform',      { link = "Keyword" })  -- for, in
  highlight(0, '@keyword.conditional.terraform', { link = "Conditional" })  -- if, else

  -- Types
  highlight(0, '@type.terraform',                { link = "Type" })  -- Resource types, labels
  highlight(0, '@type.builtin.terraform',        { link = "Type" })  -- bool, string, number, etc.

  -- Variables
  highlight(0, '@variable.terraform',            { link = "Variable" })  -- Identifiers
  highlight(0, '@variable.builtin.terraform',    { link = "Variable" })  -- var, local, module, data, path, terraform
  highlight(0, '@variable.member.terraform',     { link = "Variable" })  -- Attributes

  -- Functions
  highlight(0, '@function.terraform',            { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.terraform',    { link = "Function" })  -- Built-in functions

  -- Strings
  highlight(0, '@string.terraform',              { link = "String" })  -- Strings
  highlight(0, '@string.escape.terraform',       { link = "String" })  -- Escapes

  -- Numbers
  highlight(0, '@number.terraform',              { link = "Number" })  -- Numbers

  -- Booleans
  highlight(0, '@boolean.terraform',             { link = "Boolean" })  -- true, false

  -- Constants
  highlight(0, '@constant.builtin.terraform',    { link = "Constant" })  -- null

  -- Operators
  highlight(0, '@operator.terraform',            { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.terraform', { fg = colors.white,      bg = 'NONE' })  -- {}, [], ()
  highlight(0, '@punctuation.delimiter.terraform', { link = "Delimiter" })  -- , .
  highlight(0, '@punctuation.special.terraform', { fg = colors.pink,       bg = 'NONE' })  -- ..., ?, =>


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.hcl / @lsp.type.xxx.terraform)

  -- HCL LSP
  highlight(0, '@lsp.type.property.hcl',     { fg = colors.blue,       bg = 'NONE' })  -- Attributes
  highlight(0, '@lsp.type.type.hcl',         { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.variable.hcl',     { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.function.hcl',     { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.string.hcl',       { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.hcl',       { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.hcl',      { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.comment.hcl',      { link = "Comment" })  -- Comments

  -- Terraform LSP
  highlight(0, '@lsp.type.property.terraform',  { fg = colors.blue,       bg = 'NONE' })  -- Attributes
  highlight(0, '@lsp.type.type.terraform',      { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.variable.terraform',  { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.function.terraform',  { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.string.terraform',    { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.terraform',    { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.terraform',   { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.comment.terraform',   { link = "Comment" })  -- Comments


  -----------------------------------------------------------------------------
  -- Terraform Block Types

  -- Core Block Types
  highlight(0, 'terraBlockResource',       { fg = colors.pink,       bg = 'NONE' })  -- resource
  highlight(0, 'terraBlockData',           { fg = colors.pink,       bg = 'NONE' })  -- data
  highlight(0, 'terraBlockVariable',       { link = "Variable" })  -- variable
  highlight(0, 'terraBlockOutput',         { fg = colors.pink,       bg = 'NONE' })  -- output
  highlight(0, 'terraBlockLocals',         { fg = colors.pink,       bg = 'NONE' })  -- locals
  highlight(0, 'terraBlockModule',         { fg = colors.pink,       bg = 'NONE' })  -- module
  highlight(0, 'terraBlockProvider',       { fg = colors.pink,       bg = 'NONE' })  -- provider
  highlight(0, 'terraBlockTerraform',      { fg = colors.pink,       bg = 'NONE' })  -- terraform

  -- Nested Block Types
  highlight(0, 'terraBlockBackend',        { fg = colors.pink,       bg = 'NONE' })  -- backend
  highlight(0, 'terraBlockRequiredProviders', { fg = colors.pink,    bg = 'NONE' })  -- required_providers
  highlight(0, 'terraBlockDynamic',        { fg = colors.pink,       bg = 'NONE' })  -- dynamic
  highlight(0, 'terraBlockContent',        { fg = colors.pink,       bg = 'NONE' })  -- content (in dynamic)
  highlight(0, 'terraBlockValidation',     { fg = colors.pink,       bg = 'NONE' })  -- validation
  highlight(0, 'terraBlockPrecondition',   { fg = colors.pink,       bg = 'NONE' })  -- precondition
  highlight(0, 'terraBlockPostcondition',  { fg = colors.pink,       bg = 'NONE' })  -- postcondition
  highlight(0, 'terraBlockCheck',          { fg = colors.pink,       bg = 'NONE' })  -- check
  highlight(0, 'terraBlockImport',         { fg = colors.pink,       bg = 'NONE' })  -- import
  highlight(0, 'terraBlockMoved',          { fg = colors.pink,       bg = 'NONE' })  -- moved
  highlight(0, 'terraBlockRemoved',        { fg = colors.pink,       bg = 'NONE' })  -- removed (v1.7+)


  -----------------------------------------------------------------------------
  -- Terraform Built-in Functions (categorized)

  -- Numeric Functions
  highlight(0, 'terraFuncNumeric',         { link = "Function" })  -- abs, ceil, floor, etc.
  highlight(0, 'terraFuncAbs',             { link = "Function" })  -- abs
  highlight(0, 'terraFuncCeil',            { link = "Function" })  -- ceil
  highlight(0, 'terraFuncFloor',           { link = "Function" })  -- floor
  highlight(0, 'terraFuncLog',             { link = "Function" })  -- log
  highlight(0, 'terraFuncMax',             { link = "Function" })  -- max
  highlight(0, 'terraFuncMin',             { link = "Function" })  -- min
  highlight(0, 'terraFuncParseint',        { link = "Function" })  -- parseint
  highlight(0, 'terraFuncPow',             { link = "Function" })  -- pow
  highlight(0, 'terraFuncSignum',          { link = "Function" })  -- signum

  -- String Functions
  highlight(0, 'terraFuncString',          { link = "String" })  -- String functions
  highlight(0, 'terraFuncChomp',           { link = "Function" })  -- chomp
  highlight(0, 'terraFuncFormat',          { link = "Function" })  -- format
  highlight(0, 'terraFuncFormatlist',      { link = "Function" })  -- formatlist
  highlight(0, 'terraFuncIndent',          { link = "Function" })  -- indent
  highlight(0, 'terraFuncJoin',            { link = "Function" })  -- join
  highlight(0, 'terraFuncLower',           { link = "Function" })  -- lower
  highlight(0, 'terraFuncUpper',           { link = "Function" })  -- upper
  highlight(0, 'terraFuncRegex',           { link = "Function" })  -- regex
  highlight(0, 'terraFuncRegexall',        { link = "Function" })  -- regexall
  highlight(0, 'terraFuncReplace',         { link = "Function" })  -- replace
  highlight(0, 'terraFuncSplit',           { link = "Function" })  -- split
  highlight(0, 'terraFuncStrrev',          { link = "Function" })  -- strrev
  highlight(0, 'terraFuncSubstr',          { link = "Function" })  -- substr
  highlight(0, 'terraFuncTitle',           { link = "Function" })  -- title
  highlight(0, 'terraFuncTrim',            { link = "Function" })  -- trim
  highlight(0, 'terraFuncTrimprefix',      { link = "Function" })  -- trimprefix
  highlight(0, 'terraFuncTrimsuffix',      { link = "Function" })  -- trimsuffix
  highlight(0, 'terraFuncTrimspace',       { link = "Function" })  -- trimspace

  -- Collection Functions
  highlight(0, 'terraFuncCollection',      { link = "Function" })  -- Collection functions
  highlight(0, 'terraFuncAlltrue',         { link = "Function" })  -- alltrue
  highlight(0, 'terraFuncAnytrue',         { link = "Function" })  -- anytrue
  highlight(0, 'terraFuncChunklist',       { link = "Function" })  -- chunklist
  highlight(0, 'terraFuncCoalesce',        { link = "Function" })  -- coalesce
  highlight(0, 'terraFuncCoalescelist',    { link = "Function" })  -- coalescelist
  highlight(0, 'terraFuncCompact',         { link = "Function" })  -- compact
  highlight(0, 'terraFuncConcat',          { link = "Function" })  -- concat
  highlight(0, 'terraFuncContains',        { link = "Function" })  -- contains
  highlight(0, 'terraFuncDistinct',        { link = "Function" })  -- distinct
  highlight(0, 'terraFuncElement',         { link = "Function" })  -- element
  highlight(0, 'terraFuncFlatten',         { link = "Function" })  -- flatten
  highlight(0, 'terraFuncIndex',           { link = "Function" })  -- index
  highlight(0, 'terraFuncKeys',            { link = "Function" })  -- keys
  highlight(0, 'terraFuncLength',          { link = "Function" })  -- length
  highlight(0, 'terraFuncLookup',          { link = "Function" })  -- lookup
  highlight(0, 'terraFuncMatchkeys',       { link = "Function" })  -- matchkeys
  highlight(0, 'terraFuncMerge',           { link = "Function" })  -- merge
  highlight(0, 'terraFuncOne',             { link = "Function" })  -- one
  highlight(0, 'terraFuncRange',           { link = "Function" })  -- range
  highlight(0, 'terraFuncReverse',         { link = "Function" })  -- reverse
  highlight(0, 'terraFuncSetintersection', { link = "Function" })  -- setintersection
  highlight(0, 'terraFuncSetproduct',      { link = "Function" })  -- setproduct
  highlight(0, 'terraFuncSetsubtract',     { link = "Function" })  -- setsubtract
  highlight(0, 'terraFuncSetunion',        { link = "Function" })  -- setunion
  highlight(0, 'terraFuncSlice',           { link = "Function" })  -- slice
  highlight(0, 'terraFuncSort',            { link = "Function" })  -- sort
  highlight(0, 'terraFuncSum',             { link = "Function" })  -- sum
  highlight(0, 'terraFuncTranspose',       { link = "Function" })  -- transpose
  highlight(0, 'terraFuncValues',          { link = "Function" })  -- values
  highlight(0, 'terraFuncZipmap',          { link = "Function" })  -- zipmap

  -- Encoding Functions
  highlight(0, 'terraFuncEncoding',        { link = "Function" })  -- Encoding functions
  highlight(0, 'terraFuncBase64encode',    { link = "Function" })  -- base64encode
  highlight(0, 'terraFuncBase64decode',    { link = "Function" })  -- base64decode
  highlight(0, 'terraFuncBase64gzip',      { link = "Function" })  -- base64gzip
  highlight(0, 'terraFuncCsvdecode',       { link = "Function" })  -- csvdecode
  highlight(0, 'terraFuncJsonencode',      { link = "Function" })  -- jsonencode
  highlight(0, 'terraFuncJsondecode',      { link = "Function" })  -- jsondecode
  highlight(0, 'terraFuncUrlencode',       { link = "Function" })  -- urlencode
  highlight(0, 'terraFuncYamlencode',      { link = "Function" })  -- yamlencode
  highlight(0, 'terraFuncYamldecode',      { link = "Function" })  -- yamldecode
  highlight(0, 'terraFuncTextencodebase64',{ link = "Function" })  -- textencodebase64
  highlight(0, 'terraFuncTextdecodebase64',{ link = "Function" })  -- textdecodebase64

  -- Filesystem Functions
  highlight(0, 'terraFuncFilesystem',      { link = "Function" })  -- Filesystem functions
  highlight(0, 'terraFuncFile',            { link = "Function" })  -- file
  highlight(0, 'terraFuncFileexists',      { link = "Function" })  -- fileexists
  highlight(0, 'terraFuncFileset',         { link = "Function" })  -- fileset
  highlight(0, 'terraFuncFilebase64',      { link = "Function" })  -- filebase64
  highlight(0, 'terraFuncTemplatefile',    { link = "Function" })  -- templatefile
  highlight(0, 'terraFuncPathexpand',      { link = "Function" })  -- pathexpand
  highlight(0, 'terraFuncDirname',         { link = "Function" })  -- dirname
  highlight(0, 'terraFuncBasename',        { link = "Function" })  -- basename
  highlight(0, 'terraFuncAbspath',         { link = "Function" })  -- abspath

  -- Date/Time Functions
  highlight(0, 'terraFuncDatetime',        { link = "Function" })  -- Date/time functions
  highlight(0, 'terraFuncFormatdate',      { link = "Function" })  -- formatdate
  highlight(0, 'terraFuncTimeadd',         { link = "Function" })  -- timeadd
  highlight(0, 'terraFuncTimecmp',         { link = "Function" })  -- timecmp
  highlight(0, 'terraFuncTimestamp',       { link = "Function" })  -- timestamp
  highlight(0, 'terraFuncPlantimestamp',   { link = "Function" })  -- plantimestamp

  -- Hash/Crypto Functions
  highlight(0, 'terraFuncHash',            { link = "Function" })  -- Hash functions
  highlight(0, 'terraFuncMd5',             { link = "Function" })  -- md5
  highlight(0, 'terraFuncSha1',            { link = "Function" })  -- sha1
  highlight(0, 'terraFuncSha256',          { link = "Function" })  -- sha256
  highlight(0, 'terraFuncSha512',          { link = "Function" })  -- sha512
  highlight(0, 'terraFuncBcrypt',          { link = "Function" })  -- bcrypt
  highlight(0, 'terraFuncRsadecrypt',      { link = "Function" })  -- rsadecrypt
  highlight(0, 'terraFuncUuid',            { link = "Function" })  -- uuid
  highlight(0, 'terraFuncUuidv5',          { link = "Function" })  -- uuidv5

  -- IP Network Functions
  highlight(0, 'terraFuncNetwork',         { link = "Function" })  -- Network functions
  highlight(0, 'terraFuncCidrhost',        { link = "Function" })  -- cidrhost
  highlight(0, 'terraFuncCidrnetmask',     { link = "Function" })  -- cidrnetmask
  highlight(0, 'terraFuncCidrsubnet',      { link = "Function" })  -- cidrsubnet
  highlight(0, 'terraFuncCidrsubnets',     { link = "Function" })  -- cidrsubnets

  -- Type Conversion Functions
  highlight(0, 'terraFuncTypeConv',        { link = "Type" })  -- Type conversion functions
  highlight(0, 'terraFuncCan',             { link = "Function" })  -- can
  highlight(0, 'terraFuncNonsensitive',    { link = "Function" })  -- nonsensitive
  highlight(0, 'terraFuncSensitive',       { link = "Function" })  -- sensitive
  highlight(0, 'terraFuncTobool',          { link = "Function" })  -- tobool
  highlight(0, 'terraFuncTolist',          { link = "Function" })  -- tolist
  highlight(0, 'terraFuncTomap',           { link = "Function" })  -- tomap
  highlight(0, 'terraFuncTonumber',        { link = "Number" })  -- tonumber
  highlight(0, 'terraFuncToset',           { link = "Function" })  -- toset
  highlight(0, 'terraFuncTostring',        { link = "String" })  -- tostring
  highlight(0, 'terraFuncTry',             { link = "Function" })  -- try
  highlight(0, 'terraFuncType',            { link = "Type" })  -- type


  -----------------------------------------------------------------------------
  -- Provider-specific Highlights (common providers)

  -- AWS Provider
  highlight(0, 'terraAWSResource',         { fg = colors.turquoise,  bg = 'NONE' })  -- aws_* resources
  highlight(0, 'terraAWSDataSource',       { fg = colors.turquoise,  bg = 'NONE' })  -- aws_* data sources

  -- Azure Provider
  highlight(0, 'terraAzureResource',       { fg = colors.turquoise,  bg = 'NONE' })  -- azurerm_* resources
  highlight(0, 'terraAzureDataSource',     { fg = colors.turquoise,  bg = 'NONE' })  -- azurerm_* data sources

  -- Google Cloud Provider
  highlight(0, 'terraGCPResource',         { fg = colors.turquoise,  bg = 'NONE' })  -- google_* resources
  highlight(0, 'terraGCPDataSource',       { fg = colors.turquoise,  bg = 'NONE' })  -- google_* data sources

  -- Kubernetes Provider
  highlight(0, 'terraK8sResource',         { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes_* resources
  highlight(0, 'terraK8sDataSource',       { fg = colors.turquoise,  bg = 'NONE' })  -- kubernetes_* data sources

  -- Docker Provider
  highlight(0, 'terraDockerResource',      { fg = colors.turquoise,  bg = 'NONE' })  -- docker_* resources
  highlight(0, 'terraDockerDataSource',    { fg = colors.turquoise,  bg = 'NONE' })  -- docker_* data sources

  -- Null Provider
  highlight(0, 'terraNullResource',        { fg = colors.turquoise,  bg = 'NONE' })  -- null_resource
  highlight(0, 'terraNullDataSource',      { fg = colors.turquoise,  bg = 'NONE' })  -- null_data_source

  -- Random Provider
  highlight(0, 'terraRandomResource',      { fg = colors.turquoise,  bg = 'NONE' })  -- random_* resources

  -- Local Provider
  highlight(0, 'terraLocalResource',       { fg = colors.turquoise,  bg = 'NONE' })  -- local_file, local_sensitive_file

  -- Template Provider
  highlight(0, 'terraTemplateResource',    { fg = colors.turquoise,  bg = 'NONE' })  -- template_* resources


  -----------------------------------------------------------------------------
  -- Other HashiCorp Tools (Packer, Vault, Consul, Nomad)

  -- Packer HCL2
  highlight(0, 'packerBlockSource',        { fg = colors.pink,       bg = 'NONE' })  -- source
  highlight(0, 'packerBlockBuild',         { fg = colors.pink,       bg = 'NONE' })  -- build
  highlight(0, 'packerBlockProvisioner',   { fg = colors.pink,       bg = 'NONE' })  -- provisioner
  highlight(0, 'packerBlockPostProcessor', { fg = colors.pink,       bg = 'NONE' })  -- post-processor
  highlight(0, 'packerBlockPacker',        { fg = colors.pink,       bg = 'NONE' })  -- packer
  highlight(0, 'packerBlockData',          { fg = colors.pink,       bg = 'NONE' })  -- data
  highlight(0, 'packerBlockLocals',        { fg = colors.pink,       bg = 'NONE' })  -- locals
  highlight(0, 'packerBlockVariable',      { link = "Variable" })  -- variable
  highlight(0, 'packerSourceType',         { link = "Type" })  -- amazon-ebs, docker, etc.

  -- Vault Policy
  highlight(0, 'vaultBlockPath',           { fg = colors.pink,       bg = 'NONE' })  -- path block
  highlight(0, 'vaultCapability',          { fg = colors.blue,       bg = 'NONE' })  -- create, read, update, delete, list
  highlight(0, 'vaultPath',                { fg = colors.redLight,   bg = 'NONE' })  -- Path patterns

  -- Consul Configuration
  highlight(0, 'consulBlockService',       { fg = colors.pink,       bg = 'NONE' })  -- service
  highlight(0, 'consulBlockUpstream',      { fg = colors.pink,       bg = 'NONE' })  -- upstream

  -- Nomad Job Spec
  highlight(0, 'nomadBlockJob',            { fg = colors.pink,       bg = 'NONE' })  -- job
  highlight(0, 'nomadBlockGroup',          { fg = colors.pink,       bg = 'NONE' })  -- group
  highlight(0, 'nomadBlockTask',           { fg = colors.pink,       bg = 'NONE' })  -- task
  highlight(0, 'nomadBlockService',        { fg = colors.pink,       bg = 'NONE' })  -- service
  highlight(0, 'nomadBlockNetwork',        { fg = colors.pink,       bg = 'NONE' })  -- network
  highlight(0, 'nomadBlockVolume',         { fg = colors.pink,       bg = 'NONE' })  -- volume
  highlight(0, 'nomadBlockTemplate',       { fg = colors.pink,       bg = 'NONE' })  -- template
  highlight(0, 'nomadBlockArtifact',       { fg = colors.pink,       bg = 'NONE' })  -- artifact
  highlight(0, 'nomadBlockVault',          { fg = colors.pink,       bg = 'NONE' })  -- vault


  -----------------------------------------------------------------------------
  -- tfvars File Specific

  highlight(0, 'tfvarsKey',                { fg = colors.blue,       bg = 'NONE' })  -- Variable names in .tfvars
  highlight(0, 'tfvarsValue',              { fg = colors.white,      bg = 'NONE' })  -- Values in .tfvars


  -----------------------------------------------------------------------------
  -- Terraform State (terraform.tfstate - JSON)

  highlight(0, 'tfstateVersion',           { fg = colors.greenLight, bg = 'NONE' })  -- version
  highlight(0, 'tfstateSerial',            { fg = colors.greenLight, bg = 'NONE' })  -- serial
  highlight(0, 'tfstateResourceType',      { link = "Type" })  -- Resource types
  highlight(0, 'tfstateResourceName',      { fg = colors.blue,       bg = 'NONE' })  -- Resource names
  highlight(0, 'tfstateAttributeKey',      { fg = colors.blue,       bg = 'NONE' })  -- Attribute keys


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'hclError',                 { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'terraError',               { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Terraform errors
  highlight(0, 'terraDeprecated',          { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- Deprecated
end

return hcl
