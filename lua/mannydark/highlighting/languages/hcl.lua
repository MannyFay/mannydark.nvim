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
  highlight(0, 'hclComment',               { fg = colors.red,        bg = 'NONE' })  -- # and // comments
  highlight(0, 'hclBlockComment',          { fg = colors.red,        bg = 'NONE' })  -- /* */ block comments
  highlight(0, 'hclTodo',                  { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX, BUG

  -- Block Types
  highlight(0, 'hclBlockType',             { fg = colors.pink,       bg = 'NONE' })  -- resource, variable, etc.
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
  highlight(0, 'hclValueString',           { fg = colors.redLight,   bg = 'NONE' })  -- "string"
  highlight(0, 'hclStringInterp',          { fg = colors.purple,     bg = 'NONE' })  -- ${...} interpolation
  highlight(0, 'hclTemplateInterp',        { fg = colors.purple,     bg = 'NONE' })  -- %{...} template directive
  highlight(0, 'hclHereDoc',               { fg = colors.redLight,   bg = 'NONE' })  -- <<EOF heredocs
  highlight(0, 'hclHereDocText',           { fg = colors.redLight,   bg = 'NONE' })  -- Heredoc content
  highlight(0, 'hclHereDocDelim',          { fg = colors.pink,       bg = 'NONE' })  -- EOF delimiter

  -- Escape Sequences
  highlight(0, 'hclEscape',                { fg = colors.pink,       bg = 'NONE' })  -- \n, \t, \\, etc.
  highlight(0, 'hclStringEscape',          { fg = colors.pink,       bg = 'NONE' })  -- Escape in strings

  -- Functions
  highlight(0, 'hclFunction',              { fg = colors.orange,     bg = 'NONE' })  -- Function calls

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
  highlight(0, 'hclConditional',           { fg = colors.pink,       bg = 'NONE' })  -- if, else
  highlight(0, 'hclForExpr',               { fg = colors.pink,       bg = 'NONE' })  -- for expression
  highlight(0, 'hclEndfor',                { fg = colors.pink,       bg = 'NONE' })  -- endfor
  highlight(0, 'hclEndif',                 { fg = colors.pink,       bg = 'NONE' })  -- endif


  -----------------------------------------------------------------------------
  -- Terraform-specific Vim Syntax Groups

  -- Block Types (Terraform)
  highlight(0, 'terraBlockType',           { fg = colors.pink,       bg = 'NONE' })  -- resource, data, module, etc.
  highlight(0, 'terraResourceType',        { fg = colors.turquoise,  bg = 'NONE' })  -- aws_instance, google_compute, etc.
  highlight(0, 'terraResourceName',        { fg = colors.turquoise,  bg = 'NONE' })  -- Resource name label
  highlight(0, 'terraDataSource',          { fg = colors.turquoise,  bg = 'NONE' })  -- Data source type

  -- Types
  highlight(0, 'terraType',                { fg = colors.turquoise,  bg = 'NONE' })  -- string, bool, number, etc.
  highlight(0, 'terraPrimitiveType',       { fg = colors.turquoise,  bg = 'NONE' })  -- string, bool, number
  highlight(0, 'terraCollectionType',      { fg = colors.turquoise,  bg = 'NONE' })  -- list, map, set
  highlight(0, 'terraStructuralType',      { fg = colors.turquoise,  bg = 'NONE' })  -- object, tuple
  highlight(0, 'terraAnyType',             { fg = colors.turquoise,  bg = 'NONE' })  -- any

  -- Built-in Variables
  highlight(0, 'terraBuiltinVar',          { fg = colors.purple,     bg = 'NONE' })  -- var, local, module, data
  highlight(0, 'terraVarRef',              { fg = colors.purple,     bg = 'NONE' })  -- var.name
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
  highlight(0, 'terraVarType',             { fg = colors.blue,       bg = 'NONE' })  -- type
  highlight(0, 'terraVarDescription',      { fg = colors.blue,       bg = 'NONE' })  -- description
  highlight(0, 'terraVarValidation',       { fg = colors.blue,       bg = 'NONE' })  -- validation
  highlight(0, 'terraVarSensitive',        { fg = colors.blue,       bg = 'NONE' })  -- sensitive
  highlight(0, 'terraVarNullable',         { fg = colors.blue,       bg = 'NONE' })  -- nullable
  highlight(0, 'terraVarEphemeral',        { fg = colors.blue,       bg = 'NONE' })  -- ephemeral (v1.10+)

  -- Output Block Attributes
  highlight(0, 'terraOutputAttr',          { fg = colors.blue,       bg = 'NONE' })  -- value, description, sensitive
  highlight(0, 'terraOutputValue',         { fg = colors.blue,       bg = 'NONE' })  -- value
  highlight(0, 'terraOutputSensitive',     { fg = colors.blue,       bg = 'NONE' })  -- sensitive

  -- Provisioners
  highlight(0, 'terraProvisioner',         { fg = colors.pink,       bg = 'NONE' })  -- provisioner block
  highlight(0, 'terraProvisionerType',     { fg = colors.turquoise,  bg = 'NONE' })  -- local-exec, remote-exec
  highlight(0, 'terraProvisionerAttr',     { fg = colors.blue,       bg = 'NONE' })  -- command, inline, script
  highlight(0, 'terraConnection',          { fg = colors.pink,       bg = 'NONE' })  -- connection block


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.hcl)

  -- Comments
  highlight(0, '@comment.hcl',             { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Keywords
  highlight(0, '@keyword.hcl',             { fg = colors.pink,       bg = 'NONE' })  -- Block identifiers
  highlight(0, '@keyword.repeat.hcl',      { fg = colors.pink,       bg = 'NONE' })  -- for, endfor, in
  highlight(0, '@keyword.conditional.hcl', { fg = colors.pink,       bg = 'NONE' })  -- if, else, endif

  -- Types
  highlight(0, '@type.hcl',                { fg = colors.turquoise,  bg = 'NONE' })  -- Nested block identifiers
  highlight(0, '@type.builtin.hcl',        { fg = colors.turquoise,  bg = 'NONE' })  -- Type keywords

  -- Variables
  highlight(0, '@variable.hcl',            { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, '@variable.builtin.hcl',    { fg = colors.purple,     bg = 'NONE' })  -- var, data, local, module
  highlight(0, '@variable.member.hcl',     { fg = colors.blue,       bg = 'NONE' })  -- Object attributes/keys

  -- Functions
  highlight(0, '@function.hcl',            { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.hcl',    { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Strings
  highlight(0, '@string.hcl',              { fg = colors.redLight,   bg = 'NONE' })  -- String literals
  highlight(0, '@string.escape.hcl',       { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences

  -- Numbers
  highlight(0, '@number.hcl',              { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.hcl',        { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.hcl',             { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Constants
  highlight(0, '@constant.hcl',            { fg = colors.blue,       bg = 'NONE' })  -- null and other constants
  highlight(0, '@constant.builtin.hcl',    { fg = colors.blue,       bg = 'NONE' })  -- null

  -- Operators
  highlight(0, '@operator.hcl',            { fg = colors.white,      bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.hcl', { fg = colors.white,      bg = 'NONE' })  -- {}, [], ()
  highlight(0, '@punctuation.delimiter.hcl', { fg = colors.white,    bg = 'NONE' })  -- , . [*] .*
  highlight(0, '@punctuation.special.hcl', { fg = colors.pink,       bg = 'NONE' })  -- ..., ?, =>

  -- None (assignment)
  highlight(0, '@none.hcl',                { fg = colors.white,      bg = 'NONE' })  -- : =


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.terraform)

  -- Comments
  highlight(0, '@comment.terraform',             { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Keywords
  highlight(0, '@keyword.terraform',             { fg = colors.pink,       bg = 'NONE' })  -- Block types
  highlight(0, '@keyword.repeat.terraform',      { fg = colors.pink,       bg = 'NONE' })  -- for, in
  highlight(0, '@keyword.conditional.terraform', { fg = colors.pink,       bg = 'NONE' })  -- if, else

  -- Types
  highlight(0, '@type.terraform',                { fg = colors.turquoise,  bg = 'NONE' })  -- Resource types, labels
  highlight(0, '@type.builtin.terraform',        { fg = colors.turquoise,  bg = 'NONE' })  -- bool, string, number, etc.

  -- Variables
  highlight(0, '@variable.terraform',            { fg = colors.white,      bg = 'NONE' })  -- Identifiers
  highlight(0, '@variable.builtin.terraform',    { fg = colors.purple,     bg = 'NONE' })  -- var, local, module, data, path, terraform
  highlight(0, '@variable.member.terraform',     { fg = colors.blue,       bg = 'NONE' })  -- Attributes

  -- Functions
  highlight(0, '@function.terraform',            { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.builtin.terraform',    { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions

  -- Strings
  highlight(0, '@string.terraform',              { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.terraform',       { fg = colors.pink,       bg = 'NONE' })  -- Escapes

  -- Numbers
  highlight(0, '@number.terraform',              { fg = colors.greenLight, bg = 'NONE' })  -- Numbers

  -- Booleans
  highlight(0, '@boolean.terraform',             { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Constants
  highlight(0, '@constant.builtin.terraform',    { fg = colors.blue,       bg = 'NONE' })  -- null

  -- Operators
  highlight(0, '@operator.terraform',            { fg = colors.white,      bg = 'NONE' })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.terraform', { fg = colors.white,      bg = 'NONE' })  -- {}, [], ()
  highlight(0, '@punctuation.delimiter.terraform', { fg = colors.white,    bg = 'NONE' })  -- , .
  highlight(0, '@punctuation.special.terraform', { fg = colors.pink,       bg = 'NONE' })  -- ..., ?, =>


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.hcl / @lsp.type.xxx.terraform)

  -- HCL LSP
  highlight(0, '@lsp.type.property.hcl',     { fg = colors.blue,       bg = 'NONE' })  -- Attributes
  highlight(0, '@lsp.type.type.hcl',         { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.variable.hcl',     { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.function.hcl',     { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.string.hcl',       { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.hcl',       { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.keyword.hcl',      { fg = colors.pink,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.comment.hcl',      { fg = colors.red,        bg = 'NONE' })  -- Comments

  -- Terraform LSP
  highlight(0, '@lsp.type.property.terraform',  { fg = colors.blue,       bg = 'NONE' })  -- Attributes
  highlight(0, '@lsp.type.type.terraform',      { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.variable.terraform',  { fg = colors.white,      bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.function.terraform',  { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.string.terraform',    { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.terraform',    { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.keyword.terraform',   { fg = colors.pink,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.comment.terraform',   { fg = colors.red,        bg = 'NONE' })  -- Comments


  -----------------------------------------------------------------------------
  -- Terraform Block Types

  -- Core Block Types
  highlight(0, 'terraBlockResource',       { fg = colors.pink,       bg = 'NONE' })  -- resource
  highlight(0, 'terraBlockData',           { fg = colors.pink,       bg = 'NONE' })  -- data
  highlight(0, 'terraBlockVariable',       { fg = colors.pink,       bg = 'NONE' })  -- variable
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
  highlight(0, 'terraFuncNumeric',         { fg = colors.orange,     bg = 'NONE' })  -- abs, ceil, floor, etc.
  highlight(0, 'terraFuncAbs',             { fg = colors.orange,     bg = 'NONE' })  -- abs
  highlight(0, 'terraFuncCeil',            { fg = colors.orange,     bg = 'NONE' })  -- ceil
  highlight(0, 'terraFuncFloor',           { fg = colors.orange,     bg = 'NONE' })  -- floor
  highlight(0, 'terraFuncLog',             { fg = colors.orange,     bg = 'NONE' })  -- log
  highlight(0, 'terraFuncMax',             { fg = colors.orange,     bg = 'NONE' })  -- max
  highlight(0, 'terraFuncMin',             { fg = colors.orange,     bg = 'NONE' })  -- min
  highlight(0, 'terraFuncParseint',        { fg = colors.orange,     bg = 'NONE' })  -- parseint
  highlight(0, 'terraFuncPow',             { fg = colors.orange,     bg = 'NONE' })  -- pow
  highlight(0, 'terraFuncSignum',          { fg = colors.orange,     bg = 'NONE' })  -- signum

  -- String Functions
  highlight(0, 'terraFuncString',          { fg = colors.orange,     bg = 'NONE' })  -- String functions
  highlight(0, 'terraFuncChomp',           { fg = colors.orange,     bg = 'NONE' })  -- chomp
  highlight(0, 'terraFuncFormat',          { fg = colors.orange,     bg = 'NONE' })  -- format
  highlight(0, 'terraFuncFormatlist',      { fg = colors.orange,     bg = 'NONE' })  -- formatlist
  highlight(0, 'terraFuncIndent',          { fg = colors.orange,     bg = 'NONE' })  -- indent
  highlight(0, 'terraFuncJoin',            { fg = colors.orange,     bg = 'NONE' })  -- join
  highlight(0, 'terraFuncLower',           { fg = colors.orange,     bg = 'NONE' })  -- lower
  highlight(0, 'terraFuncUpper',           { fg = colors.orange,     bg = 'NONE' })  -- upper
  highlight(0, 'terraFuncRegex',           { fg = colors.orange,     bg = 'NONE' })  -- regex
  highlight(0, 'terraFuncRegexall',        { fg = colors.orange,     bg = 'NONE' })  -- regexall
  highlight(0, 'terraFuncReplace',         { fg = colors.orange,     bg = 'NONE' })  -- replace
  highlight(0, 'terraFuncSplit',           { fg = colors.orange,     bg = 'NONE' })  -- split
  highlight(0, 'terraFuncStrrev',          { fg = colors.orange,     bg = 'NONE' })  -- strrev
  highlight(0, 'terraFuncSubstr',          { fg = colors.orange,     bg = 'NONE' })  -- substr
  highlight(0, 'terraFuncTitle',           { fg = colors.orange,     bg = 'NONE' })  -- title
  highlight(0, 'terraFuncTrim',            { fg = colors.orange,     bg = 'NONE' })  -- trim
  highlight(0, 'terraFuncTrimprefix',      { fg = colors.orange,     bg = 'NONE' })  -- trimprefix
  highlight(0, 'terraFuncTrimsuffix',      { fg = colors.orange,     bg = 'NONE' })  -- trimsuffix
  highlight(0, 'terraFuncTrimspace',       { fg = colors.orange,     bg = 'NONE' })  -- trimspace

  -- Collection Functions
  highlight(0, 'terraFuncCollection',      { fg = colors.orange,     bg = 'NONE' })  -- Collection functions
  highlight(0, 'terraFuncAlltrue',         { fg = colors.orange,     bg = 'NONE' })  -- alltrue
  highlight(0, 'terraFuncAnytrue',         { fg = colors.orange,     bg = 'NONE' })  -- anytrue
  highlight(0, 'terraFuncChunklist',       { fg = colors.orange,     bg = 'NONE' })  -- chunklist
  highlight(0, 'terraFuncCoalesce',        { fg = colors.orange,     bg = 'NONE' })  -- coalesce
  highlight(0, 'terraFuncCoalescelist',    { fg = colors.orange,     bg = 'NONE' })  -- coalescelist
  highlight(0, 'terraFuncCompact',         { fg = colors.orange,     bg = 'NONE' })  -- compact
  highlight(0, 'terraFuncConcat',          { fg = colors.orange,     bg = 'NONE' })  -- concat
  highlight(0, 'terraFuncContains',        { fg = colors.orange,     bg = 'NONE' })  -- contains
  highlight(0, 'terraFuncDistinct',        { fg = colors.orange,     bg = 'NONE' })  -- distinct
  highlight(0, 'terraFuncElement',         { fg = colors.orange,     bg = 'NONE' })  -- element
  highlight(0, 'terraFuncFlatten',         { fg = colors.orange,     bg = 'NONE' })  -- flatten
  highlight(0, 'terraFuncIndex',           { fg = colors.orange,     bg = 'NONE' })  -- index
  highlight(0, 'terraFuncKeys',            { fg = colors.orange,     bg = 'NONE' })  -- keys
  highlight(0, 'terraFuncLength',          { fg = colors.orange,     bg = 'NONE' })  -- length
  highlight(0, 'terraFuncLookup',          { fg = colors.orange,     bg = 'NONE' })  -- lookup
  highlight(0, 'terraFuncMatchkeys',       { fg = colors.orange,     bg = 'NONE' })  -- matchkeys
  highlight(0, 'terraFuncMerge',           { fg = colors.orange,     bg = 'NONE' })  -- merge
  highlight(0, 'terraFuncOne',             { fg = colors.orange,     bg = 'NONE' })  -- one
  highlight(0, 'terraFuncRange',           { fg = colors.orange,     bg = 'NONE' })  -- range
  highlight(0, 'terraFuncReverse',         { fg = colors.orange,     bg = 'NONE' })  -- reverse
  highlight(0, 'terraFuncSetintersection', { fg = colors.orange,     bg = 'NONE' })  -- setintersection
  highlight(0, 'terraFuncSetproduct',      { fg = colors.orange,     bg = 'NONE' })  -- setproduct
  highlight(0, 'terraFuncSetsubtract',     { fg = colors.orange,     bg = 'NONE' })  -- setsubtract
  highlight(0, 'terraFuncSetunion',        { fg = colors.orange,     bg = 'NONE' })  -- setunion
  highlight(0, 'terraFuncSlice',           { fg = colors.orange,     bg = 'NONE' })  -- slice
  highlight(0, 'terraFuncSort',            { fg = colors.orange,     bg = 'NONE' })  -- sort
  highlight(0, 'terraFuncSum',             { fg = colors.orange,     bg = 'NONE' })  -- sum
  highlight(0, 'terraFuncTranspose',       { fg = colors.orange,     bg = 'NONE' })  -- transpose
  highlight(0, 'terraFuncValues',          { fg = colors.orange,     bg = 'NONE' })  -- values
  highlight(0, 'terraFuncZipmap',          { fg = colors.orange,     bg = 'NONE' })  -- zipmap

  -- Encoding Functions
  highlight(0, 'terraFuncEncoding',        { fg = colors.orange,     bg = 'NONE' })  -- Encoding functions
  highlight(0, 'terraFuncBase64encode',    { fg = colors.orange,     bg = 'NONE' })  -- base64encode
  highlight(0, 'terraFuncBase64decode',    { fg = colors.orange,     bg = 'NONE' })  -- base64decode
  highlight(0, 'terraFuncBase64gzip',      { fg = colors.orange,     bg = 'NONE' })  -- base64gzip
  highlight(0, 'terraFuncCsvdecode',       { fg = colors.orange,     bg = 'NONE' })  -- csvdecode
  highlight(0, 'terraFuncJsonencode',      { fg = colors.orange,     bg = 'NONE' })  -- jsonencode
  highlight(0, 'terraFuncJsondecode',      { fg = colors.orange,     bg = 'NONE' })  -- jsondecode
  highlight(0, 'terraFuncUrlencode',       { fg = colors.orange,     bg = 'NONE' })  -- urlencode
  highlight(0, 'terraFuncYamlencode',      { fg = colors.orange,     bg = 'NONE' })  -- yamlencode
  highlight(0, 'terraFuncYamldecode',      { fg = colors.orange,     bg = 'NONE' })  -- yamldecode
  highlight(0, 'terraFuncTextencodebase64',{ fg = colors.orange,     bg = 'NONE' })  -- textencodebase64
  highlight(0, 'terraFuncTextdecodebase64',{ fg = colors.orange,     bg = 'NONE' })  -- textdecodebase64

  -- Filesystem Functions
  highlight(0, 'terraFuncFilesystem',      { fg = colors.orange,     bg = 'NONE' })  -- Filesystem functions
  highlight(0, 'terraFuncFile',            { fg = colors.orange,     bg = 'NONE' })  -- file
  highlight(0, 'terraFuncFileexists',      { fg = colors.orange,     bg = 'NONE' })  -- fileexists
  highlight(0, 'terraFuncFileset',         { fg = colors.orange,     bg = 'NONE' })  -- fileset
  highlight(0, 'terraFuncFilebase64',      { fg = colors.orange,     bg = 'NONE' })  -- filebase64
  highlight(0, 'terraFuncTemplatefile',    { fg = colors.orange,     bg = 'NONE' })  -- templatefile
  highlight(0, 'terraFuncPathexpand',      { fg = colors.orange,     bg = 'NONE' })  -- pathexpand
  highlight(0, 'terraFuncDirname',         { fg = colors.orange,     bg = 'NONE' })  -- dirname
  highlight(0, 'terraFuncBasename',        { fg = colors.orange,     bg = 'NONE' })  -- basename
  highlight(0, 'terraFuncAbspath',         { fg = colors.orange,     bg = 'NONE' })  -- abspath

  -- Date/Time Functions
  highlight(0, 'terraFuncDatetime',        { fg = colors.orange,     bg = 'NONE' })  -- Date/time functions
  highlight(0, 'terraFuncFormatdate',      { fg = colors.orange,     bg = 'NONE' })  -- formatdate
  highlight(0, 'terraFuncTimeadd',         { fg = colors.orange,     bg = 'NONE' })  -- timeadd
  highlight(0, 'terraFuncTimecmp',         { fg = colors.orange,     bg = 'NONE' })  -- timecmp
  highlight(0, 'terraFuncTimestamp',       { fg = colors.orange,     bg = 'NONE' })  -- timestamp
  highlight(0, 'terraFuncPlantimestamp',   { fg = colors.orange,     bg = 'NONE' })  -- plantimestamp

  -- Hash/Crypto Functions
  highlight(0, 'terraFuncHash',            { fg = colors.orange,     bg = 'NONE' })  -- Hash functions
  highlight(0, 'terraFuncMd5',             { fg = colors.orange,     bg = 'NONE' })  -- md5
  highlight(0, 'terraFuncSha1',            { fg = colors.orange,     bg = 'NONE' })  -- sha1
  highlight(0, 'terraFuncSha256',          { fg = colors.orange,     bg = 'NONE' })  -- sha256
  highlight(0, 'terraFuncSha512',          { fg = colors.orange,     bg = 'NONE' })  -- sha512
  highlight(0, 'terraFuncBcrypt',          { fg = colors.orange,     bg = 'NONE' })  -- bcrypt
  highlight(0, 'terraFuncRsadecrypt',      { fg = colors.orange,     bg = 'NONE' })  -- rsadecrypt
  highlight(0, 'terraFuncUuid',            { fg = colors.orange,     bg = 'NONE' })  -- uuid
  highlight(0, 'terraFuncUuidv5',          { fg = colors.orange,     bg = 'NONE' })  -- uuidv5

  -- IP Network Functions
  highlight(0, 'terraFuncNetwork',         { fg = colors.orange,     bg = 'NONE' })  -- Network functions
  highlight(0, 'terraFuncCidrhost',        { fg = colors.orange,     bg = 'NONE' })  -- cidrhost
  highlight(0, 'terraFuncCidrnetmask',     { fg = colors.orange,     bg = 'NONE' })  -- cidrnetmask
  highlight(0, 'terraFuncCidrsubnet',      { fg = colors.orange,     bg = 'NONE' })  -- cidrsubnet
  highlight(0, 'terraFuncCidrsubnets',     { fg = colors.orange,     bg = 'NONE' })  -- cidrsubnets

  -- Type Conversion Functions
  highlight(0, 'terraFuncTypeConv',        { fg = colors.orange,     bg = 'NONE' })  -- Type conversion functions
  highlight(0, 'terraFuncCan',             { fg = colors.orange,     bg = 'NONE' })  -- can
  highlight(0, 'terraFuncNonsensitive',    { fg = colors.orange,     bg = 'NONE' })  -- nonsensitive
  highlight(0, 'terraFuncSensitive',       { fg = colors.orange,     bg = 'NONE' })  -- sensitive
  highlight(0, 'terraFuncTobool',          { fg = colors.orange,     bg = 'NONE' })  -- tobool
  highlight(0, 'terraFuncTolist',          { fg = colors.orange,     bg = 'NONE' })  -- tolist
  highlight(0, 'terraFuncTomap',           { fg = colors.orange,     bg = 'NONE' })  -- tomap
  highlight(0, 'terraFuncTonumber',        { fg = colors.orange,     bg = 'NONE' })  -- tonumber
  highlight(0, 'terraFuncToset',           { fg = colors.orange,     bg = 'NONE' })  -- toset
  highlight(0, 'terraFuncTostring',        { fg = colors.orange,     bg = 'NONE' })  -- tostring
  highlight(0, 'terraFuncTry',             { fg = colors.orange,     bg = 'NONE' })  -- try
  highlight(0, 'terraFuncType',            { fg = colors.orange,     bg = 'NONE' })  -- type


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
  highlight(0, 'packerBlockVariable',      { fg = colors.pink,       bg = 'NONE' })  -- variable
  highlight(0, 'packerSourceType',         { fg = colors.turquoise,  bg = 'NONE' })  -- amazon-ebs, docker, etc.

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
  highlight(0, 'tfstateResourceType',      { fg = colors.turquoise,  bg = 'NONE' })  -- Resource types
  highlight(0, 'tfstateResourceName',      { fg = colors.blue,       bg = 'NONE' })  -- Resource names
  highlight(0, 'tfstateAttributeKey',      { fg = colors.blue,       bg = 'NONE' })  -- Attribute keys


  -----------------------------------------------------------------------------
  -- Error Highlighting

  highlight(0, 'hclError',                 { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Syntax errors
  highlight(0, 'terraError',               { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Terraform errors
  highlight(0, 'terraDeprecated',          { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- Deprecated
end

return hcl
