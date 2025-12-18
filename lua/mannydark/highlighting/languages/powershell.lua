-------------------------------------------------------------------------------
-- PowerShell Files
-- Highlighting for .ps1, .psm1, .psd1 files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local powershell = {}


-------------------------------------------------------------------------------
-- Settings

powershell.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords - Control Flow
  highlight(0, 'ps1Conditional',       { fg = colors.blue,       bg = 'NONE'            })  -- if, else, elseif, switch, default
  highlight(0, 'ps1Repeat',            { fg = colors.blue,       bg = 'NONE'            })  -- while, for, do, until, break, continue, foreach, in

  -- Keywords - Exception Handling
  highlight(0, 'ps1Exception',         { fg = colors.blue,       bg = 'NONE'            })  -- begin, process, end, exit, inlinescript, parallel, sequence

  -- Keywords - General
  highlight(0, 'ps1Keyword',           { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, finally, throw, return, filter, trap, param, data, dynamicparam, class, define, from, using, var, function, workflow, configuration
  highlight(0, 'ps1Statement',         { fg = colors.blue,       bg = 'NONE'            })  -- General statements

  -- Keywords - Function/Script Blocks
  highlight(0, 'ps1FunctionKeyword',   { fg = colors.blue,       bg = 'NONE'            })  -- function, filter, workflow
  highlight(0, 'ps1Param',             { fg = colors.blue,       bg = 'NONE'            })  -- param, dynamicparam
  highlight(0, 'ps1ScriptBlock',       { fg = colors.blue,       bg = 'NONE'            })  -- begin, process, end

  -- Keywords - Class/Type
  highlight(0, 'ps1Class',             { fg = colors.blue,       bg = 'NONE'            })  -- class
  highlight(0, 'ps1Enum',              { fg = colors.blue,       bg = 'NONE'            })  -- enum

  -- Cmdlets (Verb-Noun pattern)
  highlight(0, 'ps1Cmdlet',            { fg = colors.orange,     bg = 'NONE'            })  -- Get-*, Set-*, New-*, Remove-*, Add-*, etc.
  highlight(0, 'ps1Function',          { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'ps1FunctionName',      { fg = colors.orange,     bg = 'NONE'            })  -- Function declarations
  highlight(0, 'ps1FunctionInvocation',{ fg = colors.orange,     bg = 'NONE'            })  -- Function calls

  -- Common Cmdlet Verbs
  highlight(0, 'ps1CmdletVerb',        { fg = colors.orange,     bg = 'NONE'            })  -- Get, Set, New, Remove, Add, Clear, Copy, Move, Invoke, Start, Stop, Test, Enable, Disable, Install, Uninstall, Import, Export, Write, Read, Out, Format, etc.

  -- Types
  highlight(0, 'ps1Type',              { fg = colors.turquoise,  bg = 'NONE'            })  -- [typename]
  highlight(0, 'ps1TypeName',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names in brackets
  highlight(0, 'ps1TypeAccelerator',   { fg = colors.turquoise,  bg = 'NONE'            })  -- [int], [string], [bool], [array], [hashtable], etc.

  -- Variables
  highlight(0, 'ps1Variable',          { fg = colors.purple,     bg = 'NONE'            })  -- $variable
  highlight(0, 'ps1VariableSimple',    { fg = colors.purple,     bg = 'NONE'            })  -- $var
  highlight(0, 'ps1VariableBraced',    { fg = colors.purple,     bg = 'NONE'            })  -- ${variable}

  -- Variable Scope Modifiers
  highlight(0, 'ps1ScopeModifier',     { fg = colors.pink,       bg = 'NONE'            })  -- global:, local:, private:, script:, using:, env:

  -- Automatic Variables
  highlight(0, 'ps1Constant',          { fg = colors.pink,       bg = 'NONE'            })  -- $true, $false, $null, $?, $_, $$, $^
  highlight(0, 'ps1BuiltIn',           { fg = colors.pink,       bg = 'NONE'            })  -- $args, $error, $host, $input, $PSVersionTable
  highlight(0, 'ps1AutomaticVar',      { fg = colors.pink,       bg = 'NONE'            })  -- $PSCmdlet, $PSScriptRoot, $PSCommandPath, $MyInvocation, $PSBoundParameters, $PSItem, $Matches, $LASTEXITCODE, $PID, $HOME, $PWD, $PROFILE, etc.

  -- Boolean
  highlight(0, 'ps1Boolean',           { fg = colors.blue,       bg = 'NONE'            })  -- $true, $false

  -- Strings
  highlight(0, 'ps1String',            { fg = colors.redLight,   bg = 'NONE'            })  -- "string"
  highlight(0, 'ps1StringDouble',      { fg = colors.redLight,   bg = 'NONE'            })  -- "double quoted"
  highlight(0, 'ps1StringSingle',      { fg = colors.redLight,   bg = 'NONE'            })  -- 'single quoted'

  -- Here-Strings
  highlight(0, 'ps1HereString',        { fg = colors.redLight,   bg = 'NONE'            })  -- @"..."@ and @'...'@
  highlight(0, 'ps1HereStringDouble',  { fg = colors.redLight,   bg = 'NONE'            })  -- @"..."@
  highlight(0, 'ps1HereStringSingle',  { fg = colors.redLight,   bg = 'NONE'            })  -- @'...'@

  -- String Interpolation
  highlight(0, 'ps1Interpolation',     { fg = colors.purple,     bg = 'NONE'            })  -- $(expression) in strings
  highlight(0, 'ps1InterpolationVar',  { fg = colors.purple,     bg = 'NONE'            })  -- $variable in strings

  -- Escape Sequences
  highlight(0, 'ps1Escape',            { fg = colors.pink,       bg = 'NONE'            })  -- `n, `t, `r, ``, etc.

  -- Numbers
  highlight(0, 'ps1Number',            { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'ps1Integer',           { fg = colors.greenLight, bg = 'NONE'            })  -- Integers
  highlight(0, 'ps1Float',             { fg = colors.greenLight, bg = 'NONE'            })  -- Floats
  highlight(0, 'ps1Hex',               { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex
  highlight(0, 'ps1Scientific',        { fg = colors.greenLight, bg = 'NONE'            })  -- Scientific notation
  highlight(0, 'ps1SizeSuffix',        { fg = colors.greenLight, bg = 'NONE'            })  -- KB, MB, GB, TB, PB

  -- Operators - Comparison
  highlight(0, 'ps1Operator',          { fg = colors.white,      bg = 'NONE'            })  -- General operators
  highlight(0, 'ps1ComparisonOp',      { fg = colors.blue,       bg = 'NONE'            })  -- -eq, -ne, -gt, -ge, -lt, -le, -like, -notlike, -match, -notmatch, -contains, -notcontains, -in, -notin, -replace
  highlight(0, 'ps1CaseSensitiveOp',   { fg = colors.blue,       bg = 'NONE'            })  -- -ceq, -cne, -cgt, -cge, -clt, -cle, -clike, -cmatch, etc.
  highlight(0, 'ps1CaseInsensitiveOp', { fg = colors.blue,       bg = 'NONE'            })  -- -ieq, -ine, -igt, -ige, -ilt, -ile, -ilike, -imatch, etc.

  -- Operators - Logical
  highlight(0, 'ps1LogicalOp',         { fg = colors.blue,       bg = 'NONE'            })  -- -and, -or, -xor, -not, !

  -- Operators - Bitwise
  highlight(0, 'ps1BitwiseOp',         { fg = colors.blue,       bg = 'NONE'            })  -- -band, -bor, -bxor, -bnot, -shl, -shr

  -- Operators - Type
  highlight(0, 'ps1TypeOp',            { fg = colors.blue,       bg = 'NONE'            })  -- -is, -isnot, -as

  -- Operators - String
  highlight(0, 'ps1StringOp',          { fg = colors.blue,       bg = 'NONE'            })  -- -split, -join, -f (format)

  -- Operators - Arithmetic
  highlight(0, 'ps1ArithmeticOp',      { fg = colors.white,      bg = 'NONE'            })  -- +, -, *, /, %

  -- Operators - Assignment
  highlight(0, 'ps1AssignmentOp',      { fg = colors.white,      bg = 'NONE'            })  -- =, +=, -=, *=, /=, %=, ++, --

  -- Operators - Redirection
  highlight(0, 'ps1RedirectionOp',     { fg = colors.pink,       bg = 'NONE'            })  -- >, >>, 2>, 2>>, *>, *>>

  -- Operators - Special
  highlight(0, 'ps1PipelineOp',        { fg = colors.white,      bg = 'NONE'            })  -- |
  highlight(0, 'ps1RangeOp',           { fg = colors.white,      bg = 'NONE'            })  -- ..
  highlight(0, 'ps1CallOp',            { fg = colors.white,      bg = 'NONE'            })  -- &
  highlight(0, 'ps1DotSourceOp',       { fg = colors.white,      bg = 'NONE'            })  -- . (dot sourcing)
  highlight(0, 'ps1StaticMemberOp',    { fg = colors.white,      bg = 'NONE'            })  -- ::
  highlight(0, 'ps1MemberOp',          { fg = colors.white,      bg = 'NONE'            })  -- . (member access)
  highlight(0, 'ps1SubexpressionOp',   { fg = colors.pink,       bg = 'NONE'            })  -- $()
  highlight(0, 'ps1ArraySubexprOp',    { fg = colors.pink,       bg = 'NONE'            })  -- @()
  highlight(0, 'ps1HashLiteralOp',     { fg = colors.pink,       bg = 'NONE'            })  -- @{}
  highlight(0, 'ps1SplatOp',           { fg = colors.pink,       bg = 'NONE'            })  -- @variable (splatting)

  -- Parameters
  highlight(0, 'ps1Parameter',         { fg = colors.blue,       bg = 'NONE'            })  -- -ParameterName
  highlight(0, 'ps1CommonParameter',   { fg = colors.blue,       bg = 'NONE'            })  -- -Verbose, -Debug, -ErrorAction, -WarningAction, etc.

  -- Attributes
  highlight(0, 'ps1Attribute',         { fg = colors.pink,       bg = 'NONE'            })  -- [Parameter()], [CmdletBinding()], [ValidateSet()], etc.
  highlight(0, 'ps1AttributeName',     { fg = colors.pink,       bg = 'NONE'            })  -- Attribute name

  -- Arrays/Hashtables
  highlight(0, 'ps1Array',             { fg = colors.white,      bg = 'NONE'            })  -- @()
  highlight(0, 'ps1Hashtable',         { fg = colors.white,      bg = 'NONE'            })  -- @{}
  highlight(0, 'ps1HashtableKey',      { fg = colors.purple,     bg = 'NONE'            })  -- Key in hashtable

  -- Brackets
  highlight(0, 'ps1Braces',            { fg = colors.white,      bg = 'NONE'            })  -- { }
  highlight(0, 'ps1Brackets',          { fg = colors.white,      bg = 'NONE'            })  -- [ ]
  highlight(0, 'ps1Parens',            { fg = colors.white,      bg = 'NONE'            })  -- ( )

  -- Punctuation
  highlight(0, 'ps1Semicolon',         { fg = colors.white,      bg = 'NONE'            })  -- ;
  highlight(0, 'ps1Comma',             { fg = colors.white,      bg = 'NONE'            })  -- ,

  -- Label
  highlight(0, 'ps1Label',             { fg = colors.blue,       bg = 'NONE'            })  -- :label

  -- Comments
  highlight(0, 'ps1Comment',           { fg = colors.red,        bg = 'NONE'            })  -- # comments
  highlight(0, 'ps1BlockComment',      { fg = colors.red,        bg = 'NONE'            })  -- <# ... #>
  highlight(0, 'ps1CommentDoc',        { fg = colors.red,        bg = 'NONE'            })  -- .SYNOPSIS, .DESCRIPTION, .PARAMETER, .EXAMPLE, etc.
  highlight(0, 'ps1CommentDocKeyword', { fg = colors.green,      bg = 'NONE'            })  -- Doc keywords
  highlight(0, 'ps1Todo',              { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Requires
  highlight(0, 'ps1Requires',          { fg = colors.pink,       bg = 'NONE'            })  -- #Requires

  -- Region
  highlight(0, 'ps1Region',            { fg = colors.red,        bg = 'NONE'            })  -- #region, #endregion

  -- Error
  highlight(0, 'ps1Error',             { fg = colors.red,        bg = 'NONE'            })  -- Errors


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.powershell)

  -- Variables
  highlight(0, '@variable.powershell',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.builtin.powershell',      { fg = colors.pink,      bg = 'NONE' })  -- $_, $PSItem, $true, $false, $null, $args, $error, etc.
  highlight(0, '@variable.parameter.powershell',    { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@variable.parameter.builtin.powershell', { fg = colors.blue, bg = 'NONE' })  -- Command parameters (-Name, -Path, etc.)
  highlight(0, '@variable.member.powershell',       { fg = colors.purple,    bg = 'NONE' })  -- Member variables

  -- Constants
  highlight(0, '@constant.powershell',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.powershell',      { fg = colors.pink,      bg = 'NONE' })  -- $true, $false, $null

  -- Functions
  highlight(0, '@function.powershell',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.powershell',         { fg = colors.orange,    bg = 'NONE' })  -- Function/cmdlet calls
  highlight(0, '@function.method.powershell',       { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@constructor.powershell',           { fg = colors.orange,    bg = 'NONE' })  -- Constructors

  -- Types
  highlight(0, '@type.powershell',                  { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@type.builtin.powershell',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types [int], [string], etc.

  -- Properties
  highlight(0, '@property.powershell',              { fg = colors.purple,    bg = 'NONE' })  -- Properties

  -- Keywords
  highlight(0, '@keyword.powershell',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.function.powershell',      { fg = colors.blue,      bg = 'NONE' })  -- function, filter, workflow
  highlight(0, '@keyword.type.powershell',          { fg = colors.blue,      bg = 'NONE' })  -- class, enum
  highlight(0, '@keyword.operator.powershell',      { fg = colors.blue,      bg = 'NONE' })  -- -and, -or, -not, -eq, -ne, etc.
  highlight(0, '@keyword.return.powershell',        { fg = colors.blue,      bg = 'NONE' })  -- return, exit
  highlight(0, '@keyword.repeat.powershell',        { fg = colors.blue,      bg = 'NONE' })  -- for, foreach, while, do, until
  highlight(0, '@keyword.conditional.powershell',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, elseif, switch
  highlight(0, '@keyword.exception.powershell',     { fg = colors.blue,      bg = 'NONE' })  -- try, catch, finally, throw, trap
  highlight(0, '@keyword.coroutine.powershell',     { fg = colors.blue,      bg = 'NONE' })  -- parallel, sequence

  -- Attributes
  highlight(0, '@attribute.powershell',             { fg = colors.pink,      bg = 'NONE' })  -- [CmdletBinding()], [Parameter()], etc.

  -- Strings
  highlight(0, '@string.powershell',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.powershell',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.regexp.powershell',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex patterns
  highlight(0, '@string.special.powershell',        { fg = colors.pink,      bg = 'NONE' })  -- Special strings

  -- Numbers
  highlight(0, '@number.powershell',                { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@number.float.powershell',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.powershell',               { fg = colors.blue,      bg = 'NONE' })  -- $true, $false

  -- Comments
  highlight(0, '@comment.powershell',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.powershell', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Operators and Punctuation
  highlight(0, '@operator.powershell',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.powershell',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}
  highlight(0, '@punctuation.delimiter.powershell', { fg = colors.white,     bg = 'NONE' })  -- . :: , ;
  highlight(0, '@punctuation.special.powershell',   { fg = colors.pink,      bg = 'NONE' })  -- $ @ in variables, @() @{}


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.powershell)

  highlight(0, '@lsp.type.variable.powershell',      { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@lsp.type.parameter.powershell',     { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.powershell',      { fg = colors.purple,    bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.function.powershell',      { fg = colors.orange,    bg = 'NONE' })  -- Functions/Cmdlets
  highlight(0, '@lsp.type.method.powershell',        { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.type.powershell',          { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.class.powershell',         { fg = colors.turquoise, bg = 'NONE' })  -- Classes
  highlight(0, '@lsp.type.enum.powershell',          { fg = colors.turquoise, bg = 'NONE' })  -- Enums
  highlight(0, '@lsp.type.enumMember.powershell',    { fg = colors.purple,    bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.keyword.powershell',       { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.powershell',      { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.powershell',        { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.powershell',        { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.powershell',       { fg = colors.red,       bg = 'NONE' })  -- Comments

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.readonly.powershell',    { fg = colors.pink,      bg = 'NONE' })  -- Automatic variables
  highlight(0, '@lsp.typemod.function.declaration.powershell', { fg = colors.orange,    bg = 'NONE' })  -- Function declarations
  highlight(0, '@lsp.typemod.function.defaultLibrary.powershell', { fg = colors.orange, bg = 'NONE' })  -- Built-in cmdlets
  highlight(0, '@lsp.typemod.type.defaultLibrary.powershell',  { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types
end

return powershell

