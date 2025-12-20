-------------------------------------------------------------------------------
-- PHP
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local php       = {}


-------------------------------------------------------------------------------
-- Settings

php.setupHighlighting = function()


  -------------------------------------------------------------------------
  -- Vim PHP Syntax Groups
  -------------------------------------------------------------------------

  -- PHP Region Tags
  highlight(0, 'phpRegion',                   { fg = colors.white,      bg = 'NONE'            })  -- <?php ... ?>
  highlight(0, 'phpRegionAsp',                { fg = colors.white,      bg = 'NONE'            })  -- <% ... %>
  highlight(0, 'phpRegionSc',                 { fg = colors.white,      bg = 'NONE'            })  -- <script language="php">

  -- Keywords
  highlight(0, 'phpKeyword',                  { link = "Keyword" })  -- var, const
  highlight(0, 'phpConditional',              { link = "Conditional" })  -- if, else, elseif, switch, match
  highlight(0, 'phpRepeat',                   { fg = colors.blue,       bg = 'NONE'            })  -- for, foreach, while, do
  highlight(0, 'phpStatement',                { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, yield, yield from
  highlight(0, 'phpLabel',                    { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'phpStructure',                { fg = colors.blue,       bg = 'NONE'            })  -- class, interface, trait, enum, namespace, extends, implements
  highlight(0, 'phpException',                { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, throw, finally
  highlight(0, 'phpDefine',                   { fg = colors.blue,       bg = 'NONE'            })  -- function, fn, echo, print, new, clone
  highlight(0, 'phpInclude',                  { fg = colors.blue,       bg = 'NONE'            })  -- include, include_once, require, require_once, use

  -- Storage Class and Modifiers
  highlight(0, 'phpStorageClass',             { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected, static, final, abstract, readonly
  highlight(0, 'phpSCKeyword',                { link = "Keyword" })  -- Storage class keywords
  highlight(0, 'phpFCKeyword',                { link = "Keyword" })  -- Function class keywords

  -- Types
  highlight(0, 'phpType',                     { link = "Type" })  -- int, float, string, bool, array, object, mixed, void, never, null, callable, iterable
  highlight(0, 'phpNullValue',                { fg = colors.turquoise,  bg = 'NONE'            })  -- null
  highlight(0, 'phpBoolean',                  { link = "Boolean" })  -- true, false

  -- Classes and Interfaces
  highlight(0, 'phpClass',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'phpClasses',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in class names
  highlight(0, 'phpClassName',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Class name in definition
  highlight(0, 'phpClassExtends',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Class name after extends
  highlight(0, 'phpClassImplements',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names after implements
  highlight(0, 'phpStaticClasses',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Static class references
  highlight(0, 'phpInterfaces',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in interface names
  highlight(0, 'phpInterfaceName',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Interface name in definition
  highlight(0, 'phpTrait',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Trait names
  highlight(0, 'phpEnum',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum names
  highlight(0, 'phpClassDelimiter',           { link = "Delimiter" })  -- Class delimiters

  -- Functions and Methods
  highlight(0, 'phpFunction',                 { link = "Function" })  -- Function names
  highlight(0, 'phpFunctions',                { link = "Function" })  -- Built-in function names
  highlight(0, 'phpMethod',                   { link = "Function" })  -- Method names
  highlight(0, 'phpMethods',                  { link = "Function" })  -- Object method calls
  highlight(0, 'phpMethodsVar',               { link = "Function" })  -- Variable method calls
  highlight(0, 'phpSpecialFunction',          { link = "Function" })  -- Magic methods, isset(), eval(), etc.
  highlight(0, 'phpMagicMethods',             { link = "Function" })  -- __construct, __destruct, __call, etc.

  -- Variables
  highlight(0, 'phpIdentifier',               { fg = colors.purple,     bg = 'NONE'            })  -- Variable names ($var)
  highlight(0, 'phpIdentifierSimply',         { fg = colors.purple,     bg = 'NONE'            })  -- Variables in ${...} syntax
  highlight(0, 'phpIdentifierComplex',        { fg = colors.purple,     bg = 'NONE'            })  -- Complex variable expressions
  highlight(0, 'phpVarSelector',              { link = "Variable" })  -- $ dollar sign
  highlight(0, 'phpVariable',                 { link = "Variable" })  -- Variable names

  -- Superglobals and Special Variables
  highlight(0, 'phpSuperglobals',             { fg = colors.pink,       bg = 'NONE'            })  -- $_GET, $_POST, $_SESSION, etc.
  highlight(0, 'phpIntVar',                   { link = "Variable" })  -- Internal superglobals
  highlight(0, 'phpEnvVar',                   { link = "Variable" })  -- Environment variables
  highlight(0, 'phpServerVars',               { link = "Variable" })  -- $_SERVER variables
  highlight(0, 'phpGlobals',                  { fg = colors.pink,       bg = 'NONE'            })  -- $GLOBALS

  -- Constants
  highlight(0, 'phpConstants',                { link = "Constant" })  -- User-defined constants
  highlight(0, 'phpConstant',                 { link = "Constant" })  -- __LINE__, __FILE__, __FUNCTION__, etc.
  highlight(0, 'phpCoreConstant',             { link = "Constant" })  -- PHP_VERSION, E_ERROR, etc.
  highlight(0, 'phpMagicConstants',           { link = "Constant" })  -- __CLASS__, __METHOD__, __DIR__, etc.

  -- Operators
  highlight(0, 'phpOperator',                 { link = "Operator" })  -- Standard operators
  highlight(0, 'phpRelation',                 { fg = colors.white,      bg = 'NONE'            })  -- Comparison operators
  highlight(0, 'phpComparison',               { fg = colors.white,      bg = 'NONE'            })  -- Comparison operators (alt)
  highlight(0, 'phpAssignByRef',              { fg = colors.white,      bg = 'NONE'            })  -- Reference assignment =&
  highlight(0, 'phpSplatOperator',            { link = "Operator" })  -- ... (spread operator)
  highlight(0, 'phpNullsafeOperator',         { link = "Operator" })  -- ?-> (nullsafe operator)
  highlight(0, 'phpArrow',                    { fg = colors.blue,       bg = 'NONE'            })  -- => (array/fn arrow)
  highlight(0, 'phpDoubleArrow',              { fg = colors.blue,       bg = 'NONE'            })  -- =>
  highlight(0, 'phpInstanceof',               { fg = colors.blue,       bg = 'NONE'            })  -- instanceof

  -- Member Access
  highlight(0, 'phpMemberSelector',           { fg = colors.white,      bg = 'NONE'            })  -- -> and ?->
  highlight(0, 'phpStaticAccessor',           { fg = colors.white,      bg = 'NONE'            })  -- ::
  highlight(0, 'phpSelf',                     { fg = colors.pink,       bg = 'NONE'            })  -- self
  highlight(0, 'phpParent',                   { fg = colors.white,      bg = 'NONE'            })  -- parent (also brackets/parens)
  highlight(0, 'phpStatic',                   { fg = colors.pink,       bg = 'NONE'            })  -- static (late static binding)

  -- Numbers
  highlight(0, 'phpNumber',                   { link = "Number" })  -- Integer literals
  highlight(0, 'phpFloat',                    { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'phpHexNumber',                { link = "Number" })  -- Hexadecimal: 0x1A
  highlight(0, 'phpOctalNumber',              { link = "Number" })  -- Octal: 0o17
  highlight(0, 'phpBinaryNumber',             { link = "Number" })  -- Binary: 0b1010

  -- Strings
  highlight(0, 'phpStringSingle',             { link = "String" })  -- Single-quoted strings
  highlight(0, 'phpStringDouble',             { link = "String" })  -- Double-quoted strings
  highlight(0, 'phpString',                   { link = "String" })  -- Generic string
  highlight(0, 'phpStringDelimiter',          { link = "Delimiter" })  -- String delimiters
  highlight(0, 'phpHereDoc',                  { fg = colors.redLight,   bg = 'NONE'            })  -- Heredoc syntax
  highlight(0, 'phpNowDoc',                   { fg = colors.redLight,   bg = 'NONE'            })  -- Nowdoc syntax
  highlight(0, 'phpBacktick',                 { fg = colors.orange,     bg = 'NONE'            })  -- Backtick command execution

  -- String Escapes and Interpolation
  highlight(0, 'phpStrEsc',                   { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, 'phpSpecialChar',              { fg = colors.pink,       bg = 'NONE'            })  -- Special characters
  highlight(0, 'phpBackslashSequences',       { fg = colors.pink,       bg = 'NONE'            })  -- Backslash sequences
  highlight(0, 'phpInterpSimple',             { fg = colors.purple,     bg = 'NONE'            })  -- Simple interpolation
  highlight(0, 'phpInterpDollarCurly1',       { fg = colors.purple,     bg = 'NONE'            })  -- ${var} interpolation
  highlight(0, 'phpInterpDollarCurly2',       { fg = colors.purple,     bg = 'NONE'            })  -- {$var} interpolation

  -- Namespaces
  highlight(0, 'phpNamespace',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace names
  highlight(0, 'phpNamespaceSeparator',       { fg = colors.white,      bg = 'NONE'            })  -- \ separator
  highlight(0, 'phpUseNamespaceSeparator',    { fg = colors.white,      bg = 'NONE'            })  -- \ in use statements
  highlight(0, 'phpClassNamespaceSeparator',  { fg = colors.white,      bg = 'NONE'            })  -- \ in class references

  -- Use Statements
  highlight(0, 'phpUseKeyword',               { link = "Keyword" })  -- use keyword
  highlight(0, 'phpUseClass',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Class in use statement
  highlight(0, 'phpUseFunction',              { link = "Function" })  -- Function in use statement
  highlight(0, 'phpUseAlias',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Alias in use statement
  highlight(0, 'phpUseConst',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Constant in use statement

  -- Attributes (PHP 8+)
  highlight(0, 'phpAttribute',                { fg = colors.pink,       bg = 'NONE'            })  -- #[Attribute]
  highlight(0, 'phpAttributeName',            { fg = colors.pink,       bg = 'NONE'            })  -- Attribute name
  highlight(0, 'phpAttributeDelimiter',       { link = "Delimiter" })  -- #[ ]

  -- Comments
  highlight(0, 'phpComment',                  { link = "Comment" })  -- // and # comments
  highlight(0, 'phpCommentStar',              { link = "Comment" })  -- /* */ comments
  highlight(0, 'phpDocComment',               { link = "Comment" })  -- /** */ doc comments
  highlight(0, 'phpCommentTitle',             { link = "Comment" })  -- Comment title
  highlight(0, 'phpTodo',                     { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- PHPDoc
  highlight(0, 'phpDocTags',                  { fg = colors.blue,       bg = 'NONE'            })  -- @param, @return, @throws, etc.
  highlight(0, 'phpDocCustomTags',            { fg = colors.blue,       bg = 'NONE'            })  -- Custom doc tags
  highlight(0, 'phpDocParam',                 { fg = colors.purple,     bg = 'NONE'            })  -- Parameter name in doc
  highlight(0, 'phpDocIdentifier',            { fg = colors.purple,     bg = 'NONE'            })  -- Identifier in doc
  highlight(0, 'phpDocNamespaceSeparator',    { fg = colors.white,      bg = 'NONE'            })  -- \ in doc types

  -- Errors
  highlight(0, 'phpOctalError',               { fg = colors.white,      bg = colors.red        })  -- Invalid octal
  highlight(0, 'phpBinaryError',              { fg = colors.white,      bg = colors.red        })  -- Invalid binary
  highlight(0, 'phpFloatError',               { fg = colors.white,      bg = colors.red        })  -- Invalid float
  highlight(0, 'phpParentError',              { fg = colors.white,      bg = colors.red        })  -- Mismatched delimiters
  highlight(0, 'phpInterpSimpleError',        { fg = colors.white,      bg = colors.red        })  -- Interpolation error

  -- Yield
  highlight(0, 'phpYield',                    { fg = colors.blue,       bg = 'NONE'            })  -- yield
  highlight(0, 'phpYieldFromKeyword',         { link = "Keyword" })  -- yield from

  -- Match Expression (PHP 8+)
  highlight(0, 'phpMatch',                    { fg = colors.blue,       bg = 'NONE'            })  -- match
  highlight(0, 'phpMatchArm',                 { fg = colors.white,      bg = 'NONE'            })  -- Match arm
  highlight(0, 'phpMatchDefault',             { fg = colors.blue,       bg = 'NONE'            })  -- default in match

  -- Named Arguments (PHP 8+)
  highlight(0, 'phpNamedArgument',            { fg = colors.blue,       bg = 'NONE'            })  -- name: in function calls

  -- Enums (PHP 8.1+)
  highlight(0, 'phpEnumKeyword',              { link = "Keyword" })  -- enum keyword
  highlight(0, 'phpEnumCase',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum cases

  -- Readonly (PHP 8.1+)
  highlight(0, 'phpReadonly',                 { fg = colors.blue,       bg = 'NONE'            })  -- readonly

  -- Fibers (PHP 8.1+)
  highlight(0, 'phpFiber',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Fiber class

  -- Fold Groups (for folding)
  highlight(0, 'phpFoldIfContainer',          { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldWhile',                { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldDoWhile',              { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldFor',                  { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldForeach',              { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldTryContainer',         { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldSwitch',               { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldFunction',             { link = "Function" })
  highlight(0, 'phpFoldClass',                { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldInterface',            { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldHtmlInside',           { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldCatch',                { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldFinally',              { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldElseIf',               { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldElse',                 { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldCase',                 { fg = colors.white,      bg = 'NONE'            })
  highlight(0, 'phpFoldDefault',              { fg = colors.white,      bg = 'NONE'            })


  -------------------------------------------------------------------------
  -- Treesitter PHP Captures
  -------------------------------------------------------------------------

  -- Keywords
  highlight(0, '@keyword.php',                     { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.function.php',            { link = "Keyword" })  -- function, fn
  highlight(0, '@keyword.operator.php',            { link = "Operator" })  -- and, or, xor, instanceof
  highlight(0, '@keyword.type.php',                { link = "Keyword" })  -- class, interface, trait, enum
  highlight(0, '@keyword.modifier.php',            { link = "Keyword" })  -- public, private, protected, static, final, abstract, readonly
  highlight(0, '@keyword.return.php',              { link = "Keyword" })  -- return
  highlight(0, '@keyword.conditional.php',         { link = "Conditional" })  -- if, else, elseif, switch, match
  highlight(0, '@keyword.conditional.ternary.php', { link = "Conditional" })  -- ?:
  highlight(0, '@keyword.repeat.php',              { link = "Keyword" })  -- for, foreach, while, do
  highlight(0, '@keyword.exception.php',           { link = "Keyword" })  -- try, catch, throw, finally
  highlight(0, '@keyword.import.php',              { link = "Keyword" })  -- use, namespace

  -- Functions
  highlight(0, '@function.php',                    { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.php',               { link = "Function" })  -- Function calls
  highlight(0, '@function.builtin.php',            { link = "Function" })  -- Built-in functions
  highlight(0, '@function.method.php',             { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.php',        { link = "Function" })  -- Method calls
  highlight(0, '@constructor.php',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- __construct, new ClassName

  -- Variables
  highlight(0, '@variable.php',                    { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.php',            { link = "Variable" })  -- $this, self, parent, static
  highlight(0, '@variable.parameter.php',          { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.php',             { link = "Variable" })  -- Object properties

  -- Types
  highlight(0, '@type.php',                        { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.php',                { link = "Type" })  -- int, string, array, etc.
  highlight(0, '@type.definition.php',             { link = "Type" })  -- Type in definition

  -- Properties
  highlight(0, '@property.php',                    { fg = colors.blue,       bg = 'NONE'            })  -- Object properties

  -- Constants
  highlight(0, '@constant.php',                    { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.php',            { link = "Constant" })  -- true, false, null, __LINE__, etc.
  highlight(0, '@boolean.php',                     { link = "Boolean" })  -- true, false

  -- Numbers
  highlight(0, '@number.php',                      { link = "Number" })  -- Numbers
  highlight(0, '@number.float.php',                { link = "Number" })  -- Floats

  -- Strings
  highlight(0, '@string.php',                      { link = "String" })  -- Strings
  highlight(0, '@string.escape.php',               { link = "String" })  -- Escape sequences

  -- Modules/Namespaces
  highlight(0, '@module.php',                      { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace names
  highlight(0, '@module.builtin.php',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in namespaces

  -- Labels
  highlight(0, '@label.php',                       { fg = colors.blue,       bg = 'NONE'            })  -- Labels (goto targets)

  -- Attributes
  highlight(0, '@attribute.php',                   { fg = colors.pink,       bg = 'NONE'            })  -- #[Attribute]

  -- Operators
  highlight(0, '@operator.php',                    { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.delimiter.php',       { link = "Delimiter" })  -- Delimiters (;, ,)
  highlight(0, '@punctuation.bracket.php',         { fg = colors.white,      bg = 'NONE'            })  -- Brackets

  -- Comments
  highlight(0, '@comment.php',                     { link = "Comment" })  -- Comments
  highlight(0, '@spell.php',                       { link = '@comment.php'                          })  -- Spell check


  -------------------------------------------------------------------------
  -- LSP Semantic Tokens
  -------------------------------------------------------------------------

  highlight(0, '@lsp.type.class.php',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Classes
  highlight(0, '@lsp.type.interface.php',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Interfaces
  highlight(0, '@lsp.type.trait.php',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Traits
  highlight(0, '@lsp.type.enum.php',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Enums
  highlight(0, '@lsp.type.enumMember.php',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum cases
  highlight(0, '@lsp.type.type.php',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@lsp.type.function.php',           { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@lsp.type.method.php',             { fg = colors.orange,     bg = 'NONE'            })  -- Methods
  highlight(0, '@lsp.type.property.php',           { fg = colors.blue,       bg = 'NONE'            })  -- Properties
  highlight(0, '@lsp.type.variable.php',           { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.php',          { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.namespace.php',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespaces
  highlight(0, '@lsp.type.string.php',             { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.php',             { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.php',            { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.php',           { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.comment.php',            { link = "Comment" })  -- Comments

  highlight(0, '@lsp.mod.static.php',              { fg = colors.blue,       bg = 'NONE'            })  -- Static members
  highlight(0, '@lsp.mod.readonly.php',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Readonly
  highlight(0, '@lsp.mod.deprecated.php',          { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- Deprecated
  highlight(0, '@lsp.mod.abstract.php',            { fg = colors.blue,       bg = 'NONE', italic = true })  -- Abstract
  highlight(0, '@lsp.mod.async.php',               { fg = colors.blue,       bg = 'NONE'            })  -- Async (Fibers)
  highlight(0, '@lsp.mod.declaration.php',         { fg = colors.orange,     bg = 'NONE'            })  -- Declarations
  highlight(0, '@lsp.mod.definition.php',          { fg = colors.orange,     bg = 'NONE'            })  -- Definitions
  highlight(0, '@lsp.mod.defaultLibrary.php',      { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions

  highlight(0, '@lsp.typemod.function.declaration.php', { fg = colors.orange, bg = 'NONE'          })  -- Function declarations
  highlight(0, '@lsp.typemod.method.declaration.php',   { fg = colors.orange, bg = 'NONE'          })  -- Method declarations
  highlight(0, '@lsp.typemod.variable.readonly.php',    { link = "Variable" })  -- Readonly variables

end

return php
