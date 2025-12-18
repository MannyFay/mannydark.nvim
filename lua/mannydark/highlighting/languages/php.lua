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
  highlight(0, 'phpKeyword',                  { fg = colors.blue,       bg = 'NONE'            })  -- var, const
  highlight(0, 'phpConditional',              { fg = colors.blue,       bg = 'NONE'            })  -- if, else, elseif, switch, match
  highlight(0, 'phpRepeat',                   { fg = colors.blue,       bg = 'NONE'            })  -- for, foreach, while, do
  highlight(0, 'phpStatement',                { fg = colors.blue,       bg = 'NONE'            })  -- return, break, continue, yield, yield from
  highlight(0, 'phpLabel',                    { fg = colors.blue,       bg = 'NONE'            })  -- case, default
  highlight(0, 'phpStructure',                { fg = colors.blue,       bg = 'NONE'            })  -- class, interface, trait, enum, namespace, extends, implements
  highlight(0, 'phpException',                { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, throw, finally
  highlight(0, 'phpDefine',                   { fg = colors.blue,       bg = 'NONE'            })  -- function, fn, echo, print, new, clone
  highlight(0, 'phpInclude',                  { fg = colors.blue,       bg = 'NONE'            })  -- include, include_once, require, require_once, use

  -- Storage Class and Modifiers
  highlight(0, 'phpStorageClass',             { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected, static, final, abstract, readonly
  highlight(0, 'phpSCKeyword',                { fg = colors.blue,       bg = 'NONE'            })  -- Storage class keywords
  highlight(0, 'phpFCKeyword',                { fg = colors.blue,       bg = 'NONE'            })  -- Function class keywords

  -- Types
  highlight(0, 'phpType',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- int, float, string, bool, array, object, mixed, void, never, null, callable, iterable
  highlight(0, 'phpNullValue',                { fg = colors.turquoise,  bg = 'NONE'            })  -- null
  highlight(0, 'phpBoolean',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false

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
  highlight(0, 'phpClassDelimiter',           { fg = colors.white,      bg = 'NONE'            })  -- Class delimiters

  -- Functions and Methods
  highlight(0, 'phpFunction',                 { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'phpFunctions',                { fg = colors.orange,     bg = 'NONE'            })  -- Built-in function names
  highlight(0, 'phpMethod',                   { fg = colors.orange,     bg = 'NONE'            })  -- Method names
  highlight(0, 'phpMethods',                  { fg = colors.orange,     bg = 'NONE'            })  -- Object method calls
  highlight(0, 'phpMethodsVar',               { fg = colors.purple,     bg = 'NONE'            })  -- Variable method calls
  highlight(0, 'phpSpecialFunction',          { fg = colors.orange,     bg = 'NONE'            })  -- Magic methods, isset(), eval(), etc.
  highlight(0, 'phpMagicMethods',             { fg = colors.orange,     bg = 'NONE'            })  -- __construct, __destruct, __call, etc.

  -- Variables
  highlight(0, 'phpIdentifier',               { fg = colors.purple,     bg = 'NONE'            })  -- Variable names ($var)
  highlight(0, 'phpIdentifierSimply',         { fg = colors.purple,     bg = 'NONE'            })  -- Variables in ${...} syntax
  highlight(0, 'phpIdentifierComplex',        { fg = colors.purple,     bg = 'NONE'            })  -- Complex variable expressions
  highlight(0, 'phpVarSelector',              { fg = colors.purple,     bg = 'NONE'            })  -- $ dollar sign
  highlight(0, 'phpVariable',                 { fg = colors.purple,     bg = 'NONE'            })  -- Variable names

  -- Superglobals and Special Variables
  highlight(0, 'phpSuperglobals',             { fg = colors.pink,       bg = 'NONE'            })  -- $_GET, $_POST, $_SESSION, etc.
  highlight(0, 'phpIntVar',                   { fg = colors.pink,       bg = 'NONE'            })  -- Internal superglobals
  highlight(0, 'phpEnvVar',                   { fg = colors.pink,       bg = 'NONE'            })  -- Environment variables
  highlight(0, 'phpServerVars',               { fg = colors.pink,       bg = 'NONE'            })  -- $_SERVER variables
  highlight(0, 'phpGlobals',                  { fg = colors.pink,       bg = 'NONE'            })  -- $GLOBALS

  -- Constants
  highlight(0, 'phpConstants',                { fg = colors.turquoise,  bg = 'NONE'            })  -- User-defined constants
  highlight(0, 'phpConstant',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- __LINE__, __FILE__, __FUNCTION__, etc.
  highlight(0, 'phpCoreConstant',             { fg = colors.turquoise,  bg = 'NONE'            })  -- PHP_VERSION, E_ERROR, etc.
  highlight(0, 'phpMagicConstants',           { fg = colors.pink,       bg = 'NONE'            })  -- __CLASS__, __METHOD__, __DIR__, etc.

  -- Operators
  highlight(0, 'phpOperator',                 { fg = colors.white,      bg = 'NONE'            })  -- Standard operators
  highlight(0, 'phpRelation',                 { fg = colors.white,      bg = 'NONE'            })  -- Comparison operators
  highlight(0, 'phpComparison',               { fg = colors.white,      bg = 'NONE'            })  -- Comparison operators (alt)
  highlight(0, 'phpAssignByRef',              { fg = colors.white,      bg = 'NONE'            })  -- Reference assignment =&
  highlight(0, 'phpSplatOperator',            { fg = colors.white,      bg = 'NONE'            })  -- ... (spread operator)
  highlight(0, 'phpNullsafeOperator',         { fg = colors.white,      bg = 'NONE'            })  -- ?-> (nullsafe operator)
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
  highlight(0, 'phpNumber',                   { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'phpFloat',                    { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'phpHexNumber',                { fg = colors.greenLight, bg = 'NONE'            })  -- Hexadecimal: 0x1A
  highlight(0, 'phpOctalNumber',              { fg = colors.greenLight, bg = 'NONE'            })  -- Octal: 0o17
  highlight(0, 'phpBinaryNumber',             { fg = colors.greenLight, bg = 'NONE'            })  -- Binary: 0b1010

  -- Strings
  highlight(0, 'phpStringSingle',             { fg = colors.redLight,   bg = 'NONE'            })  -- Single-quoted strings
  highlight(0, 'phpStringDouble',             { fg = colors.redLight,   bg = 'NONE'            })  -- Double-quoted strings
  highlight(0, 'phpString',                   { fg = colors.redLight,   bg = 'NONE'            })  -- Generic string
  highlight(0, 'phpStringDelimiter',          { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters
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
  highlight(0, 'phpUseKeyword',               { fg = colors.blue,       bg = 'NONE'            })  -- use keyword
  highlight(0, 'phpUseClass',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Class in use statement
  highlight(0, 'phpUseFunction',              { fg = colors.orange,     bg = 'NONE'            })  -- Function in use statement
  highlight(0, 'phpUseAlias',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Alias in use statement
  highlight(0, 'phpUseConst',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Constant in use statement

  -- Attributes (PHP 8+)
  highlight(0, 'phpAttribute',                { fg = colors.pink,       bg = 'NONE'            })  -- #[Attribute]
  highlight(0, 'phpAttributeName',            { fg = colors.pink,       bg = 'NONE'            })  -- Attribute name
  highlight(0, 'phpAttributeDelimiter',       { fg = colors.white,      bg = 'NONE'            })  -- #[ ]

  -- Comments
  highlight(0, 'phpComment',                  { fg = colors.gray,       bg = 'NONE', italic = true })  -- // and # comments
  highlight(0, 'phpCommentStar',              { fg = colors.gray,       bg = 'NONE', italic = true })  -- /* */ comments
  highlight(0, 'phpDocComment',               { fg = colors.gray,       bg = 'NONE', italic = true })  -- /** */ doc comments
  highlight(0, 'phpCommentTitle',             { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comment title
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
  highlight(0, 'phpYieldFromKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- yield from

  -- Match Expression (PHP 8+)
  highlight(0, 'phpMatch',                    { fg = colors.blue,       bg = 'NONE'            })  -- match
  highlight(0, 'phpMatchArm',                 { fg = colors.white,      bg = 'NONE'            })  -- Match arm
  highlight(0, 'phpMatchDefault',             { fg = colors.blue,       bg = 'NONE'            })  -- default in match

  -- Named Arguments (PHP 8+)
  highlight(0, 'phpNamedArgument',            { fg = colors.blue,       bg = 'NONE'            })  -- name: in function calls

  -- Enums (PHP 8.1+)
  highlight(0, 'phpEnumKeyword',              { fg = colors.blue,       bg = 'NONE'            })  -- enum keyword
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
  highlight(0, 'phpFoldFunction',             { fg = colors.white,      bg = 'NONE'            })
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
  highlight(0, '@keyword.php',                     { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@keyword.function.php',            { fg = colors.blue,       bg = 'NONE'            })  -- function, fn
  highlight(0, '@keyword.operator.php',            { fg = colors.blue,       bg = 'NONE'            })  -- and, or, xor, instanceof
  highlight(0, '@keyword.type.php',                { fg = colors.blue,       bg = 'NONE'            })  -- class, interface, trait, enum
  highlight(0, '@keyword.modifier.php',            { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected, static, final, abstract, readonly
  highlight(0, '@keyword.return.php',              { fg = colors.blue,       bg = 'NONE'            })  -- return
  highlight(0, '@keyword.conditional.php',         { fg = colors.blue,       bg = 'NONE'            })  -- if, else, elseif, switch, match
  highlight(0, '@keyword.conditional.ternary.php', { fg = colors.blue,       bg = 'NONE'            })  -- ?:
  highlight(0, '@keyword.repeat.php',              { fg = colors.blue,       bg = 'NONE'            })  -- for, foreach, while, do
  highlight(0, '@keyword.exception.php',           { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, throw, finally
  highlight(0, '@keyword.import.php',              { fg = colors.blue,       bg = 'NONE'            })  -- use, namespace

  -- Functions
  highlight(0, '@function.php',                    { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, '@function.call.php',               { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, '@function.builtin.php',            { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions
  highlight(0, '@function.method.php',             { fg = colors.orange,     bg = 'NONE'            })  -- Method definitions
  highlight(0, '@function.method.call.php',        { fg = colors.orange,     bg = 'NONE'            })  -- Method calls
  highlight(0, '@constructor.php',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- __construct, new ClassName

  -- Variables
  highlight(0, '@variable.php',                    { fg = colors.purple,     bg = 'NONE'            })  -- Variables
  highlight(0, '@variable.builtin.php',            { fg = colors.pink,       bg = 'NONE'            })  -- $this, self, parent, static
  highlight(0, '@variable.parameter.php',          { fg = colors.white,      bg = 'NONE'            })  -- Function parameters
  highlight(0, '@variable.member.php',             { fg = colors.blue,       bg = 'NONE'            })  -- Object properties

  -- Types
  highlight(0, '@type.php',                        { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, '@type.builtin.php',                { fg = colors.turquoise,  bg = 'NONE'            })  -- int, string, array, etc.
  highlight(0, '@type.definition.php',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Type in definition

  -- Properties
  highlight(0, '@property.php',                    { fg = colors.blue,       bg = 'NONE'            })  -- Object properties

  -- Constants
  highlight(0, '@constant.php',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Constants
  highlight(0, '@constant.builtin.php',            { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false, null, __LINE__, etc.
  highlight(0, '@boolean.php',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false

  -- Numbers
  highlight(0, '@number.php',                      { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@number.float.php',                { fg = colors.greenLight, bg = 'NONE'            })  -- Floats

  -- Strings
  highlight(0, '@string.php',                      { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@string.escape.php',               { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences

  -- Modules/Namespaces
  highlight(0, '@module.php',                      { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace names
  highlight(0, '@module.builtin.php',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in namespaces

  -- Labels
  highlight(0, '@label.php',                       { fg = colors.blue,       bg = 'NONE'            })  -- Labels (goto targets)

  -- Attributes
  highlight(0, '@attribute.php',                   { fg = colors.pink,       bg = 'NONE'            })  -- #[Attribute]

  -- Operators
  highlight(0, '@operator.php',                    { fg = colors.white,      bg = 'NONE'            })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.delimiter.php',       { fg = colors.white,      bg = 'NONE'            })  -- Delimiters (;, ,)
  highlight(0, '@punctuation.bracket.php',         { fg = colors.white,      bg = 'NONE'            })  -- Brackets

  -- Comments
  highlight(0, '@comment.php',                     { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments
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
  highlight(0, '@lsp.type.variable.php',           { fg = colors.purple,     bg = 'NONE'            })  -- Variables
  highlight(0, '@lsp.type.parameter.php',          { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.namespace.php',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespaces
  highlight(0, '@lsp.type.string.php',             { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@lsp.type.number.php',             { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@lsp.type.keyword.php',            { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@lsp.type.operator.php',           { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, '@lsp.type.comment.php',            { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments

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
  highlight(0, '@lsp.typemod.variable.readonly.php',    { fg = colors.turquoise, bg = 'NONE'       })  -- Readonly variables

end

return php
