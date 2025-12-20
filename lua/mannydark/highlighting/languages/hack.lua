-------------------------------------------------------------------------------
-- Hack (Meta/HHVM)
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local hack      = {}


-------------------------------------------------------------------------------
-- Settings

hack.setupHighlighting = function()


  -------------------------------------------------------------------------
  -- Vim Hack Syntax Groups (vim-hack extends PHP syntax)
  -------------------------------------------------------------------------

  -- Type Declarations
  highlight(0, 'hackTypeDecl',                { link = "Type" })  -- type, newtype, shape
  highlight(0, 'hackGenericType',             { link = "Type" })  -- Generic types: Type<T>
  highlight(0, 'hackTypeAlias',               { link = "Type" })  -- Type alias names
  highlight(0, 'hackNewtype',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Newtype names

  -- Keywords
  highlight(0, 'hackKeyword',                 { link = "Keyword" })  -- Hack keywords
  highlight(0, 'hackStatement',               { fg = colors.blue,       bg = 'NONE'            })  -- Statements
  highlight(0, 'hackConditional',             { link = "Conditional" })  -- if, else, elseif, switch
  highlight(0, 'hackRepeat',                  { fg = colors.blue,       bg = 'NONE'            })  -- for, foreach, while, do
  highlight(0, 'hackException',               { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, throw, finally
  highlight(0, 'hackReturn',                  { fg = colors.blue,       bg = 'NONE'            })  -- return

  -- Async/Await (Hack's async features)
  highlight(0, 'hackAsync',                   { fg = colors.blue,       bg = 'NONE'            })  -- async
  highlight(0, 'hackAwait',                   { fg = colors.blue,       bg = 'NONE'            })  -- await
  highlight(0, 'hackConcurrent',              { fg = colors.blue,       bg = 'NONE'            })  -- concurrent
  highlight(0, 'hackAwaitable',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Awaitable<T>

  -- Storage Class and Modifiers
  highlight(0, 'hackStorageClass',            { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected, static, final, abstract
  highlight(0, 'hackModifier',                { fg = colors.blue,       bg = 'NONE'            })  -- Modifiers
  highlight(0, 'hackVisibility',              { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected
  highlight(0, 'hackStatic',                  { fg = colors.blue,       bg = 'NONE'            })  -- static
  highlight(0, 'hackFinal',                   { fg = colors.blue,       bg = 'NONE'            })  -- final
  highlight(0, 'hackAbstract',                { fg = colors.blue,       bg = 'NONE'            })  -- abstract
  highlight(0, 'hackReadonly',                { fg = colors.blue,       bg = 'NONE'            })  -- readonly

  -- Types (Hack's strict type system)
  highlight(0, 'hackType',                    { link = "Type" })  -- Type names
  highlight(0, 'hackBuiltinType',             { link = "Type" })  -- int, float, string, bool, num, arraykey
  highlight(0, 'hackInt',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- int
  highlight(0, 'hackFloat',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- float
  highlight(0, 'hackString',                  { link = "String" })  -- string
  highlight(0, 'hackBool',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- bool
  highlight(0, 'hackNum',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- num (int | float)
  highlight(0, 'hackArraykey',                { fg = colors.turquoise,  bg = 'NONE'            })  -- arraykey (int | string)
  highlight(0, 'hackMixed',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- mixed
  highlight(0, 'hackVoid',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- void
  highlight(0, 'hackNoreturn',                { fg = colors.turquoise,  bg = 'NONE'            })  -- noreturn
  highlight(0, 'hackNothing',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- nothing
  highlight(0, 'hackNonnull',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- nonnull
  highlight(0, 'hackNull',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- null
  highlight(0, 'hackResource',                { fg = colors.turquoise,  bg = 'NONE'            })  -- resource
  highlight(0, 'hackDynamic',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- dynamic
  highlight(0, 'hackThis',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- this (as return type)

  -- Nullable Types
  highlight(0, 'hackNullable',                { fg = colors.turquoise,  bg = 'NONE'            })  -- ?Type
  highlight(0, 'hackNullableOperator',        { link = "Operator" })  -- ? prefix

  -- Hack Collections (vec, dict, keyset)
  highlight(0, 'hackVec',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- vec
  highlight(0, 'hackDict',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- dict
  highlight(0, 'hackKeyset',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- keyset
  highlight(0, 'hackVector',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Vector (legacy)
  highlight(0, 'hackMap',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- Map (legacy)
  highlight(0, 'hackSet',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- Set (legacy)
  highlight(0, 'hackPair',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Pair (legacy)
  highlight(0, 'hackImmVector',               { fg = colors.turquoise,  bg = 'NONE'            })  -- ImmVector (legacy)
  highlight(0, 'hackImmMap',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- ImmMap (legacy)
  highlight(0, 'hackImmSet',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- ImmSet (legacy)

  -- Shape and Tuple
  highlight(0, 'hackShape',                   { fg = colors.blue,       bg = 'NONE'            })  -- shape keyword
  highlight(0, 'hackShapeType',               { link = "Type" })  -- shape(...) type
  highlight(0, 'hackShapeField',              { fg = colors.blue,       bg = 'NONE'            })  -- Shape field names
  highlight(0, 'hackTuple',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- tuple types
  highlight(0, 'hackTupleFunc',               { link = "Function" })  -- tuple() function

  -- Enums
  highlight(0, 'hackEnum',                    { fg = colors.blue,       bg = 'NONE'            })  -- enum keyword
  highlight(0, 'hackEnumName',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum names
  highlight(0, 'hackEnumClass',               { fg = colors.blue,       bg = 'NONE'            })  -- enum class keyword
  highlight(0, 'hackEnumMember',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum members/cases

  -- Classes and Interfaces
  highlight(0, 'hackClass',                   { fg = colors.blue,       bg = 'NONE'            })  -- class keyword
  highlight(0, 'hackClassName',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Class names
  highlight(0, 'hackInterface',               { fg = colors.blue,       bg = 'NONE'            })  -- interface keyword
  highlight(0, 'hackInterfaceName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Interface names
  highlight(0, 'hackTrait',                   { fg = colors.blue,       bg = 'NONE'            })  -- trait keyword
  highlight(0, 'hackTraitName',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Trait names
  highlight(0, 'hackExtends',                 { fg = colors.blue,       bg = 'NONE'            })  -- extends
  highlight(0, 'hackImplements',              { fg = colors.blue,       bg = 'NONE'            })  -- implements
  highlight(0, 'hackUse',                     { fg = colors.blue,       bg = 'NONE'            })  -- use (for traits)

  -- Generics
  highlight(0, 'hackGeneric',                 { fg = colors.purple,     bg = 'NONE'            })  -- Generic type parameters <T>
  highlight(0, 'hackTypeParameter',           { link = "Type" })  -- Type parameters
  highlight(0, 'hackTypeConstraint',          { link = "Type" })  -- as, super constraints
  highlight(0, 'hackVariance',                { fg = colors.blue,       bg = 'NONE'            })  -- +, - variance markers

  -- Functions and Methods
  highlight(0, 'hackFunction',                { link = "Function" })  -- function keyword
  highlight(0, 'hackFunctionName',            { link = "Function" })  -- Function names
  highlight(0, 'hackFunctionCall',            { link = "Function" })  -- Function calls
  highlight(0, 'hackMethod',                  { link = "Function" })  -- Method names
  highlight(0, 'hackMethodCall',              { link = "Function" })  -- Method calls
  highlight(0, 'hackLambda',                  { fg = colors.blue,       bg = 'NONE'            })  -- ==> (lambda arrow)
  highlight(0, 'hackClosure',                 { fg = colors.orange,     bg = 'NONE'            })  -- Closures

  -- Variables
  highlight(0, 'hackVariable',                { link = "Variable" })  -- $variable
  highlight(0, 'hackVarSelector',             { link = "Variable" })  -- $ sign
  highlight(0, 'hackParameter',               { fg = colors.white,      bg = 'NONE'            })  -- Function parameters
  highlight(0, 'hackProperty',                { fg = colors.blue,       bg = 'NONE'            })  -- Object properties
  highlight(0, 'hackThisVar',                 { link = "Variable" })  -- $this
  highlight(0, 'hackSelf',                    { fg = colors.pink,       bg = 'NONE'            })  -- self
  highlight(0, 'hackParent',                  { fg = colors.pink,       bg = 'NONE'            })  -- parent
  highlight(0, 'hackStaticVar',               { link = "Variable" })  -- static (variable context)

  -- Operators
  highlight(0, 'hackOperator',                { link = "Operator" })  -- Operators
  highlight(0, 'hackArrow',                   { fg = colors.white,      bg = 'NONE'            })  -- -> (member access)
  highlight(0, 'hackNullsafeArrow',           { fg = colors.white,      bg = 'NONE'            })  -- ?-> (nullsafe)
  highlight(0, 'hackDoubleArrow',             { fg = colors.blue,       bg = 'NONE'            })  -- => (array/shape)
  highlight(0, 'hackLambdaArrow',             { fg = colors.blue,       bg = 'NONE'            })  -- ==> (lambda)
  highlight(0, 'hackDoubleColon',             { fg = colors.white,      bg = 'NONE'            })  -- :: (static access)
  highlight(0, 'hackPipe',                    { fg = colors.white,      bg = 'NONE'            })  -- |> (pipe)
  highlight(0, 'hackCoalesce',                { fg = colors.white,      bg = 'NONE'            })  -- ?? (null coalesce)
  highlight(0, 'hackSpread',                  { fg = colors.white,      bg = 'NONE'            })  -- ... (spread/splat)
  highlight(0, 'hackConcat',                  { fg = colors.white,      bg = 'NONE'            })  -- . (string concat)
  highlight(0, 'hackIs',                      { fg = colors.blue,       bg = 'NONE'            })  -- is (type check)
  highlight(0, 'hackAs',                      { fg = colors.blue,       bg = 'NONE'            })  -- as (type assertion)
  highlight(0, 'hackInstanceof',              { fg = colors.blue,       bg = 'NONE'            })  -- instanceof

  -- Constants
  highlight(0, 'hackConstant',                { link = "Constant" })  -- Constants
  highlight(0, 'hackConstKeyword',            { link = "Keyword" })  -- const keyword
  highlight(0, 'hackClassConstant',           { link = "Constant" })  -- Class constants
  highlight(0, 'hackMagicConstant',           { link = "Constant" })  -- __CLASS__, __FUNCTION__, etc.
  highlight(0, 'hackBoolean',                 { link = "Boolean" })  -- true, false

  -- Numbers
  highlight(0, 'hackNumber',                  { link = "Number" })  -- Integer literals
  highlight(0, 'hackFloatLiteral',            { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'hackHexNumber',               { link = "Number" })  -- Hexadecimal
  highlight(0, 'hackOctalNumber',             { link = "Number" })  -- Octal
  highlight(0, 'hackBinaryNumber',            { link = "Number" })  -- Binary

  -- Strings
  highlight(0, 'hackStringLiteral',           { link = "String" })  -- Strings
  highlight(0, 'hackStringSingle',            { link = "String" })  -- Single-quoted
  highlight(0, 'hackStringDouble',            { link = "String" })  -- Double-quoted
  highlight(0, 'hackHeredoc',                 { fg = colors.redLight,   bg = 'NONE'            })  -- Heredoc
  highlight(0, 'hackNowdoc',                  { fg = colors.redLight,   bg = 'NONE'            })  -- Nowdoc
  highlight(0, 'hackStringEscape',            { link = "String" })  -- Escape sequences
  highlight(0, 'hackInterpolation',           { fg = colors.purple,     bg = 'NONE'            })  -- String interpolation

  -- Namespaces
  highlight(0, 'hackNamespace',               { fg = colors.blue,       bg = 'NONE'            })  -- namespace keyword
  highlight(0, 'hackNamespaceName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace name
  highlight(0, 'hackNamespaceSeparator',      { fg = colors.white,      bg = 'NONE'            })  -- \ separator
  highlight(0, 'hackUseKeyword',              { link = "Keyword" })  -- use keyword

  -- Attributes (Hack's annotation system)
  highlight(0, 'hackAttribute',               { fg = colors.pink,       bg = 'NONE'            })  -- <<Attribute>>
  highlight(0, 'hackAttributeName',           { fg = colors.pink,       bg = 'NONE'            })  -- Attribute name
  highlight(0, 'hackAttributeDelimiter',      { link = "Delimiter" })  -- << >>
  highlight(0, 'hackOverride',                { fg = colors.pink,       bg = 'NONE'            })  -- <<__Override>>
  highlight(0, 'hackMemoize',                 { fg = colors.pink,       bg = 'NONE'            })  -- <<__Memoize>>
  highlight(0, 'hackDeprecated',              { fg = colors.pink,       bg = 'NONE'            })  -- <<__Deprecated>>
  highlight(0, 'hackEntryPoint',              { fg = colors.pink,       bg = 'NONE'            })  -- <<__EntryPoint>>

  -- XHP (JSX-like syntax in Hack)
  highlight(0, 'hackXhpTag',                  { fg = colors.blue,       bg = 'NONE'            })  -- XHP tags
  highlight(0, 'hackXhpTagName',              { fg = colors.blue,       bg = 'NONE'            })  -- XHP tag name
  highlight(0, 'hackXhpAttribute',            { fg = colors.turquoise,  bg = 'NONE'            })  -- XHP attributes
  highlight(0, 'hackXhpAttributeValue',       { fg = colors.redLight,   bg = 'NONE'            })  -- XHP attribute values
  highlight(0, 'hackXhpText',                 { fg = colors.white,      bg = 'NONE'            })  -- XHP text content
  highlight(0, 'hackXhpExpr',                 { fg = colors.white,      bg = 'NONE'            })  -- XHP expressions {expr}
  highlight(0, 'hackXhpSpread',               { fg = colors.white,      bg = 'NONE'            })  -- XHP spread {...$attrs}
  highlight(0, 'hackXhpDelimiter',            { link = "Delimiter" })  -- < > />
  highlight(0, 'hackXhpClass',                { fg = colors.turquoise,  bg = 'NONE'            })  -- XHP class names

  -- Comments
  highlight(0, 'hackComment',                 { link = "Comment" })  -- Comments
  highlight(0, 'hackLineComment',             { link = "Comment" })  -- // comments
  highlight(0, 'hackBlockComment',            { link = "Comment" })  -- /* */ comments
  highlight(0, 'hackDocComment',              { link = "Comment" })  -- /** */ doc comments
  highlight(0, 'hackTodo',                    { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, HACK

  -- HackDoc Tags
  highlight(0, 'hackDocTag',                  { fg = colors.blue,       bg = 'NONE'            })  -- @param, @return, etc.
  highlight(0, 'hackDocParam',                { fg = colors.purple,     bg = 'NONE'            })  -- Parameter in doc
  highlight(0, 'hackDocType',                 { link = "Type" })  -- Type in doc

  -- Punctuation
  highlight(0, 'hackPunctuation',             { fg = colors.white,      bg = 'NONE'            })  -- Punctuation
  highlight(0, 'hackParen',                   { fg = colors.white,      bg = 'NONE'            })  -- ( )
  highlight(0, 'hackBracket',                 { fg = colors.white,      bg = 'NONE'            })  -- [ ]
  highlight(0, 'hackBrace',                   { fg = colors.white,      bg = 'NONE'            })  -- { }
  highlight(0, 'hackAngleBracket',            { fg = colors.white,      bg = 'NONE'            })  -- < > (generics)
  highlight(0, 'hackComma',                   { fg = colors.white,      bg = 'NONE'            })  -- ,
  highlight(0, 'hackSemicolon',               { fg = colors.white,      bg = 'NONE'            })  -- ;
  highlight(0, 'hackColon',                   { fg = colors.white,      bg = 'NONE'            })  -- :

  -- Module System (Hack modules)
  highlight(0, 'hackModule',                  { fg = colors.blue,       bg = 'NONE'            })  -- module keyword
  highlight(0, 'hackModuleName',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Module name
  highlight(0, 'hackInternal',                { fg = colors.blue,       bg = 'NONE'            })  -- internal keyword

  -- Context and Capabilities (Hack's effect system)
  highlight(0, 'hackContext',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Context/capability
  highlight(0, 'hackContextList',             { fg = colors.turquoise,  bg = 'NONE'            })  -- [ctx1, ctx2]


  -------------------------------------------------------------------------
  -- Treesitter Hack Captures
  -------------------------------------------------------------------------

  -- Variables
  highlight(0, '@variable.hack',                   { link = "Variable" })  -- Variables
  highlight(0, '@variable.builtin.hack',           { link = "Variable" })  -- $this, self, parent
  highlight(0, '@variable.parameter.hack',         { link = "Variable" })  -- Parameters

  -- Types
  highlight(0, '@type.hack',                       { link = "Type" })  -- Types
  highlight(0, '@type.builtin.hack',               { link = "Type" })  -- Built-in types

  -- Functions
  highlight(0, '@function.hack',                   { link = "Function" })  -- Functions
  highlight(0, '@function.call.hack',              { link = "Function" })  -- Function calls
  highlight(0, '@function.method.hack',            { link = "Function" })  -- Methods
  highlight(0, '@function.method.call.hack',       { link = "Function" })  -- Method calls

  -- Keywords
  highlight(0, '@keyword.hack',                    { link = "Keyword" })  -- Keywords
  highlight(0, '@keyword.function.hack',           { link = "Keyword" })  -- function, async
  highlight(0, '@keyword.type.hack',               { link = "Keyword" })  -- class, interface, trait, enum
  highlight(0, '@keyword.coroutine.hack',          { link = "Keyword" })  -- async, await
  highlight(0, '@keyword.import.hack',             { link = "Keyword" })  -- use, namespace
  highlight(0, '@keyword.operator.hack',           { link = "Operator" })  -- is, as, instanceof
  highlight(0, '@keyword.return.hack',             { link = "Keyword" })  -- return
  highlight(0, '@keyword.modifier.hack',           { link = "Keyword" })  -- public, private, protected, static, final, abstract
  highlight(0, '@keyword.conditional.hack',        { link = "Conditional" })  -- if, else, switch
  highlight(0, '@keyword.conditional.ternary.hack', { link = "Conditional" })  -- ?:
  highlight(0, '@keyword.exception.hack',          { link = "Keyword" })  -- try, catch, throw, finally
  highlight(0, '@keyword.repeat.hack',             { link = "Keyword" })  -- for, foreach, while

  -- Constants
  highlight(0, '@constant.hack',                   { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.hack',           { link = "Constant" })  -- true, false, null
  highlight(0, '@boolean.hack',                    { link = "Boolean" })  -- true, false

  -- Numbers
  highlight(0, '@number.hack',                     { link = "Number" })  -- Numbers
  highlight(0, '@number.float.hack',               { link = "Number" })  -- Floats

  -- Strings
  highlight(0, '@string.hack',                     { link = "String" })  -- Strings

  -- Modules
  highlight(0, '@module.hack',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespaces/modules

  -- Attributes
  highlight(0, '@attribute.hack',                  { fg = colors.pink,       bg = 'NONE'            })  -- <<Attribute>>

  -- Operators
  highlight(0, '@operator.hack',                   { link = "Operator" })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.hack',        { fg = colors.white,      bg = 'NONE'            })  -- Brackets
  highlight(0, '@punctuation.delimiter.hack',      { link = "Delimiter" })  -- Delimiters

  -- XHP/Tags
  highlight(0, '@tag.hack',                        { fg = colors.blue,       bg = 'NONE'            })  -- XHP tags
  highlight(0, '@tag.delimiter.hack',              { link = "Delimiter" })  -- < > />

  -- Comments
  highlight(0, '@comment.hack',                    { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.hack',      { link = "Comment" })  -- Doc comments
  highlight(0, '@spell.hack',                      { link = '@comment.hack'                         })  -- Spell check

  -- None/Other
  highlight(0, '@none.hack',                       { fg = colors.white,      bg = 'NONE'            })  -- Uncategorized


  -------------------------------------------------------------------------
  -- LSP Semantic Tokens
  -------------------------------------------------------------------------

  highlight(0, '@lsp.type.class.hack',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Classes
  highlight(0, '@lsp.type.interface.hack',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Interfaces
  highlight(0, '@lsp.type.trait.hack',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Traits
  highlight(0, '@lsp.type.enum.hack',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Enums
  highlight(0, '@lsp.type.enumMember.hack',        { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum members
  highlight(0, '@lsp.type.type.hack',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@lsp.type.typeParameter.hack',     { fg = colors.purple,     bg = 'NONE'            })  -- Type parameters
  highlight(0, '@lsp.type.function.hack',          { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@lsp.type.method.hack',            { fg = colors.orange,     bg = 'NONE'            })  -- Methods
  highlight(0, '@lsp.type.property.hack',          { fg = colors.blue,       bg = 'NONE'            })  -- Properties
  highlight(0, '@lsp.type.variable.hack',          { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.hack',         { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.namespace.hack',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespaces
  highlight(0, '@lsp.type.string.hack',            { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.hack',            { link = "Number" })  -- Numbers
  highlight(0, '@lsp.type.keyword.hack',           { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.hack',          { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.comment.hack',           { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.decorator.hack',         { fg = colors.pink,       bg = 'NONE'            })  -- Attributes

  highlight(0, '@lsp.mod.static.hack',             { fg = colors.blue,       bg = 'NONE'            })  -- Static
  highlight(0, '@lsp.mod.readonly.hack',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Readonly
  highlight(0, '@lsp.mod.async.hack',              { fg = colors.blue,       bg = 'NONE'            })  -- Async
  highlight(0, '@lsp.mod.abstract.hack',           { fg = colors.blue,       bg = 'NONE', italic = true })  -- Abstract
  highlight(0, '@lsp.mod.deprecated.hack',         { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- Deprecated
  highlight(0, '@lsp.mod.declaration.hack',        { fg = colors.orange,     bg = 'NONE'            })  -- Declarations
  highlight(0, '@lsp.mod.definition.hack',         { fg = colors.orange,     bg = 'NONE'            })  -- Definitions
  highlight(0, '@lsp.mod.defaultLibrary.hack',     { fg = colors.orange,     bg = 'NONE'            })  -- Built-in

end

return hack
