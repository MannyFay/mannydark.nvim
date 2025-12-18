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
  highlight(0, 'hackTypeDecl',                { fg = colors.blue,       bg = 'NONE'            })  -- type, newtype, shape
  highlight(0, 'hackGenericType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Generic types: Type<T>
  highlight(0, 'hackTypeAlias',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Type alias names
  highlight(0, 'hackNewtype',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Newtype names

  -- Keywords
  highlight(0, 'hackKeyword',                 { fg = colors.blue,       bg = 'NONE'            })  -- Hack keywords
  highlight(0, 'hackStatement',               { fg = colors.blue,       bg = 'NONE'            })  -- Statements
  highlight(0, 'hackConditional',             { fg = colors.blue,       bg = 'NONE'            })  -- if, else, elseif, switch
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
  highlight(0, 'hackType',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'hackBuiltinType',             { fg = colors.turquoise,  bg = 'NONE'            })  -- int, float, string, bool, num, arraykey
  highlight(0, 'hackInt',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- int
  highlight(0, 'hackFloat',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- float
  highlight(0, 'hackString',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- string
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
  highlight(0, 'hackNullableOperator',        { fg = colors.white,      bg = 'NONE'            })  -- ? prefix

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
  highlight(0, 'hackShapeType',               { fg = colors.turquoise,  bg = 'NONE'            })  -- shape(...) type
  highlight(0, 'hackShapeField',              { fg = colors.blue,       bg = 'NONE'            })  -- Shape field names
  highlight(0, 'hackTuple',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- tuple types
  highlight(0, 'hackTupleFunc',               { fg = colors.orange,     bg = 'NONE'            })  -- tuple() function

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
  highlight(0, 'hackTypeParameter',           { fg = colors.purple,     bg = 'NONE'            })  -- Type parameters
  highlight(0, 'hackTypeConstraint',          { fg = colors.blue,       bg = 'NONE'            })  -- as, super constraints
  highlight(0, 'hackVariance',                { fg = colors.blue,       bg = 'NONE'            })  -- +, - variance markers

  -- Functions and Methods
  highlight(0, 'hackFunction',                { fg = colors.orange,     bg = 'NONE'            })  -- function keyword
  highlight(0, 'hackFunctionName',            { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'hackFunctionCall',            { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'hackMethod',                  { fg = colors.orange,     bg = 'NONE'            })  -- Method names
  highlight(0, 'hackMethodCall',              { fg = colors.orange,     bg = 'NONE'            })  -- Method calls
  highlight(0, 'hackLambda',                  { fg = colors.blue,       bg = 'NONE'            })  -- ==> (lambda arrow)
  highlight(0, 'hackClosure',                 { fg = colors.orange,     bg = 'NONE'            })  -- Closures

  -- Variables
  highlight(0, 'hackVariable',                { fg = colors.purple,     bg = 'NONE'            })  -- $variable
  highlight(0, 'hackVarSelector',             { fg = colors.purple,     bg = 'NONE'            })  -- $ sign
  highlight(0, 'hackParameter',               { fg = colors.white,      bg = 'NONE'            })  -- Function parameters
  highlight(0, 'hackProperty',                { fg = colors.blue,       bg = 'NONE'            })  -- Object properties
  highlight(0, 'hackThisVar',                 { fg = colors.pink,       bg = 'NONE'            })  -- $this
  highlight(0, 'hackSelf',                    { fg = colors.pink,       bg = 'NONE'            })  -- self
  highlight(0, 'hackParent',                  { fg = colors.pink,       bg = 'NONE'            })  -- parent
  highlight(0, 'hackStaticVar',               { fg = colors.pink,       bg = 'NONE'            })  -- static (variable context)

  -- Operators
  highlight(0, 'hackOperator',                { fg = colors.white,      bg = 'NONE'            })  -- Operators
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
  highlight(0, 'hackConstant',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Constants
  highlight(0, 'hackConstKeyword',            { fg = colors.blue,       bg = 'NONE'            })  -- const keyword
  highlight(0, 'hackClassConstant',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Class constants
  highlight(0, 'hackMagicConstant',           { fg = colors.pink,       bg = 'NONE'            })  -- __CLASS__, __FUNCTION__, etc.
  highlight(0, 'hackBoolean',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false

  -- Numbers
  highlight(0, 'hackNumber',                  { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'hackFloatLiteral',            { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'hackHexNumber',               { fg = colors.greenLight, bg = 'NONE'            })  -- Hexadecimal
  highlight(0, 'hackOctalNumber',             { fg = colors.greenLight, bg = 'NONE'            })  -- Octal
  highlight(0, 'hackBinaryNumber',            { fg = colors.greenLight, bg = 'NONE'            })  -- Binary

  -- Strings
  highlight(0, 'hackStringLiteral',           { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, 'hackStringSingle',            { fg = colors.redLight,   bg = 'NONE'            })  -- Single-quoted
  highlight(0, 'hackStringDouble',            { fg = colors.redLight,   bg = 'NONE'            })  -- Double-quoted
  highlight(0, 'hackHeredoc',                 { fg = colors.redLight,   bg = 'NONE'            })  -- Heredoc
  highlight(0, 'hackNowdoc',                  { fg = colors.redLight,   bg = 'NONE'            })  -- Nowdoc
  highlight(0, 'hackStringEscape',            { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, 'hackInterpolation',           { fg = colors.purple,     bg = 'NONE'            })  -- String interpolation

  -- Namespaces
  highlight(0, 'hackNamespace',               { fg = colors.blue,       bg = 'NONE'            })  -- namespace keyword
  highlight(0, 'hackNamespaceName',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespace name
  highlight(0, 'hackNamespaceSeparator',      { fg = colors.white,      bg = 'NONE'            })  -- \ separator
  highlight(0, 'hackUseKeyword',              { fg = colors.blue,       bg = 'NONE'            })  -- use keyword

  -- Attributes (Hack's annotation system)
  highlight(0, 'hackAttribute',               { fg = colors.pink,       bg = 'NONE'            })  -- <<Attribute>>
  highlight(0, 'hackAttributeName',           { fg = colors.pink,       bg = 'NONE'            })  -- Attribute name
  highlight(0, 'hackAttributeDelimiter',      { fg = colors.white,      bg = 'NONE'            })  -- << >>
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
  highlight(0, 'hackXhpDelimiter',            { fg = colors.white,      bg = 'NONE'            })  -- < > />
  highlight(0, 'hackXhpClass',                { fg = colors.turquoise,  bg = 'NONE'            })  -- XHP class names

  -- Comments
  highlight(0, 'hackComment',                 { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments
  highlight(0, 'hackLineComment',             { fg = colors.gray,       bg = 'NONE', italic = true })  -- // comments
  highlight(0, 'hackBlockComment',            { fg = colors.gray,       bg = 'NONE', italic = true })  -- /* */ comments
  highlight(0, 'hackDocComment',              { fg = colors.gray,       bg = 'NONE', italic = true })  -- /** */ doc comments
  highlight(0, 'hackTodo',                    { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, HACK

  -- HackDoc Tags
  highlight(0, 'hackDocTag',                  { fg = colors.blue,       bg = 'NONE'            })  -- @param, @return, etc.
  highlight(0, 'hackDocParam',                { fg = colors.purple,     bg = 'NONE'            })  -- Parameter in doc
  highlight(0, 'hackDocType',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Type in doc

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
  highlight(0, '@variable.hack',                   { fg = colors.purple,     bg = 'NONE'            })  -- Variables
  highlight(0, '@variable.builtin.hack',           { fg = colors.pink,       bg = 'NONE'            })  -- $this, self, parent
  highlight(0, '@variable.parameter.hack',         { fg = colors.white,      bg = 'NONE'            })  -- Parameters

  -- Types
  highlight(0, '@type.hack',                       { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@type.builtin.hack',               { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in types

  -- Functions
  highlight(0, '@function.hack',                   { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@function.call.hack',              { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, '@function.method.hack',            { fg = colors.orange,     bg = 'NONE'            })  -- Methods
  highlight(0, '@function.method.call.hack',       { fg = colors.orange,     bg = 'NONE'            })  -- Method calls

  -- Keywords
  highlight(0, '@keyword.hack',                    { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@keyword.function.hack',           { fg = colors.blue,       bg = 'NONE'            })  -- function, async
  highlight(0, '@keyword.type.hack',               { fg = colors.blue,       bg = 'NONE'            })  -- class, interface, trait, enum
  highlight(0, '@keyword.coroutine.hack',          { fg = colors.blue,       bg = 'NONE'            })  -- async, await
  highlight(0, '@keyword.import.hack',             { fg = colors.blue,       bg = 'NONE'            })  -- use, namespace
  highlight(0, '@keyword.operator.hack',           { fg = colors.blue,       bg = 'NONE'            })  -- is, as, instanceof
  highlight(0, '@keyword.return.hack',             { fg = colors.blue,       bg = 'NONE'            })  -- return
  highlight(0, '@keyword.modifier.hack',           { fg = colors.blue,       bg = 'NONE'            })  -- public, private, protected, static, final, abstract
  highlight(0, '@keyword.conditional.hack',        { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch
  highlight(0, '@keyword.conditional.ternary.hack', { fg = colors.blue,      bg = 'NONE'            })  -- ?:
  highlight(0, '@keyword.exception.hack',          { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, throw, finally
  highlight(0, '@keyword.repeat.hack',             { fg = colors.blue,       bg = 'NONE'            })  -- for, foreach, while

  -- Constants
  highlight(0, '@constant.hack',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- Constants
  highlight(0, '@constant.builtin.hack',           { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false, null
  highlight(0, '@boolean.hack',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false

  -- Numbers
  highlight(0, '@number.hack',                     { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@number.float.hack',               { fg = colors.greenLight, bg = 'NONE'            })  -- Floats

  -- Strings
  highlight(0, '@string.hack',                     { fg = colors.redLight,   bg = 'NONE'            })  -- Strings

  -- Modules
  highlight(0, '@module.hack',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespaces/modules

  -- Attributes
  highlight(0, '@attribute.hack',                  { fg = colors.pink,       bg = 'NONE'            })  -- <<Attribute>>

  -- Operators
  highlight(0, '@operator.hack',                   { fg = colors.white,      bg = 'NONE'            })  -- Operators

  -- Punctuation
  highlight(0, '@punctuation.bracket.hack',        { fg = colors.white,      bg = 'NONE'            })  -- Brackets
  highlight(0, '@punctuation.delimiter.hack',      { fg = colors.white,      bg = 'NONE'            })  -- Delimiters

  -- XHP/Tags
  highlight(0, '@tag.hack',                        { fg = colors.blue,       bg = 'NONE'            })  -- XHP tags
  highlight(0, '@tag.delimiter.hack',              { fg = colors.white,      bg = 'NONE'            })  -- < > />

  -- Comments
  highlight(0, '@comment.hack',                    { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments
  highlight(0, '@comment.documentation.hack',      { fg = colors.gray,       bg = 'NONE', italic = true })  -- Doc comments
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
  highlight(0, '@lsp.type.variable.hack',          { fg = colors.purple,     bg = 'NONE'            })  -- Variables
  highlight(0, '@lsp.type.parameter.hack',         { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.namespace.hack',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Namespaces
  highlight(0, '@lsp.type.string.hack',            { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@lsp.type.number.hack',            { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@lsp.type.keyword.hack',           { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@lsp.type.operator.hack',          { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, '@lsp.type.comment.hack',           { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments
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
