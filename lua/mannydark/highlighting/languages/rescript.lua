-------------------------------------------------------------------------------
-- ReScript
-------------------------------------------------------------------------------

local colors   = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local rescript = {}


-------------------------------------------------------------------------------
-- Settings

rescript.setupHighlighting = function()


  -------------------------------------------------------------------------
  -- Vim ReScript Syntax Groups (vim-rescript)
  -------------------------------------------------------------------------

  -- Keywords
  highlight(0, 'resKeyword',                  { fg = colors.blue,       bg = 'NONE'            })  -- let, if, switch, async, await, rec, and, as, external, mutable, lazy, private, constraint
  highlight(0, 'resConditional',              { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch
  highlight(0, 'resRepeat',                   { fg = colors.blue,       bg = 'NONE'            })  -- for, while, in
  highlight(0, 'resException',                { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, exception, raise, assert
  highlight(0, 'resInclude',                  { fg = colors.blue,       bg = 'NONE'            })  -- open, include
  highlight(0, 'resAsync',                    { fg = colors.blue,       bg = 'NONE'            })  -- async, await
  highlight(0, 'resLet',                      { fg = colors.blue,       bg = 'NONE'            })  -- let
  highlight(0, 'resRec',                      { fg = colors.blue,       bg = 'NONE'            })  -- rec
  highlight(0, 'resMutable',                  { fg = colors.blue,       bg = 'NONE'            })  -- mutable
  highlight(0, 'resExternal',                 { fg = colors.blue,       bg = 'NONE'            })  -- external
  highlight(0, 'resPrivate',                  { fg = colors.blue,       bg = 'NONE'            })  -- private
  highlight(0, 'resLazy',                     { fg = colors.blue,       bg = 'NONE'            })  -- lazy
  highlight(0, 'resConstraint',               { fg = colors.blue,       bg = 'NONE'            })  -- constraint

  -- Types
  highlight(0, 'resType',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in types: bool, int, float, string, char, unit, array, list, option, result, promise
  highlight(0, 'resTypeKeyword',              { fg = colors.blue,       bg = 'NONE'            })  -- type keyword
  highlight(0, 'resTypeName',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Type names
  highlight(0, 'resTypeAnnotation',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Type annotations
  highlight(0, 'resTypeParameter',            { fg = colors.purple,     bg = 'NONE'            })  -- Type parameters ('a, 'b)
  highlight(0, 'resTypeVar',                  { fg = colors.purple,     bg = 'NONE'            })  -- Type variables

  -- Built-in Types
  highlight(0, 'resBool',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- bool
  highlight(0, 'resInt',                      { fg = colors.turquoise,  bg = 'NONE'            })  -- int
  highlight(0, 'resFloatType',                { fg = colors.turquoise,  bg = 'NONE'            })  -- float
  highlight(0, 'resStringType',               { fg = colors.turquoise,  bg = 'NONE'            })  -- string
  highlight(0, 'resCharType',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- char
  highlight(0, 'resUnit',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- unit
  highlight(0, 'resArray',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- array
  highlight(0, 'resList',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- list
  highlight(0, 'resOption',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- option
  highlight(0, 'resResult',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- result
  highlight(0, 'resPromise',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- promise
  highlight(0, 'resRef',                      { fg = colors.turquoise,  bg = 'NONE'            })  -- ref

  -- Modules and Variants
  highlight(0, 'resModuleOrVariant',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Capitalized identifiers (modules/constructors)
  highlight(0, 'resModuleChain',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Module path notation (Module.SubModule)
  highlight(0, 'resModule',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- Module names
  highlight(0, 'resModuleKeyword',            { fg = colors.blue,       bg = 'NONE'            })  -- module keyword
  highlight(0, 'resModuleAccess',             { fg = colors.turquoise,  bg = 'NONE'            })  -- Module access

  -- Constructors and Variants
  highlight(0, 'resConstructor',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Data constructors
  highlight(0, 'resVariant',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Variant constructors
  highlight(0, 'resPolyVariant',              { fg = colors.turquoise,  bg = 'NONE'            })  -- Polymorphic variants (#variant)
  highlight(0, 'resSome',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- Some constructor
  highlight(0, 'resNone',                     { fg = colors.turquoise,  bg = 'NONE'            })  -- None constructor
  highlight(0, 'resOk',                       { fg = colors.turquoise,  bg = 'NONE'            })  -- Ok constructor
  highlight(0, 'resError',                    { fg = colors.turquoise,  bg = 'NONE'            })  -- Error constructor

  -- Functions
  highlight(0, 'resFunction',                 { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'resFunctionCall',             { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'resFunctionDef',              { fg = colors.orange,     bg = 'NONE'            })  -- Function definitions
  highlight(0, 'resMethod',                   { fg = colors.orange,     bg = 'NONE'            })  -- Method calls
  highlight(0, 'resLambda',                   { fg = colors.blue,       bg = 'NONE'            })  -- Lambda expressions

  -- Variables and Identifiers
  highlight(0, 'resIdentifier',               { fg = colors.white,      bg = 'NONE'            })  -- Identifiers
  highlight(0, 'resVariable',                 { fg = colors.white,      bg = 'NONE'            })  -- Variables
  highlight(0, 'resParameter',                { fg = colors.white,      bg = 'NONE'            })  -- Function parameters
  highlight(0, 'resProperty',                 { fg = colors.blue,       bg = 'NONE'            })  -- Record/object properties
  highlight(0, 'resField',                    { fg = colors.blue,       bg = 'NONE'            })  -- Record fields
  highlight(0, 'resLabel',                    { fg = colors.blue,       bg = 'NONE'            })  -- Labeled arguments (~label)
  highlight(0, 'resLabeledArg',               { fg = colors.blue,       bg = 'NONE'            })  -- Labeled arguments

  -- Operators
  highlight(0, 'resOperator',                 { fg = colors.white,      bg = 'NONE'            })  -- Standard operators
  highlight(0, 'resArrowPipe',                { fg = colors.blue,       bg = 'NONE'            })  -- =>, ->, |>
  highlight(0, 'resArrow',                    { fg = colors.blue,       bg = 'NONE'            })  -- => (fat arrow)
  highlight(0, 'resThinArrow',                { fg = colors.blue,       bg = 'NONE'            })  -- -> (thin arrow)
  highlight(0, 'resPipe',                     { fg = colors.blue,       bg = 'NONE'            })  -- |> (pipe)
  highlight(0, 'resDelimiter',                { fg = colors.white,      bg = 'NONE'            })  -- | (pipe delimiter)
  highlight(0, 'resEquals',                   { fg = colors.white,      bg = 'NONE'            })  -- = (assignment)
  highlight(0, 'resDoubleColon',              { fg = colors.white,      bg = 'NONE'            })  -- :: (list cons)
  highlight(0, 'resSpread',                   { fg = colors.white,      bg = 'NONE'            })  -- ... (spread)
  highlight(0, 'resCustomOperator',           { fg = colors.white,      bg = 'NONE'            })  -- Custom operators
  highlight(0, 'resMathOperator',             { fg = colors.white,      bg = 'NONE'            })  -- + - * / mod
  highlight(0, 'resCompareOperator',          { fg = colors.white,      bg = 'NONE'            })  -- == != < > <= >= === !==
  highlight(0, 'resBoolOperator',             { fg = colors.white,      bg = 'NONE'            })  -- && || !
  highlight(0, 'resRef',                      { fg = colors.white,      bg = 'NONE'            })  -- := (ref assignment)
  highlight(0, 'resDeref',                    { fg = colors.white,      bg = 'NONE'            })  -- ^ (dereference)

  -- Literals
  highlight(0, 'resBoolean',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- true, false
  highlight(0, 'resNumber',                   { fg = colors.greenLight, bg = 'NONE'            })  -- Integer literals
  highlight(0, 'resFloat',                    { fg = colors.greenLight, bg = 'NONE'            })  -- Float literals
  highlight(0, 'resHexNumber',                { fg = colors.greenLight, bg = 'NONE'            })  -- Hexadecimal: 0x1A
  highlight(0, 'resOctalNumber',              { fg = colors.greenLight, bg = 'NONE'            })  -- Octal: 0o17
  highlight(0, 'resBinaryNumber',             { fg = colors.greenLight, bg = 'NONE'            })  -- Binary: 0b1010

  -- Strings and Characters
  highlight(0, 'resString',                   { fg = colors.redLight,   bg = 'NONE'            })  -- Double-quoted strings
  highlight(0, 'resChar',                     { fg = colors.redLight,   bg = 'NONE'            })  -- Character literals
  highlight(0, 'resStringEscapeSeq',          { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences: \n, \t, etc.
  highlight(0, 'resUnicodeChar',              { fg = colors.pink,       bg = 'NONE'            })  -- Unicode escapes: \u{...}
  highlight(0, 'resTemplateString',           { fg = colors.redLight,   bg = 'NONE'            })  -- Template strings (backticks)
  highlight(0, 'resInterpolation',            { fg = colors.pink,       bg = 'NONE'            })  -- String interpolation
  highlight(0, 'resInterpolationVariable',    { fg = colors.white,      bg = 'NONE'            })  -- Interpolation variables
  highlight(0, 'resInterpolationBlock',       { fg = colors.pink,       bg = 'NONE'            })  -- ${...} interpolation blocks
  highlight(0, 'resInterpolatedStringEscapeSeq', { fg = colors.pink,    bg = 'NONE'            })  -- Backtick string escapes

  -- Comments
  highlight(0, 'resSingleLineComment',        { fg = colors.gray,       bg = 'NONE', italic = true })  -- // comments
  highlight(0, 'resMultiLineComment',         { fg = colors.gray,       bg = 'NONE', italic = true })  -- /* */ comments
  highlight(0, 'resComment',                  { fg = colors.gray,       bg = 'NONE', italic = true })  -- Generic comments
  highlight(0, 'resDocComment',               { fg = colors.gray,       bg = 'NONE', italic = true })  -- Documentation comments
  highlight(0, 'resTodo',                     { fg = colors.pink,       bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Attributes and Extensions
  highlight(0, 'resAttribute',                { fg = colors.pink,       bg = 'NONE'            })  -- @ and @@ attributes
  highlight(0, 'resExtension',                { fg = colors.pink,       bg = 'NONE'            })  -- % and %% extensions
  highlight(0, 'resDecorator',                { fg = colors.pink,       bg = 'NONE'            })  -- Decorators
  highlight(0, 'resDirective',                { fg = colors.pink,       bg = 'NONE'            })  -- Directives

  -- Common Attributes
  highlight(0, 'resAttrGet',                  { fg = colors.pink,       bg = 'NONE'            })  -- @get
  highlight(0, 'resAttrSet',                  { fg = colors.pink,       bg = 'NONE'            })  -- @set
  highlight(0, 'resAttrSend',                 { fg = colors.pink,       bg = 'NONE'            })  -- @send
  highlight(0, 'resAttrNew',                  { fg = colors.pink,       bg = 'NONE'            })  -- @new
  highlight(0, 'resAttrVal',                  { fg = colors.pink,       bg = 'NONE'            })  -- @val
  highlight(0, 'resAttrModule',               { fg = colors.pink,       bg = 'NONE'            })  -- @module
  highlight(0, 'resAttrScope',                { fg = colors.pink,       bg = 'NONE'            })  -- @scope
  highlight(0, 'resAttrVariadic',             { fg = colors.pink,       bg = 'NONE'            })  -- @variadic
  highlight(0, 'resAttrReturn',               { fg = colors.pink,       bg = 'NONE'            })  -- @return
  highlight(0, 'resAttrOptional',             { fg = colors.pink,       bg = 'NONE'            })  -- @optional
  highlight(0, 'resAttrAs',                   { fg = colors.pink,       bg = 'NONE'            })  -- @as
  highlight(0, 'resAttrUnboxed',              { fg = colors.pink,       bg = 'NONE'            })  -- @unboxed
  highlight(0, 'resAttrInline',               { fg = colors.pink,       bg = 'NONE'            })  -- @inline
  highlight(0, 'resAttrReact',                { fg = colors.pink,       bg = 'NONE'            })  -- @react.component
  highlight(0, 'resAttrGenType',              { fg = colors.pink,       bg = 'NONE'            })  -- @genType

  -- Punctuation and Delimiters
  highlight(0, 'resPunctuation',              { fg = colors.white,      bg = 'NONE'            })  -- General punctuation
  highlight(0, 'resParen',                    { fg = colors.white,      bg = 'NONE'            })  -- Parentheses
  highlight(0, 'resBracket',                  { fg = colors.white,      bg = 'NONE'            })  -- Square brackets
  highlight(0, 'resBrace',                    { fg = colors.white,      bg = 'NONE'            })  -- Curly braces
  highlight(0, 'resEncl',                     { fg = colors.white,      bg = 'NONE'            })  -- Matched enclosing brackets
  highlight(0, 'resComma',                    { fg = colors.white,      bg = 'NONE'            })  -- Commas
  highlight(0, 'resSemicolon',                { fg = colors.white,      bg = 'NONE'            })  -- Semicolons
  highlight(0, 'resColon',                    { fg = colors.white,      bg = 'NONE'            })  -- Colons
  highlight(0, 'resDot',                      { fg = colors.white,      bg = 'NONE'            })  -- Dots

  -- Records and Objects
  highlight(0, 'resRecord',                   { fg = colors.white,      bg = 'NONE'            })  -- Record literals
  highlight(0, 'resRecordField',              { fg = colors.blue,       bg = 'NONE'            })  -- Record field names
  highlight(0, 'resRecordPun',                { fg = colors.blue,       bg = 'NONE'            })  -- Record field punning
  highlight(0, 'resObject',                   { fg = colors.white,      bg = 'NONE'            })  -- Object literals
  highlight(0, 'resObjectField',              { fg = colors.blue,       bg = 'NONE'            })  -- Object field names

  -- Pattern Matching
  highlight(0, 'resSwitch',                   { fg = colors.blue,       bg = 'NONE'            })  -- switch keyword
  highlight(0, 'resPattern',                  { fg = colors.white,      bg = 'NONE'            })  -- Pattern expressions
  highlight(0, 'resPatternConstructor',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructor in pattern
  highlight(0, 'resWildcard',                 { fg = colors.white,      bg = 'NONE'            })  -- _ wildcard
  highlight(0, 'resOr',                       { fg = colors.white,      bg = 'NONE'            })  -- | in patterns
  highlight(0, 'resWhen',                     { fg = colors.blue,       bg = 'NONE'            })  -- when guard

  -- JSX (ReScript has built-in JSX support)
  highlight(0, 'resJsxTag',                   { fg = colors.blue,       bg = 'NONE'            })  -- JSX tag names
  highlight(0, 'resJsxTagName',               { fg = colors.blue,       bg = 'NONE'            })  -- JSX tag name
  highlight(0, 'resJsxComponent',             { fg = colors.turquoise,  bg = 'NONE'            })  -- JSX component (capitalized)
  highlight(0, 'resJsxAttribute',             { fg = colors.turquoise,  bg = 'NONE'            })  -- JSX attributes
  highlight(0, 'resJsxAttributeValue',        { fg = colors.redLight,   bg = 'NONE'            })  -- JSX attribute values
  highlight(0, 'resJsxText',                  { fg = colors.white,      bg = 'NONE'            })  -- JSX text content
  highlight(0, 'resJsxExpr',                  { fg = colors.white,      bg = 'NONE'            })  -- JSX expressions {expr}
  highlight(0, 'resJsxBracket',               { fg = colors.white,      bg = 'NONE'            })  -- JSX brackets < > />
  highlight(0, 'resJsxSpread',                { fg = colors.white,      bg = 'NONE'            })  -- JSX spread {...props}
  highlight(0, 'resJsxFragment',              { fg = colors.white,      bg = 'NONE'            })  -- JSX fragment <> </>

  -- Errors
  highlight(0, 'resBraceErr',                 { fg = colors.white,      bg = colors.red        })  -- Unmatched brace
  highlight(0, 'resBrackErr',                 { fg = colors.white,      bg = colors.red        })  -- Unmatched bracket
  highlight(0, 'resParenErr',                 { fg = colors.white,      bg = colors.red        })  -- Unmatched paren
  highlight(0, 'resArrErr',                   { fg = colors.white,      bg = colors.red        })  -- Unmatched array bracket
  highlight(0, 'resError',                    { fg = colors.white,      bg = colors.red        })  -- Generic error

  -- Built-in Functions and Values
  highlight(0, 'resBuiltinFunction',          { fg = colors.orange,     bg = 'NONE'            })  -- Built-in functions
  highlight(0, 'resConsole',                  { fg = colors.turquoise,  bg = 'NONE'            })  -- Js.Console
  highlight(0, 'resPromiseFunc',              { fg = colors.orange,     bg = 'NONE'            })  -- Promise functions
  highlight(0, 'resArrayFunc',                { fg = colors.orange,     bg = 'NONE'            })  -- Array functions
  highlight(0, 'resStringFunc',               { fg = colors.orange,     bg = 'NONE'            })  -- String functions
  highlight(0, 'resMathFunc',                 { fg = colors.orange,     bg = 'NONE'            })  -- Math functions


  -------------------------------------------------------------------------
  -- Treesitter ReScript Captures
  -------------------------------------------------------------------------

  -- Comments
  highlight(0, '@comment.rescript',                { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments
  highlight(0, '@spell.rescript',                  { link = '@comment.rescript'                         })  -- Spell check

  -- Constants
  highlight(0, '@constant.macro.rescript',         { fg = colors.pink,       bg = 'NONE'            })  -- Macro constants
  highlight(0, '@constant.builtin.rescript',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in constants
  highlight(0, '@boolean.rescript',                { fg = colors.turquoise,  bg = 'NONE'            })  -- Booleans

  -- Variables
  highlight(0, '@variable.rescript',               { fg = colors.white,      bg = 'NONE'            })  -- Variables
  highlight(0, '@variable.member.rescript',        { fg = colors.blue,       bg = 'NONE'            })  -- Record/object fields
  highlight(0, '@variable.parameter.rescript',     { fg = colors.white,      bg = 'NONE'            })  -- Parameters

  -- Types
  highlight(0, '@type.rescript',                   { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@type.builtin.rescript',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in types
  highlight(0, '@constructor.rescript',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructors

  -- Properties and Modules
  highlight(0, '@property.rescript',               { fg = colors.blue,       bg = 'NONE'            })  -- Properties
  highlight(0, '@module.rescript',                 { fg = colors.turquoise,  bg = 'NONE'            })  -- Modules

  -- Strings and Characters
  highlight(0, '@string.rescript',                 { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@string.escape.rescript',          { fg = colors.pink,       bg = 'NONE'            })  -- Escape sequences
  highlight(0, '@character.rescript',              { fg = colors.redLight,   bg = 'NONE'            })  -- Characters

  -- Numbers
  highlight(0, '@number.rescript',                 { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers

  -- Functions
  highlight(0, '@function.rescript',               { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@function.call.rescript',          { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, '@function.method.call.rescript',   { fg = colors.orange,     bg = 'NONE'            })  -- Method calls

  -- Attributes
  highlight(0, '@attribute.rescript',              { fg = colors.pink,       bg = 'NONE'            })  -- Attributes

  -- Keywords
  highlight(0, '@keyword.rescript',                { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@keyword.import.rescript',         { fg = colors.blue,       bg = 'NONE'            })  -- open, include
  highlight(0, '@keyword.modifier.rescript',       { fg = colors.blue,       bg = 'NONE'            })  -- rec, mutable, private
  highlight(0, '@keyword.type.rescript',           { fg = colors.blue,       bg = 'NONE'            })  -- type
  highlight(0, '@keyword.operator.rescript',       { fg = colors.blue,       bg = 'NONE'            })  -- Operator keywords
  highlight(0, '@keyword.coroutine.rescript',      { fg = colors.blue,       bg = 'NONE'            })  -- async, await
  highlight(0, '@keyword.conditional.rescript',    { fg = colors.blue,       bg = 'NONE'            })  -- if, else, switch
  highlight(0, '@keyword.conditional.ternary.rescript', { fg = colors.blue,  bg = 'NONE'            })  -- Ternary operator
  highlight(0, '@keyword.exception.rescript',      { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, exception
  highlight(0, '@keyword.repeat.rescript',         { fg = colors.blue,       bg = 'NONE'            })  -- for, while

  -- Operators and Punctuation
  highlight(0, '@operator.rescript',               { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, '@punctuation.delimiter.rescript',  { fg = colors.white,      bg = 'NONE'            })  -- Delimiters
  highlight(0, '@punctuation.bracket.rescript',    { fg = colors.white,      bg = 'NONE'            })  -- Brackets
  highlight(0, '@punctuation.special.rescript',    { fg = colors.pink,       bg = 'NONE'            })  -- Special punctuation

  -- JSX
  highlight(0, '@tag.rescript',                    { fg = colors.blue,       bg = 'NONE'            })  -- JSX tags
  highlight(0, '@tag.delimiter.rescript',          { fg = colors.white,      bg = 'NONE'            })  -- JSX tag delimiters
  highlight(0, '@tag.attribute.rescript',          { fg = colors.turquoise,  bg = 'NONE'            })  -- JSX attributes

  -- None/Other
  highlight(0, '@none.rescript',                   { fg = colors.white,      bg = 'NONE'            })  -- Uncategorized


  -------------------------------------------------------------------------
  -- LSP Semantic Tokens
  -------------------------------------------------------------------------

  highlight(0, '@lsp.type.type.rescript',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Types
  highlight(0, '@lsp.type.class.rescript',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Modules (classes)
  highlight(0, '@lsp.type.enum.rescript',          { fg = colors.turquoise,  bg = 'NONE'            })  -- Variant types
  highlight(0, '@lsp.type.enumMember.rescript',    { fg = colors.turquoise,  bg = 'NONE'            })  -- Constructors
  highlight(0, '@lsp.type.function.rescript',      { fg = colors.orange,     bg = 'NONE'            })  -- Functions
  highlight(0, '@lsp.type.method.rescript',        { fg = colors.orange,     bg = 'NONE'            })  -- Methods
  highlight(0, '@lsp.type.property.rescript',      { fg = colors.blue,       bg = 'NONE'            })  -- Properties
  highlight(0, '@lsp.type.variable.rescript',      { fg = colors.white,      bg = 'NONE'            })  -- Variables
  highlight(0, '@lsp.type.parameter.rescript',     { fg = colors.white,      bg = 'NONE'            })  -- Parameters
  highlight(0, '@lsp.type.typeParameter.rescript', { fg = colors.purple,     bg = 'NONE'            })  -- Type parameters
  highlight(0, '@lsp.type.namespace.rescript',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Modules
  highlight(0, '@lsp.type.decorator.rescript',     { fg = colors.pink,       bg = 'NONE'            })  -- Attributes/decorators
  highlight(0, '@lsp.type.string.rescript',        { fg = colors.redLight,   bg = 'NONE'            })  -- Strings
  highlight(0, '@lsp.type.number.rescript',        { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, '@lsp.type.keyword.rescript',       { fg = colors.blue,       bg = 'NONE'            })  -- Keywords
  highlight(0, '@lsp.type.operator.rescript',      { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, '@lsp.type.comment.rescript',       { fg = colors.gray,       bg = 'NONE', italic = true })  -- Comments

  highlight(0, '@lsp.mod.declaration.rescript',    { fg = colors.orange,     bg = 'NONE'            })  -- Declarations
  highlight(0, '@lsp.mod.definition.rescript',     { fg = colors.orange,     bg = 'NONE'            })  -- Definitions
  highlight(0, '@lsp.mod.readonly.rescript',       { fg = colors.turquoise,  bg = 'NONE'            })  -- Immutable values
  highlight(0, '@lsp.mod.async.rescript',          { fg = colors.blue,       bg = 'NONE'            })  -- Async functions
  highlight(0, '@lsp.mod.defaultLibrary.rescript', { fg = colors.turquoise,  bg = 'NONE'            })  -- Standard library

end

return rescript
