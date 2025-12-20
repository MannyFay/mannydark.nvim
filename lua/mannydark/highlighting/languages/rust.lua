-------------------------------------------------------------------------------
-- Rust Files
-- Highlighting for .rs files.
-------------------------------------------------------------------------------

local colors  = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local rust    = {}


-------------------------------------------------------------------------------
-- Settings

rust.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy)

  -- Keywords
  highlight(0, 'rustKeyword',         { link = "Keyword" })  -- General keywords
  highlight(0, 'rustTypedef',         { link = "Type" })  -- type keyword
  highlight(0, 'rustStructure',       { fg = colors.blue,       bg = 'NONE'            })  -- struct, enum, trait, impl
  highlight(0, 'rustUnion',           { fg = colors.blue,       bg = 'NONE'            })  -- union keyword
  highlight(0, 'rustRepeat',          { fg = colors.blue,       bg = 'NONE'            })  -- loop, while, for
  highlight(0, 'rustConditional',     { link = "Conditional" })  -- if, else, match
  highlight(0, 'rustExternCrate',     { fg = colors.blue,       bg = 'NONE'            })  -- extern crate
  highlight(0, 'rustAsync',           { fg = colors.blue,       bg = 'NONE'            })  -- async keyword
  highlight(0, 'rustAwait',           { fg = colors.blue,       bg = 'NONE'            })  -- await keyword
  highlight(0, 'rustDynKeyword',      { link = "Keyword" })  -- dyn keyword
  highlight(0, 'rustExistential',     { fg = colors.blue,       bg = 'NONE'            })  -- existential keyword
  highlight(0, 'rustSuper',           { fg = colors.blue,       bg = 'NONE'            })  -- super keyword
  highlight(0, 'rustPubScopeCrate',   { fg = colors.blue,       bg = 'NONE'            })  -- pub(crate)

  -- Unsafe
  highlight(0, 'rustUnsafeKeyword',   { link = "Keyword" })  -- unsafe keyword

  -- Types
  highlight(0, 'rustType',            { link = "Type" })  -- Built-in types
  highlight(0, 'rustTrait',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Trait names
  highlight(0, 'rustDeriveTrait',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Traits in derive
  highlight(0, 'rustEnum',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum types
  highlight(0, 'rustEnumVariant',     { fg = colors.purple,     bg = 'NONE'            })  -- Enum variants
  highlight(0, 'rustModPath',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Module paths

  -- Functions and Macros
  highlight(0, 'rustFunction',        { link = "Function" })  -- fn keyword
  highlight(0, 'rustFuncName',        { link = "Function" })  -- Function names
  highlight(0, 'rustFuncCall',        { link = "Function" })  -- Function calls
  highlight(0, 'rustMacro',           { fg = colors.orange,     bg = 'NONE'            })  -- Macro invocations
  highlight(0, 'rustMacroVariable',   { link = "Variable" })  -- $var in macros
  highlight(0, 'rustMacroRepeatDelimiters', { link = "Delimiter" })  -- $()* delimiters
  highlight(0, 'rustAssert',          { fg = colors.orange,     bg = 'NONE'            })  -- assert! macro
  highlight(0, 'rustPanic',           { fg = colors.orange,     bg = 'NONE'            })  -- panic! macro

  -- Variables and Constants
  highlight(0, 'rustIdentifier',      { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'rustCapsIdent',       { fg = colors.purple,     bg = 'NONE'            })  -- SCREAMING_CASE identifiers
  highlight(0, 'rustConstant',        { link = "Constant" })  -- Constants
  highlight(0, 'rustSelf',            { fg = colors.blue,       bg = 'NONE'            })  -- self keyword
  highlight(0, 'rustSigil',           { fg = colors.white,      bg = 'NONE'            })  -- & and * sigils

  -- Storage
  highlight(0, 'rustStorage',         { fg = colors.blue,       bg = 'NONE'            })  -- mut, ref, static, const, move
  highlight(0, 'rustDefault',         { fg = colors.blue,       bg = 'NONE'            })  -- default keyword

  -- Lifetimes
  highlight(0, 'rustLifetime',        { fg = colors.pink,       bg = 'NONE'            })  -- 'a, 'static, etc.
  highlight(0, 'rustLabel',           { fg = colors.pink,       bg = 'NONE'            })  -- 'label: for loops

  -- Strings
  highlight(0, 'rustString',          { link = "String" })  -- "strings"
  highlight(0, 'rustStringDelimiter', { link = "Delimiter" })  -- String delimiters
  highlight(0, 'rustCharacter',       { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' characters
  highlight(0, 'rustEscape',          { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'rustEscapeUnicode',   { fg = colors.pink,       bg = 'NONE'            })  -- \u{XXXX}
  highlight(0, 'rustStringContinuation', { link = "String" })  -- Line continuation

  -- Numbers
  highlight(0, 'rustNumber',          { link = "Number" })  -- Numbers
  highlight(0, 'rustDecNumber',       { link = "Number" })  -- Decimal numbers
  highlight(0, 'rustHexNumber',       { link = "Number" })  -- 0x hex numbers
  highlight(0, 'rustOctNumber',       { link = "Number" })  -- 0o octal numbers
  highlight(0, 'rustBinNumber',       { link = "Number" })  -- 0b binary numbers
  highlight(0, 'rustFloat',           { fg = colors.greenLight, bg = 'NONE'            })  -- Floating point

  -- Booleans
  highlight(0, 'rustBoolean',         { link = "Boolean" })  -- true, false

  -- Operators
  highlight(0, 'rustOperator',        { link = "Operator" })  -- Operators
  highlight(0, 'rustArrowCharacter',  { fg = colors.white,      bg = 'NONE'            })  -- -> and =>
  highlight(0, 'rustQuestionMark',    { fg = colors.white,      bg = 'NONE'            })  -- ? operator
  highlight(0, 'rustModPathSep',      { fg = colors.white,      bg = 'NONE'            })  -- :: separator
  highlight(0, 'rustPubScopeDelim',   { link = "Delimiter" })  -- () in pub()

  -- Attributes
  highlight(0, 'rustAttribute',       { fg = colors.pink,       bg = 'NONE'            })  -- #[attr]
  highlight(0, 'rustDerive',          { fg = colors.pink,       bg = 'NONE'            })  -- #[derive()]
  highlight(0, 'rustAsmOptionsKey',   { fg = colors.pink,       bg = 'NONE'            })  -- asm! options

  -- Comments
  highlight(0, 'rustCommentLine',     { link = "Comment" })  -- // comments
  highlight(0, 'rustCommentBlock',    { link = "Comment" })  -- /* */ comments
  highlight(0, 'rustCommentLineDoc',  { link = "Comment" })  -- /// doc comments
  highlight(0, 'rustCommentBlockDoc', { link = "Comment" })  -- /** */ doc comments
  highlight(0, 'rustCommentLineDocLeader', { link = "Comment" })  -- /// leader
  highlight(0, 'rustCommentBlockDocStar', { link = "Comment" })  -- * in block docs
  highlight(0, 'rustCommentDocCodeFence', { link = "Comment" })  -- ``` in docs
  highlight(0, 'rustShebang',         { fg = colors.red,        bg = 'NONE'            })  -- #! shebang
  highlight(0, 'rustTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME

  -- Errors
  highlight(0, 'rustEscapeError',         { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustCharacterInvalid',    { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustCharacterInvalidUnicode', { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustCommentLineDocError', { link = "Comment" })
  highlight(0, 'rustCommentBlockDocError', { link = "Comment" })
  highlight(0, 'rustReservedKeyword',     { link = "Keyword" })
  highlight(0, 'rustObsoleteStorage',     { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustObsoleteExternMod',   { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.rust)

  -- Variables
  highlight(0, '@variable.rust',              { link = "Variable" })  -- Variables
  highlight(0, '@variable.parameter.rust',    { link = "Variable" })  -- Function parameters
  highlight(0, '@variable.member.rust',       { link = "Variable" })  -- Struct fields
  highlight(0, '@variable.builtin.rust',      { link = "Variable" })  -- self

  -- Constants
  highlight(0, '@constant.rust',              { link = "Constant" })  -- Constants
  highlight(0, '@constant.builtin.rust',      { link = "Constant" })  -- Some, None, Ok, Err

  -- Functions and Macros
  highlight(0, '@function.rust',              { link = "Function" })  -- Function definitions
  highlight(0, '@function.call.rust',         { link = "Function" })  -- Function calls
  highlight(0, '@function.method.rust',       { link = "Function" })  -- Method definitions
  highlight(0, '@function.method.call.rust',  { link = "Function" })  -- Method calls
  highlight(0, '@function.macro.rust',        { link = "Function" })  -- Macro invocations (println!, vec!)
  highlight(0, '@constructor.rust',           { fg = colors.turquoise, bg = 'NONE' })  -- Struct constructors

  -- Types
  highlight(0, '@type.rust',                  { link = "Type" })  -- Type names
  highlight(0, '@type.builtin.rust',          { link = "Type" })  -- Built-in types (i32, String, etc.)
  highlight(0, '@type.definition.rust',       { link = "Type" })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.rust',               { link = "Keyword" })  -- General keywords
  highlight(0, '@keyword.type.rust',          { link = "Keyword" })  -- struct, enum, trait, impl, type
  highlight(0, '@keyword.function.rust',      { link = "Keyword" })  -- fn
  highlight(0, '@keyword.return.rust',        { link = "Keyword" })  -- return
  highlight(0, '@keyword.coroutine.rust',     { link = "Keyword" })  -- async, await
  highlight(0, '@keyword.repeat.rust',        { link = "Keyword" })  -- loop, while, for
  highlight(0, '@keyword.conditional.rust',   { link = "Conditional" })  -- if, else, match
  highlight(0, '@keyword.import.rust',        { link = "Keyword" })  -- use, mod, crate, extern
  highlight(0, '@keyword.operator.rust',      { link = "Operator" })  -- as
  highlight(0, '@keyword.modifier.rust',      { link = "Keyword" })  -- pub, mut, ref, static, const, unsafe
  highlight(0, '@keyword.exception.rust',     { link = "Keyword" })  -- try
  highlight(0, '@keyword.debug.rust',         { link = "Keyword" })  -- assert!, panic!

  -- Strings
  highlight(0, '@string.rust',                { link = "String" })  -- Strings
  highlight(0, '@string.escape.rust',         { link = "String" })  -- Escape sequences
  highlight(0, '@string.regexp.rust',         { link = "String" })  -- Regex patterns
  highlight(0, '@character.rust',             { fg = colors.redLight,  bg = 'NONE' })  -- Characters
  highlight(0, '@character.special.rust',     { fg = colors.pink,      bg = 'NONE' })  -- Escape chars

  -- Numbers
  highlight(0, '@number.rust',                { link = "Number" })  -- Integers
  highlight(0, '@number.float.rust',          { link = "Number" })  -- Floats

  -- Booleans
  highlight(0, '@boolean.rust',               { link = "Boolean" })  -- true, false

  -- Attributes and Lifetimes
  highlight(0, '@attribute.rust',             { fg = colors.pink,      bg = 'NONE' })  -- Lifetimes ('a)
  highlight(0, '@attribute.builtin.rust',     { fg = colors.pink,      bg = 'NONE' })  -- 'static, '_
  highlight(0, '@label.rust',                 { fg = colors.pink,      bg = 'NONE' })  -- 'label: in loops

  -- Modules
  highlight(0, '@module.rust',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names

  -- Comments
  highlight(0, '@comment.rust',               { link = "Comment" })  -- Comments
  highlight(0, '@comment.documentation.rust', { link = "Comment" })  -- Doc comments

  -- Operators and Punctuation
  highlight(0, '@operator.rust',              { link = "Operator" })  -- Operators
  highlight(0, '@punctuation.bracket.rust',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.rust', { link = "Delimiter" })  -- , ; ::
  highlight(0, '@punctuation.special.rust',   { fg = colors.white,     bg = 'NONE' })  -- ! in macros


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.rust)

  highlight(0, '@lsp.type.variable.rust',         { link = "Variable" })  -- Variables
  highlight(0, '@lsp.type.parameter.rust',        { fg = colors.purple,    bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property.rust',         { fg = colors.purple,    bg = 'NONE' })  -- Struct fields
  highlight(0, '@lsp.type.function.rust',         { fg = colors.orange,    bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.method.rust',           { fg = colors.orange,    bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.macro.rust',            { fg = colors.orange,    bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.type.rust',             { fg = colors.turquoise, bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.struct.rust',           { fg = colors.turquoise, bg = 'NONE' })  -- Struct types
  highlight(0, '@lsp.type.enum.rust',             { fg = colors.turquoise, bg = 'NONE' })  -- Enum types
  highlight(0, '@lsp.type.enumMember.rust',       { fg = colors.purple,    bg = 'NONE' })  -- Enum variants
  highlight(0, '@lsp.type.interface.rust',        { fg = colors.turquoise, bg = 'NONE' })  -- Trait types
  highlight(0, '@lsp.type.namespace.rust',        { fg = colors.turquoise, bg = 'NONE' })  -- Modules
  highlight(0, '@lsp.type.typeParameter.rust',    { fg = colors.turquoise, bg = 'NONE' })  -- Generic type params
  highlight(0, '@lsp.type.lifetime.rust',         { fg = colors.pink,      bg = 'NONE' })  -- Lifetimes
  highlight(0, '@lsp.type.decorator.rust',        { fg = colors.pink,      bg = 'NONE' })  -- Attributes
  highlight(0, '@lsp.type.keyword.rust',          { link = "Keyword" })  -- Keywords
  highlight(0, '@lsp.type.operator.rust',         { link = "Operator" })  -- Operators
  highlight(0, '@lsp.type.string.rust',           { link = "String" })  -- Strings
  highlight(0, '@lsp.type.number.rust',           { link = "Number" }) -- Numbers
  highlight(0, '@lsp.type.comment.rust',          { link = "Comment" })  -- Comments
  highlight(0, '@lsp.type.selfKeyword.rust',      { link = "Keyword" })  -- self keyword
  highlight(0, '@lsp.type.selfTypeKeyword.rust',  { link = "Keyword" })  -- Self type

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.mutable.rust',      { link = "Variable" })  -- mut variables
  highlight(0, '@lsp.typemod.variable.constant.rust',     { link = "Variable" })  -- const variables
  highlight(0, '@lsp.typemod.function.declaration.rust',  { fg = colors.orange,    bg = 'NONE' })  -- fn declarations
  highlight(0, '@lsp.typemod.function.unsafe.rust',       { fg = colors.orange,    bg = 'NONE' })  -- unsafe fn
  highlight(0, '@lsp.typemod.type.defaultLibrary.rust',   { fg = colors.turquoise, bg = 'NONE' })  -- std types
  highlight(0, '@lsp.typemod.struct.defaultLibrary.rust', { fg = colors.turquoise, bg = 'NONE' })  -- std structs
  highlight(0, '@lsp.typemod.enum.defaultLibrary.rust',   { fg = colors.turquoise, bg = 'NONE' })  -- std enums
  highlight(0, '@lsp.typemod.macro.defaultLibrary.rust',  { fg = colors.orange,    bg = 'NONE' })  -- std macros
end

return rust
