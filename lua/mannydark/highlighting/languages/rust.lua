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
  highlight(0, 'rustKeyword',         { fg = colors.blue,       bg = 'NONE'            })  -- General keywords
  highlight(0, 'rustTypedef',         { fg = colors.blue,       bg = 'NONE'            })  -- type keyword
  highlight(0, 'rustStructure',       { fg = colors.blue,       bg = 'NONE'            })  -- struct, enum, trait, impl
  highlight(0, 'rustUnion',           { fg = colors.blue,       bg = 'NONE'            })  -- union keyword
  highlight(0, 'rustRepeat',          { fg = colors.blue,       bg = 'NONE'            })  -- loop, while, for
  highlight(0, 'rustConditional',     { fg = colors.blue,       bg = 'NONE'            })  -- if, else, match
  highlight(0, 'rustExternCrate',     { fg = colors.blue,       bg = 'NONE'            })  -- extern crate
  highlight(0, 'rustAsync',           { fg = colors.blue,       bg = 'NONE'            })  -- async keyword
  highlight(0, 'rustAwait',           { fg = colors.blue,       bg = 'NONE'            })  -- await keyword
  highlight(0, 'rustDynKeyword',      { fg = colors.blue,       bg = 'NONE'            })  -- dyn keyword
  highlight(0, 'rustExistential',     { fg = colors.blue,       bg = 'NONE'            })  -- existential keyword
  highlight(0, 'rustSuper',           { fg = colors.blue,       bg = 'NONE'            })  -- super keyword
  highlight(0, 'rustPubScopeCrate',   { fg = colors.blue,       bg = 'NONE'            })  -- pub(crate)

  -- Unsafe
  highlight(0, 'rustUnsafeKeyword',   { fg = colors.red,        bg = 'NONE', bold = true })  -- unsafe keyword

  -- Types
  highlight(0, 'rustType',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Built-in types
  highlight(0, 'rustTrait',           { fg = colors.turquoise,  bg = 'NONE'            })  -- Trait names
  highlight(0, 'rustDeriveTrait',     { fg = colors.turquoise,  bg = 'NONE'            })  -- Traits in derive
  highlight(0, 'rustEnum',            { fg = colors.turquoise,  bg = 'NONE'            })  -- Enum types
  highlight(0, 'rustEnumVariant',     { fg = colors.purple,     bg = 'NONE'            })  -- Enum variants
  highlight(0, 'rustModPath',         { fg = colors.turquoise,  bg = 'NONE'            })  -- Module paths

  -- Functions and Macros
  highlight(0, 'rustFunction',        { fg = colors.orange,     bg = 'NONE'            })  -- fn keyword
  highlight(0, 'rustFuncName',        { fg = colors.orange,     bg = 'NONE'            })  -- Function names
  highlight(0, 'rustFuncCall',        { fg = colors.orange,     bg = 'NONE'            })  -- Function calls
  highlight(0, 'rustMacro',           { fg = colors.orange,     bg = 'NONE'            })  -- Macro invocations
  highlight(0, 'rustMacroVariable',   { fg = colors.purple,     bg = 'NONE'            })  -- $var in macros
  highlight(0, 'rustMacroRepeatDelimiters', { fg = colors.white, bg = 'NONE'           })  -- $()* delimiters
  highlight(0, 'rustAssert',          { fg = colors.orange,     bg = 'NONE'            })  -- assert! macro
  highlight(0, 'rustPanic',           { fg = colors.orange,     bg = 'NONE'            })  -- panic! macro

  -- Variables and Constants
  highlight(0, 'rustIdentifier',      { fg = colors.purple,     bg = 'NONE'            })  -- Identifiers
  highlight(0, 'rustCapsIdent',       { fg = colors.purple,     bg = 'NONE'            })  -- SCREAMING_CASE identifiers
  highlight(0, 'rustConstant',        { fg = colors.purple,     bg = 'NONE'            })  -- Constants
  highlight(0, 'rustSelf',            { fg = colors.blue,       bg = 'NONE'            })  -- self keyword
  highlight(0, 'rustSigil',           { fg = colors.white,      bg = 'NONE'            })  -- & and * sigils

  -- Storage
  highlight(0, 'rustStorage',         { fg = colors.blue,       bg = 'NONE'            })  -- mut, ref, static, const, move
  highlight(0, 'rustDefault',         { fg = colors.blue,       bg = 'NONE'            })  -- default keyword

  -- Lifetimes
  highlight(0, 'rustLifetime',        { fg = colors.pink,       bg = 'NONE'            })  -- 'a, 'static, etc.
  highlight(0, 'rustLabel',           { fg = colors.pink,       bg = 'NONE'            })  -- 'label: for loops

  -- Strings
  highlight(0, 'rustString',          { fg = colors.redLight,   bg = 'NONE'            })  -- "strings"
  highlight(0, 'rustStringDelimiter', { fg = colors.redLight,   bg = 'NONE'            })  -- String delimiters
  highlight(0, 'rustCharacter',       { fg = colors.redLight,   bg = 'NONE'            })  -- 'c' characters
  highlight(0, 'rustEscape',          { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'rustEscapeUnicode',   { fg = colors.pink,       bg = 'NONE'            })  -- \u{XXXX}
  highlight(0, 'rustStringContinuation', { fg = colors.pink,    bg = 'NONE'            })  -- Line continuation

  -- Numbers
  highlight(0, 'rustNumber',          { fg = colors.greenLight, bg = 'NONE'            })  -- Numbers
  highlight(0, 'rustDecNumber',       { fg = colors.greenLight, bg = 'NONE'            })  -- Decimal numbers
  highlight(0, 'rustHexNumber',       { fg = colors.greenLight, bg = 'NONE'            })  -- 0x hex numbers
  highlight(0, 'rustOctNumber',       { fg = colors.greenLight, bg = 'NONE'            })  -- 0o octal numbers
  highlight(0, 'rustBinNumber',       { fg = colors.greenLight, bg = 'NONE'            })  -- 0b binary numbers
  highlight(0, 'rustFloat',           { fg = colors.greenLight, bg = 'NONE'            })  -- Floating point

  -- Booleans
  highlight(0, 'rustBoolean',         { fg = colors.blue,       bg = 'NONE'            })  -- true, false

  -- Operators
  highlight(0, 'rustOperator',        { fg = colors.white,      bg = 'NONE'            })  -- Operators
  highlight(0, 'rustArrowCharacter',  { fg = colors.white,      bg = 'NONE'            })  -- -> and =>
  highlight(0, 'rustQuestionMark',    { fg = colors.white,      bg = 'NONE'            })  -- ? operator
  highlight(0, 'rustModPathSep',      { fg = colors.white,      bg = 'NONE'            })  -- :: separator
  highlight(0, 'rustPubScopeDelim',   { fg = colors.white,      bg = 'NONE'            })  -- () in pub()

  -- Attributes
  highlight(0, 'rustAttribute',       { fg = colors.pink,       bg = 'NONE'            })  -- #[attr]
  highlight(0, 'rustDerive',          { fg = colors.pink,       bg = 'NONE'            })  -- #[derive()]
  highlight(0, 'rustAsmOptionsKey',   { fg = colors.pink,       bg = 'NONE'            })  -- asm! options

  -- Comments
  highlight(0, 'rustCommentLine',     { fg = colors.red,        bg = 'NONE'            })  -- // comments
  highlight(0, 'rustCommentBlock',    { fg = colors.red,        bg = 'NONE'            })  -- /* */ comments
  highlight(0, 'rustCommentLineDoc',  { fg = colors.red,        bg = 'NONE'            })  -- /// doc comments
  highlight(0, 'rustCommentBlockDoc', { fg = colors.red,        bg = 'NONE'            })  -- /** */ doc comments
  highlight(0, 'rustCommentLineDocLeader', { fg = colors.red,   bg = 'NONE'            })  -- /// leader
  highlight(0, 'rustCommentBlockDocStar', { fg = colors.red,    bg = 'NONE'            })  -- * in block docs
  highlight(0, 'rustCommentDocCodeFence', { fg = colors.gray,   bg = 'NONE'            })  -- ``` in docs
  highlight(0, 'rustShebang',         { fg = colors.red,        bg = 'NONE'            })  -- #! shebang
  highlight(0, 'rustTodo',            { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME

  -- Errors
  highlight(0, 'rustEscapeError',         { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustCharacterInvalid',    { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustCharacterInvalidUnicode', { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustCommentLineDocError', { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustCommentBlockDocError', { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustReservedKeyword',     { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustObsoleteStorage',     { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })
  highlight(0, 'rustObsoleteExternMod',   { fg = 'NONE', bg = 'NONE', sp = colors.red, undercurl = true })


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.rust)

  -- Variables
  highlight(0, '@variable.rust',              { fg = colors.purple,    bg = 'NONE' })  -- Variables
  highlight(0, '@variable.parameter.rust',    { fg = colors.purple,    bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.member.rust',       { fg = colors.purple,    bg = 'NONE' })  -- Struct fields
  highlight(0, '@variable.builtin.rust',      { fg = colors.blue,      bg = 'NONE' })  -- self

  -- Constants
  highlight(0, '@constant.rust',              { fg = colors.purple,    bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin.rust',      { fg = colors.purple,    bg = 'NONE' })  -- Some, None, Ok, Err

  -- Functions and Macros
  highlight(0, '@function.rust',              { fg = colors.orange,    bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.call.rust',         { fg = colors.orange,    bg = 'NONE' })  -- Function calls
  highlight(0, '@function.method.rust',       { fg = colors.orange,    bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call.rust',  { fg = colors.orange,    bg = 'NONE' })  -- Method calls
  highlight(0, '@function.macro.rust',        { fg = colors.orange,    bg = 'NONE' })  -- Macro invocations (println!, vec!)
  highlight(0, '@constructor.rust',           { fg = colors.turquoise, bg = 'NONE' })  -- Struct constructors

  -- Types
  highlight(0, '@type.rust',                  { fg = colors.turquoise, bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin.rust',          { fg = colors.turquoise, bg = 'NONE' })  -- Built-in types (i32, String, etc.)
  highlight(0, '@type.definition.rust',       { fg = colors.turquoise, bg = 'NONE' })  -- Type definitions

  -- Keywords
  highlight(0, '@keyword.rust',               { fg = colors.blue,      bg = 'NONE' })  -- General keywords
  highlight(0, '@keyword.type.rust',          { fg = colors.blue,      bg = 'NONE' })  -- struct, enum, trait, impl, type
  highlight(0, '@keyword.function.rust',      { fg = colors.blue,      bg = 'NONE' })  -- fn
  highlight(0, '@keyword.return.rust',        { fg = colors.blue,      bg = 'NONE' })  -- return
  highlight(0, '@keyword.coroutine.rust',     { fg = colors.blue,      bg = 'NONE' })  -- async, await
  highlight(0, '@keyword.repeat.rust',        { fg = colors.blue,      bg = 'NONE' })  -- loop, while, for
  highlight(0, '@keyword.conditional.rust',   { fg = colors.blue,      bg = 'NONE' })  -- if, else, match
  highlight(0, '@keyword.import.rust',        { fg = colors.blue,      bg = 'NONE' })  -- use, mod, crate, extern
  highlight(0, '@keyword.operator.rust',      { fg = colors.blue,      bg = 'NONE' })  -- as
  highlight(0, '@keyword.modifier.rust',      { fg = colors.blue,      bg = 'NONE' })  -- pub, mut, ref, static, const, unsafe
  highlight(0, '@keyword.exception.rust',     { fg = colors.blue,      bg = 'NONE' })  -- try
  highlight(0, '@keyword.debug.rust',         { fg = colors.orange,    bg = 'NONE' })  -- assert!, panic!

  -- Strings
  highlight(0, '@string.rust',                { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@string.escape.rust',         { fg = colors.pink,      bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.regexp.rust',         { fg = colors.redLight,  bg = 'NONE' })  -- Regex patterns
  highlight(0, '@character.rust',             { fg = colors.redLight,  bg = 'NONE' })  -- Characters
  highlight(0, '@character.special.rust',     { fg = colors.pink,      bg = 'NONE' })  -- Escape chars

  -- Numbers
  highlight(0, '@number.rust',                { fg = colors.greenLight, bg = 'NONE' })  -- Integers
  highlight(0, '@number.float.rust',          { fg = colors.greenLight, bg = 'NONE' })  -- Floats

  -- Booleans
  highlight(0, '@boolean.rust',               { fg = colors.blue,      bg = 'NONE' })  -- true, false

  -- Attributes and Lifetimes
  highlight(0, '@attribute.rust',             { fg = colors.pink,      bg = 'NONE' })  -- Lifetimes ('a)
  highlight(0, '@attribute.builtin.rust',     { fg = colors.pink,      bg = 'NONE' })  -- 'static, '_
  highlight(0, '@label.rust',                 { fg = colors.pink,      bg = 'NONE' })  -- 'label: in loops

  -- Modules
  highlight(0, '@module.rust',                { fg = colors.turquoise, bg = 'NONE' })  -- Module names

  -- Comments
  highlight(0, '@comment.rust',               { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation.rust', { fg = colors.red,       bg = 'NONE' })  -- Doc comments

  -- Operators and Punctuation
  highlight(0, '@operator.rust',              { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@punctuation.bracket.rust',   { fg = colors.white,     bg = 'NONE' })  -- (), [], {}, <>
  highlight(0, '@punctuation.delimiter.rust', { fg = colors.white,     bg = 'NONE' })  -- , ; ::
  highlight(0, '@punctuation.special.rust',   { fg = colors.white,     bg = 'NONE' })  -- ! in macros


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.rust)

  highlight(0, '@lsp.type.variable.rust',         { fg = colors.purple,    bg = 'NONE' })  -- Variables
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
  highlight(0, '@lsp.type.keyword.rust',          { fg = colors.blue,      bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.operator.rust',         { fg = colors.white,     bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.string.rust',           { fg = colors.redLight,  bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.number.rust',           { fg = colors.greenLight, bg = 'NONE' }) -- Numbers
  highlight(0, '@lsp.type.comment.rust',          { fg = colors.red,       bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.selfKeyword.rust',      { fg = colors.blue,      bg = 'NONE' })  -- self keyword
  highlight(0, '@lsp.type.selfTypeKeyword.rust',  { fg = colors.turquoise, bg = 'NONE' })  -- Self type

  -- LSP Modifiers
  highlight(0, '@lsp.typemod.variable.mutable.rust',      { fg = colors.purple,    bg = 'NONE' })  -- mut variables
  highlight(0, '@lsp.typemod.variable.constant.rust',     { fg = colors.purple,    bg = 'NONE' })  -- const variables
  highlight(0, '@lsp.typemod.function.declaration.rust',  { fg = colors.orange,    bg = 'NONE' })  -- fn declarations
  highlight(0, '@lsp.typemod.function.unsafe.rust',       { fg = colors.orange,    bg = 'NONE' })  -- unsafe fn
  highlight(0, '@lsp.typemod.type.defaultLibrary.rust',   { fg = colors.turquoise, bg = 'NONE' })  -- std types
  highlight(0, '@lsp.typemod.struct.defaultLibrary.rust', { fg = colors.turquoise, bg = 'NONE' })  -- std structs
  highlight(0, '@lsp.typemod.enum.defaultLibrary.rust',   { fg = colors.turquoise, bg = 'NONE' })  -- std enums
  highlight(0, '@lsp.typemod.macro.defaultLibrary.rust',  { fg = colors.orange,    bg = 'NONE' })  -- std macros
end

return rust
