-------------------------------------------------------------------------------
-- Language Defaults
-- Default/fallback highlighting for all languages.
-- These groups are used when no specific language highlighting is defined.
-------------------------------------------------------------------------------

local colors           = require('mannydark.palette')
local highlight        = vim.api.nvim_set_hl
local languageDefaults = {}


-------------------------------------------------------------------------------
-- Settings

languageDefaults.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Standard Vim Syntax Groups
  -- These are the fundamental groups that language syntaxes link to

  -- Comments
  highlight(0, 'Comment',        { fg = colors.red,        bg = 'NONE'            })  -- Any comment

  -- Constants
  highlight(0, 'Constant',       { fg = colors.purple,     bg = 'NONE'            })  -- Any constant
  highlight(0, 'String',         { fg = colors.redLight,   bg = 'NONE'            })  -- String literals
  highlight(0, 'Character',      { fg = colors.redLight,   bg = 'NONE'            })  -- Character literals 'c'
  highlight(0, 'Number',         { fg = colors.greenLight, bg = 'NONE'            })  -- Numeric literals
  highlight(0, 'Float',          { fg = colors.greenLight, bg = 'NONE'            })  -- Floating point
  highlight(0, 'Boolean',        { fg = colors.blue,       bg = 'NONE'            })  -- true, false

  -- Identifiers
  highlight(0, 'Identifier',     { fg = colors.white,      bg = 'NONE'            })  -- Variable names
  highlight(0, 'Function',       { fg = colors.orange,     bg = 'NONE'            })  -- Function names

  -- Statements
  highlight(0, 'Statement',      { fg = colors.blue,       bg = 'NONE'            })  -- Any statement
  highlight(0, 'Conditional',    { fg = colors.blue,       bg = 'NONE'            })  -- if, then, else, switch
  highlight(0, 'Repeat',         { fg = colors.blue,       bg = 'NONE'            })  -- for, while, do
  highlight(0, 'Label',          { fg = colors.blue,       bg = 'NONE'            })  -- case, default, goto labels
  highlight(0, 'Operator',       { fg = colors.white,      bg = 'NONE'            })  -- +, -, *, /, =, etc.
  highlight(0, 'Keyword',        { fg = colors.blue,       bg = 'NONE'            })  -- Any keyword
  highlight(0, 'Exception',      { fg = colors.blue,       bg = 'NONE'            })  -- try, catch, throw

  -- Preprocessor
  highlight(0, 'PreProc',        { fg = colors.pink,       bg = 'NONE'            })  -- Generic preprocessor
  highlight(0, 'Include',        { fg = colors.pink,       bg = 'NONE'            })  -- #include, import
  highlight(0, 'Define',         { fg = colors.pink,       bg = 'NONE'            })  -- #define
  highlight(0, 'Macro',          { fg = colors.pink,       bg = 'NONE'            })  -- Macros
  highlight(0, 'PreCondit',      { fg = colors.pink,       bg = 'NONE'            })  -- #if, #ifdef, #endif

  -- Types
  highlight(0, 'Type',           { fg = colors.turquoise,  bg = 'NONE'            })  -- int, long, char
  highlight(0, 'StorageClass',   { fg = colors.turquoise,  bg = 'NONE'            })  -- static, const, volatile
  highlight(0, 'Structure',      { fg = colors.turquoise,  bg = 'NONE'            })  -- struct, union, enum
  highlight(0, 'Typedef',        { fg = colors.turquoise,  bg = 'NONE'            })  -- typedef

  -- Special
  highlight(0, 'Special',        { fg = colors.pink,       bg = 'NONE'            })  -- Any special symbol
  highlight(0, 'SpecialChar',    { fg = colors.pink,       bg = 'NONE'            })  -- \n, \t, etc.
  highlight(0, 'Tag',            { fg = colors.blue,       bg = 'NONE'            })  -- HTML/XML tags
  highlight(0, 'Delimiter',      { fg = colors.white,      bg = 'NONE'            })  -- Delimiters
  highlight(0, 'SpecialComment', { fg = colors.red,        bg = 'NONE', italic = true })  -- Special comments
  highlight(0, 'Debug',          { fg = colors.orange,     bg = 'NONE'            })  -- Debug statements

  -- Other
  highlight(0, 'Underlined',     { fg = colors.blueLink,   bg = 'NONE', underline = true })  -- Underlined text
  highlight(0, 'Ignore',         { fg = colors.gray,       bg = 'NONE'            })  -- Hidden/ignored
  highlight(0, 'Error',          { fg = colors.red,        bg = 'NONE', undercurl = true })  -- Errors
  highlight(0, 'Todo',           { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, FIXME, XXX

  -- Text Formatting
  highlight(0, 'Bold',           { fg = 'NONE',            bg = 'NONE', bold = true })
  highlight(0, 'Italic',         { fg = 'NONE',            bg = 'NONE', italic = true })
  highlight(0, 'Title',          { fg = colors.white,      bg = 'NONE', bold = true })  -- Titles

  -- Additional Standard Groups
  highlight(0, 'Normal',         { fg = colors.white,      bg = colors.black      })  -- Normal text
  highlight(0, 'MatchParen',     { fg = colors.white,      bg = colors.gray       })  -- Matching parenthesis
  highlight(0, 'Variable',       { fg = colors.white,      bg = 'NONE'            })  -- Variables
  highlight(0, 'Conceal',        { fg = colors.gray,       bg = 'NONE'            })  -- Concealed text


  -----------------------------------------------------------------------------
  -- Treesitter Default Captures (@xxx without language suffix)
  -- These are fallbacks for any language

  -- Variables
  highlight(0, '@variable',                    { fg = colors.white,      bg = 'NONE' })  -- Various variable names
  highlight(0, '@variable.builtin',            { fg = colors.purple,     bg = 'NONE' })  -- Built-in variables (self, this)
  highlight(0, '@variable.parameter',          { fg = colors.white,      bg = 'NONE' })  -- Function parameters
  highlight(0, '@variable.parameter.builtin',  { fg = colors.purple,     bg = 'NONE' })  -- Special parameters (_)
  highlight(0, '@variable.member',             { fg = colors.white,      bg = 'NONE' })  -- Object/struct fields

  -- Constants
  highlight(0, '@constant',                    { fg = colors.purple,     bg = 'NONE' })  -- Constants
  highlight(0, '@constant.builtin',            { fg = colors.purple,     bg = 'NONE' })  -- Built-in constants (nil, true)
  highlight(0, '@constant.macro',              { fg = colors.purple,     bg = 'NONE' })  -- Preprocessor constants

  -- Modules/Namespaces
  highlight(0, '@module',                      { fg = colors.white,      bg = 'NONE' })  -- Modules/namespaces
  highlight(0, '@module.builtin',              { fg = colors.purple,     bg = 'NONE' })  -- Built-in modules

  -- Labels
  highlight(0, '@label',                       { fg = colors.blue,       bg = 'NONE' })  -- Labels (goto, etc.)

  -- Strings
  highlight(0, '@string',                      { fg = colors.redLight,   bg = 'NONE' })  -- String literals
  highlight(0, '@string.documentation',        { fg = colors.redLight,   bg = 'NONE' })  -- Docstrings
  highlight(0, '@string.regexp',               { fg = colors.orange,     bg = 'NONE' })  -- Regular expressions
  highlight(0, '@string.escape',               { fg = colors.pink,       bg = 'NONE' })  -- Escape sequences
  highlight(0, '@string.special',              { fg = colors.pink,       bg = 'NONE' })  -- Special strings
  highlight(0, '@string.special.symbol',       { fg = colors.purple,     bg = 'NONE' })  -- Symbols/atoms
  highlight(0, '@string.special.path',         { fg = colors.redLight,   bg = 'NONE' })  -- File paths
  highlight(0, '@string.special.url',          { fg = colors.blueLink,   bg = 'NONE', underline = true })  -- URLs

  -- Characters
  highlight(0, '@character',                   { fg = colors.redLight,   bg = 'NONE' })  -- Character literals
  highlight(0, '@character.special',           { fg = colors.pink,       bg = 'NONE' })  -- Special characters

  -- Numbers
  highlight(0, '@number',                      { fg = colors.greenLight, bg = 'NONE' })  -- Numeric literals
  highlight(0, '@number.float',                { fg = colors.greenLight, bg = 'NONE' })  -- Floating point

  -- Booleans
  highlight(0, '@boolean',                     { fg = colors.blue,       bg = 'NONE' })  -- true, false

  -- Types
  highlight(0, '@type',                        { fg = colors.turquoise,  bg = 'NONE' })  -- Type names
  highlight(0, '@type.builtin',                { fg = colors.turquoise,  bg = 'NONE' })  -- Built-in types
  highlight(0, '@type.definition',             { fg = colors.turquoise,  bg = 'NONE' })  -- Type definitions
  highlight(0, '@type.qualifier',              { fg = colors.blue,       bg = 'NONE' })  -- Type qualifiers (const)

  -- Attributes/Annotations
  highlight(0, '@attribute',                   { fg = colors.purple,     bg = 'NONE' })  -- Annotations/decorators
  highlight(0, '@attribute.builtin',           { fg = colors.purple,     bg = 'NONE' })  -- Built-in annotations

  -- Properties
  highlight(0, '@property',                    { fg = colors.white,      bg = 'NONE' })  -- Object properties

  -- Functions
  highlight(0, '@function',                    { fg = colors.orange,     bg = 'NONE' })  -- Function definitions
  highlight(0, '@function.builtin',            { fg = colors.orange,     bg = 'NONE' })  -- Built-in functions
  highlight(0, '@function.call',               { fg = colors.orange,     bg = 'NONE' })  -- Function calls
  highlight(0, '@function.macro',              { fg = colors.pink,       bg = 'NONE' })  -- Macro functions
  highlight(0, '@function.method',             { fg = colors.orange,     bg = 'NONE' })  -- Method definitions
  highlight(0, '@function.method.call',        { fg = colors.orange,     bg = 'NONE' })  -- Method calls

  -- Constructors
  highlight(0, '@constructor',                 { fg = colors.turquoise,  bg = 'NONE' })  -- Constructors

  -- Operators
  highlight(0, '@operator',                    { fg = colors.white,      bg = 'NONE' })  -- Operators (+, -, *)

  -- Keywords
  highlight(0, '@keyword',                     { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@keyword.coroutine',           { fg = colors.blue,       bg = 'NONE' })  -- Coroutine keywords
  highlight(0, '@keyword.function',            { fg = colors.blue,       bg = 'NONE' })  -- function, def, fn
  highlight(0, '@keyword.operator',            { fg = colors.blue,       bg = 'NONE' })  -- and, or, not
  highlight(0, '@keyword.import',              { fg = colors.pink,       bg = 'NONE' })  -- import, include, require
  highlight(0, '@keyword.type',                { fg = colors.blue,       bg = 'NONE' })  -- class, struct, enum
  highlight(0, '@keyword.modifier',            { fg = colors.blue,       bg = 'NONE' })  -- public, private, static
  highlight(0, '@keyword.repeat',              { fg = colors.blue,       bg = 'NONE' })  -- for, while, loop
  highlight(0, '@keyword.return',              { fg = colors.blue,       bg = 'NONE' })  -- return, yield
  highlight(0, '@keyword.debug',               { fg = colors.orange,     bg = 'NONE' })  -- debugger
  highlight(0, '@keyword.exception',           { fg = colors.blue,       bg = 'NONE' })  -- try, catch, throw
  highlight(0, '@keyword.conditional',         { fg = colors.blue,       bg = 'NONE' })  -- if, else, switch
  highlight(0, '@keyword.conditional.ternary', { fg = colors.white,      bg = 'NONE' })  -- ? :
  highlight(0, '@keyword.directive',           { fg = colors.pink,       bg = 'NONE' })  -- Preprocessor directives
  highlight(0, '@keyword.directive.define',    { fg = colors.pink,       bg = 'NONE' })  -- #define
  highlight(0, '@keyword.storage',             { fg = colors.blue,       bg = 'NONE' })  -- Storage keywords

  -- Punctuation
  highlight(0, '@punctuation.delimiter',       { fg = colors.white,      bg = 'NONE' })  -- ; . ,
  highlight(0, '@punctuation.bracket',         { fg = colors.white,      bg = 'NONE' })  -- () {} []
  highlight(0, '@punctuation.special',         { fg = colors.pink,       bg = 'NONE' })  -- Interpolation ${}

  -- Comments
  highlight(0, '@comment',                     { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@comment.documentation',       { fg = colors.red,        bg = 'NONE' })  -- Doc comments
  highlight(0, '@comment.error',               { fg = colors.red,        bg = 'NONE', bold = true })  -- ERROR, FIXME
  highlight(0, '@comment.warning',             { fg = colors.orange,     bg = 'NONE', bold = true })  -- WARNING, HACK
  highlight(0, '@comment.todo',                { fg = colors.red,        bg = 'NONE', bold = true })  -- TODO, WIP
  highlight(0, '@comment.note',                { fg = colors.turquoise,  bg = 'NONE', bold = true })  -- NOTE, INFO, XXX

  -- Markup (Markdown, etc.)
  highlight(0, '@markup.strong',               { fg = colors.white,      bg = 'NONE', bold = true })  -- **bold**
  highlight(0, '@markup.italic',               { fg = colors.white,      bg = 'NONE', italic = true })  -- *italic*
  highlight(0, '@markup.strikethrough',        { fg = colors.gray,       bg = 'NONE', strikethrough = true })  -- ~~strike~~
  highlight(0, '@markup.underline',            { fg = colors.white,      bg = 'NONE', underline = true })  -- Underlined
  highlight(0, '@markup.heading',              { fg = colors.blue,       bg = 'NONE', bold = true })  -- Headings
  highlight(0, '@markup.heading.1',            { fg = colors.blue,       bg = 'NONE', bold = true })  -- # H1
  highlight(0, '@markup.heading.2',            { fg = colors.blue,       bg = 'NONE', bold = true })  -- ## H2
  highlight(0, '@markup.heading.3',            { fg = colors.blue,       bg = 'NONE', bold = true })  -- ### H3
  highlight(0, '@markup.heading.4',            { fg = colors.blue,       bg = 'NONE', bold = true })  -- #### H4
  highlight(0, '@markup.heading.5',            { fg = colors.blue,       bg = 'NONE', bold = true })  -- ##### H5
  highlight(0, '@markup.heading.6',            { fg = colors.blue,       bg = 'NONE', bold = true })  -- ###### H6
  highlight(0, '@markup.quote',                { fg = colors.gray,       bg = 'NONE', italic = true })  -- > blockquote
  highlight(0, '@markup.math',                 { fg = colors.greenLight, bg = 'NONE' })  -- Math environments
  highlight(0, '@markup.link',                 { fg = colors.blueLink,   bg = 'NONE', underline = true })  -- Links
  highlight(0, '@markup.link.label',           { fg = colors.turquoise,  bg = 'NONE' })  -- [label]
  highlight(0, '@markup.link.url',             { fg = colors.blueLink,   bg = 'NONE' })  -- (url)
  highlight(0, '@markup.raw',                  { fg = colors.redLight,   bg = 'NONE' })  -- `inline code`
  highlight(0, '@markup.raw.block',            { fg = colors.redLight,   bg = 'NONE' })  -- Code blocks
  highlight(0, '@markup.list',                 { fg = colors.blue,       bg = 'NONE' })  -- List markers
  highlight(0, '@markup.list.checked',         { fg = colors.green,      bg = 'NONE' })  -- [x] checked
  highlight(0, '@markup.list.unchecked',       { fg = colors.gray,       bg = 'NONE' })  -- [ ] unchecked

  -- Diff
  highlight(0, '@diff.plus',                   { fg = colors.green,      bg = 'NONE' })  -- Added lines
  highlight(0, '@diff.minus',                  { fg = colors.red,        bg = 'NONE' })  -- Removed lines
  highlight(0, '@diff.delta',                  { fg = colors.yellow,     bg = 'NONE' })  -- Changed lines

  -- Tags (HTML, XML, JSX)
  highlight(0, '@tag',                         { fg = colors.blue,       bg = 'NONE' })  -- Tag names
  highlight(0, '@tag.builtin',                 { fg = colors.blue,       bg = 'NONE' })  -- Built-in tags
  highlight(0, '@tag.attribute',               { fg = colors.turquoise,  bg = 'NONE' })  -- Tag attributes
  highlight(0, '@tag.delimiter',               { fg = colors.white,      bg = 'NONE' })  -- < > </ />

  -- Spell
  highlight(0, '@spell',                       { link = 'Normal' })  -- Content to spell-check
  highlight(0, '@nospell',                     { link = 'Normal' })  -- No spell-check

  -- None (for disabling highlighting)
  highlight(0, '@none',                        { fg = 'NONE',            bg = 'NONE' })


  -----------------------------------------------------------------------------
  -- LSP Semantic Token Types (@lsp.type.xxx)

  highlight(0, '@lsp.type.class',              { fg = colors.turquoise,  bg = 'NONE' })  -- Class types
  highlight(0, '@lsp.type.comment',            { fg = colors.red,        bg = 'NONE' })  -- Comments
  highlight(0, '@lsp.type.decorator',          { fg = colors.purple,     bg = 'NONE' })  -- Decorators
  highlight(0, '@lsp.type.enum',               { fg = colors.turquoise,  bg = 'NONE' })  -- Enum types
  highlight(0, '@lsp.type.enumMember',         { fg = colors.purple,     bg = 'NONE' })  -- Enum members
  highlight(0, '@lsp.type.event',              { fg = colors.orange,     bg = 'NONE' })  -- Events
  highlight(0, '@lsp.type.function',           { fg = colors.orange,     bg = 'NONE' })  -- Functions
  highlight(0, '@lsp.type.interface',          { fg = colors.turquoise,  bg = 'NONE' })  -- Interfaces
  highlight(0, '@lsp.type.keyword',            { fg = colors.blue,       bg = 'NONE' })  -- Keywords
  highlight(0, '@lsp.type.macro',              { fg = colors.pink,       bg = 'NONE' })  -- Macros
  highlight(0, '@lsp.type.method',             { fg = colors.orange,     bg = 'NONE' })  -- Methods
  highlight(0, '@lsp.type.modifier',           { fg = colors.blue,       bg = 'NONE' })  -- Modifiers
  highlight(0, '@lsp.type.namespace',          { fg = colors.white,      bg = 'NONE' })  -- Namespaces
  highlight(0, '@lsp.type.number',             { fg = colors.greenLight, bg = 'NONE' })  -- Numbers
  highlight(0, '@lsp.type.operator',           { fg = colors.white,      bg = 'NONE' })  -- Operators
  highlight(0, '@lsp.type.parameter',          { fg = colors.white,      bg = 'NONE' })  -- Parameters
  highlight(0, '@lsp.type.property',           { fg = colors.white,      bg = 'NONE' })  -- Properties
  highlight(0, '@lsp.type.regexp',             { fg = colors.orange,     bg = 'NONE' })  -- Regexes
  highlight(0, '@lsp.type.string',             { fg = colors.redLight,   bg = 'NONE' })  -- Strings
  highlight(0, '@lsp.type.struct',             { fg = colors.turquoise,  bg = 'NONE' })  -- Structs
  highlight(0, '@lsp.type.type',               { fg = colors.turquoise,  bg = 'NONE' })  -- Types
  highlight(0, '@lsp.type.typeParameter',      { fg = colors.turquoise,  bg = 'NONE' })  -- Type parameters
  highlight(0, '@lsp.type.variable',           { fg = colors.white,      bg = 'NONE' })  -- Variables


  -----------------------------------------------------------------------------
  -- LSP Semantic Token Modifiers (@lsp.mod.xxx)

  highlight(0, '@lsp.mod.abstract',            { fg = 'NONE',            bg = 'NONE', italic = true })  -- Abstract
  highlight(0, '@lsp.mod.async',               { fg = 'NONE',            bg = 'NONE', italic = true })  -- Async
  highlight(0, '@lsp.mod.declaration',         { fg = 'NONE',            bg = 'NONE' })  -- Declarations
  highlight(0, '@lsp.mod.defaultLibrary',      { fg = 'NONE',            bg = 'NONE' })  -- Standard library
  highlight(0, '@lsp.mod.definition',          { fg = 'NONE',            bg = 'NONE' })  -- Definitions
  highlight(0, '@lsp.mod.deprecated',          { fg = 'NONE',            bg = 'NONE', strikethrough = true })  -- Deprecated
  highlight(0, '@lsp.mod.documentation',       { fg = 'NONE',            bg = 'NONE' })  -- Documentation
  highlight(0, '@lsp.mod.modification',        { fg = 'NONE',            bg = 'NONE' })  -- Modifications
  highlight(0, '@lsp.mod.readonly',            { fg = 'NONE',            bg = 'NONE' })  -- Readonly
  highlight(0, '@lsp.mod.static',              { fg = 'NONE',            bg = 'NONE', italic = true })  -- Static


  -----------------------------------------------------------------------------
  -- LSP Reference Highlighting

  highlight(0, 'LspReferenceText',             { fg = 'NONE',            bg = colors.grayBlue })  -- References
  highlight(0, 'LspReferenceRead',             { fg = 'NONE',            bg = colors.grayBlue })  -- Read references
  highlight(0, 'LspReferenceWrite',            { fg = 'NONE',            bg = colors.grayBlue })  -- Write references


  -----------------------------------------------------------------------------
  -- LSP CodeLens

  highlight(0, 'LspCodeLens',                  { fg = colors.gray,       bg = 'NONE' })  -- CodeLens
  highlight(0, 'LspCodeLensSeparator',         { fg = colors.gray,       bg = 'NONE' })  -- CodeLens separator


  -----------------------------------------------------------------------------
  -- LSP Signature Help

  highlight(0, 'LspSignatureActiveParameter',  { fg = colors.orange,     bg = 'NONE', bold = true })  -- Active parameter


  -----------------------------------------------------------------------------
  -- LSP Inlay Hints

  highlight(0, 'LspInlayHint',                 { fg = colors.gray,       bg = 'NONE', italic = true })  -- Inlay hints


  -----------------------------------------------------------------------------
  -- Diagnostic Highlight Groups

  -- Base diagnostic groups
  highlight(0, 'DiagnosticError',              { fg = colors.red,        bg = 'NONE' })  -- Errors
  highlight(0, 'DiagnosticWarn',               { fg = colors.yellow,     bg = 'NONE' })  -- Warnings
  highlight(0, 'DiagnosticInfo',               { fg = colors.blue,       bg = 'NONE' })  -- Information
  highlight(0, 'DiagnosticHint',               { fg = colors.turquoise,  bg = 'NONE' })  -- Hints
  highlight(0, 'DiagnosticOk',                 { fg = colors.green,      bg = 'NONE' })  -- OK

  -- Virtual text
  highlight(0, 'DiagnosticVirtualTextError',   { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'DiagnosticVirtualTextWarn',    { fg = colors.yellow,     bg = 'NONE' })
  highlight(0, 'DiagnosticVirtualTextInfo',    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'DiagnosticVirtualTextHint',    { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'DiagnosticVirtualTextOk',      { fg = colors.green,      bg = 'NONE' })

  -- Underline
  highlight(0, 'DiagnosticUnderlineError',     { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.red })
  highlight(0, 'DiagnosticUnderlineWarn',      { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.yellow })
  highlight(0, 'DiagnosticUnderlineInfo',      { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.blue })
  highlight(0, 'DiagnosticUnderlineHint',      { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.turquoise })
  highlight(0, 'DiagnosticUnderlineOk',        { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.green })

  -- Floating windows
  highlight(0, 'DiagnosticFloatingError',      { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'DiagnosticFloatingWarn',       { fg = colors.yellow,     bg = 'NONE' })
  highlight(0, 'DiagnosticFloatingInfo',       { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'DiagnosticFloatingHint',       { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'DiagnosticFloatingOk',         { fg = colors.green,      bg = 'NONE' })

  -- Signs
  highlight(0, 'DiagnosticSignError',          { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'DiagnosticSignWarn',           { fg = colors.yellow,     bg = 'NONE' })
  highlight(0, 'DiagnosticSignInfo',           { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'DiagnosticSignHint',           { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'DiagnosticSignOk',             { fg = colors.green,      bg = 'NONE' })

  -- Deprecated (for backwards compatibility)
  highlight(0, 'DiagnosticDeprecated',         { fg = 'NONE',            bg = 'NONE', strikethrough = true })
  highlight(0, 'DiagnosticUnnecessary',        { fg = colors.gray,       bg = 'NONE' })


  -----------------------------------------------------------------------------
  -- Spell Checking

  highlight(0, 'SpellBad',                     { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.red })
  highlight(0, 'SpellCap',                     { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.yellow })
  highlight(0, 'SpellLocal',                   { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.turquoise })
  highlight(0, 'SpellRare',                    { fg = 'NONE',            bg = 'NONE', undercurl = true, sp = colors.purple })


  -----------------------------------------------------------------------------
  -- Legacy LSP Groups (for backwards compatibility)

  highlight(0, 'LspDiagnosticsDefaultError',       { link = 'DiagnosticError' })
  highlight(0, 'LspDiagnosticsDefaultWarning',     { link = 'DiagnosticWarn' })
  highlight(0, 'LspDiagnosticsDefaultInformation', { link = 'DiagnosticInfo' })
  highlight(0, 'LspDiagnosticsDefaultHint',        { link = 'DiagnosticHint' })


  -----------------------------------------------------------------------------
  -- Additional Text Objects (for plugins and special cases)

  highlight(0, '@text',                        { fg = colors.white,      bg = 'NONE' })  -- Text content
  highlight(0, '@text.strong',                 { fg = colors.white,      bg = 'NONE', bold = true })
  highlight(0, '@text.emphasis',               { fg = colors.white,      bg = 'NONE', italic = true })
  highlight(0, '@text.underline',              { fg = colors.white,      bg = 'NONE', underline = true })
  highlight(0, '@text.strike',                 { fg = colors.gray,       bg = 'NONE', strikethrough = true })
  highlight(0, '@text.title',                  { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@text.literal',                { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@text.uri',                    { fg = colors.blueLink,   bg = 'NONE', underline = true })
  highlight(0, '@text.math',                   { fg = colors.greenLight, bg = 'NONE' })
  highlight(0, '@text.reference',              { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@text.environment',            { fg = colors.pink,       bg = 'NONE' })
  highlight(0, '@text.environment.name',       { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@text.note',                   { fg = colors.turquoise,  bg = 'NONE', bold = true })
  highlight(0, '@text.warning',                { fg = colors.orange,     bg = 'NONE', bold = true })
  highlight(0, '@text.danger',                 { fg = colors.red,        bg = 'NONE', bold = true })
  highlight(0, '@text.todo',                   { fg = colors.red,        bg = 'NONE', bold = true })


  -----------------------------------------------------------------------------
  -- Injected Languages (for embedded code)

  highlight(0, '@injection.content',           { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@injection.language',          { fg = colors.gray,       bg = 'NONE' })


  -----------------------------------------------------------------------------
  -- Additional Semantic Groups

  highlight(0, '@definition',                  { fg = 'NONE',            bg = 'NONE', underline = true })
  highlight(0, '@definition.constant',         { fg = colors.purple,     bg = 'NONE', underline = true })
  highlight(0, '@definition.function',         { fg = colors.orange,     bg = 'NONE', underline = true })
  highlight(0, '@definition.method',           { fg = colors.orange,     bg = 'NONE', underline = true })
  highlight(0, '@definition.var',              { fg = colors.white,      bg = 'NONE', underline = true })
  highlight(0, '@definition.parameter',        { fg = colors.white,      bg = 'NONE', underline = true })
  highlight(0, '@definition.type',             { fg = colors.turquoise,  bg = 'NONE', underline = true })
  highlight(0, '@definition.field',            { fg = colors.white,      bg = 'NONE', underline = true })

  highlight(0, '@reference',                   { fg = 'NONE',            bg = 'NONE' })
  highlight(0, '@scope',                       { fg = 'NONE',            bg = 'NONE' })
end

return languageDefaults
