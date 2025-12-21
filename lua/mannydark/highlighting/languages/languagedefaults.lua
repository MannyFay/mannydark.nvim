-----------------------------------------------------------------------------------------------------------------------
-- Language Defaults
-- Default/fallback highlighting for all languages.
-- These groups are used when no specific language highlighting is defined.
-----------------------------------------------------------------------------------------------------------------------

-- Color → Semantic Highlight Group     | Color Anchor
--
-- red        = Comment                  | MannydarkRed
-- redLight   = String                   | MannydarkRedLight
-- orange     = Warning                  | MannydarkOrange
-- yellow     = -                        | MannydarkYellow
-- green      = DiagnosticOk             | MannydarkGreen
-- greenLight = Number                   | MannydarkGreenLight
-- blue       = Label                    | MannydarkBlue
-- blueLink   = Underlined               | MannydarkBlueLink
-- purple     = Constant                 | MannydarkPurple
-- pink       = PreProc                  | MannydarkPink
-- turquoise  = Type                     | MannydarkTurquoise
-- white      = Operator                 | MannydarkWhite
-- gray       = Ignore                   | MannydarkGray
-- grayLight  = NonText                  | MannydarkGrayLight
-- grayDark   = ColorColumn  (bg)        | MannydarkGrayDark (bg)
-- black      = Normal       (bg)        | MannydarkBlack (bg)

local colors           = require("mannydark.palette")
local highlight        = vim.api.nvim_set_hl
local languageDefaults = {}

-----------------------------------------------------------------------------------------------------------------------
-- Settings

languageDefaults.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Color Anchors (Mannydark*)
  -- Direct color links without semantic meaning. Use: { link = "MannydarkOrange" }

  highlight(0, "MannydarkFgBlack",      { fg = colors.black, bg = "NONE" }     )
  highlight(0, "MannydarkFgBlue",       { fg = colors.blue, bg = "NONE" }      )
  highlight(0, "MannydarkFgBlueLink",   { fg = colors.blueLink, bg = "NONE" }  )
  highlight(0, "MannydarkFgGray",       { fg = colors.gray, bg = "NONE" }      )
  highlight(0, "MannydarkFgGrayDark",   { fg = colors.grayDark, bg = "NONE" }  )
  highlight(0, "MannydarkFgGrayLight",  { fg = colors.grayLight, bg = "NONE" } )
  highlight(0, "MannydarkFgGreen",      { fg = colors.green, bg = "NONE" }     )
  highlight(0, "MannydarkFgGreenLight", { fg = colors.greenLight, bg = "NONE" })
  highlight(0, "MannydarkFgOrange",     { fg = colors.orange, bg = "NONE" }    )
  highlight(0, "MannydarkFgPink",       { fg = colors.pink, bg = "NONE" }      )
  highlight(0, "MannydarkFgPurple",     { fg = colors.purple, bg = "NONE" }    )
  highlight(0, "MannydarkFgRed",        { fg = colors.red, bg = "NONE" }       )
  highlight(0, "MannydarkFgRedLight",   { fg = colors.redLight, bg = "NONE" }  )
  highlight(0, "MannydarkFgTurquoise",  { fg = colors.turquoise, bg = "NONE" } )
  highlight(0, "MannydarkFgWhite",      { fg = colors.white, bg = "NONE" }     )
  highlight(0, "MannydarkFgYellow",     { fg = colors.yellow, bg = "NONE" }    )


  -----------------------------------------------------------------------------
  -- Standard Vim Syntax Groups
  -- These are the fundamental groups that language syntaxes link to.

  highlight(0, "Bold",           { fg = "NONE", bg = "NONE", bold = true }         )
  highlight(0, "Boolean",        { link = "MannydarkFgBlue" }                      )  -- true, false
  highlight(0, "Character",      { link = "MannydarkFgRedLight" }             )  -- Character literals "c"
  highlight(0, "ColorColumn",    { fg = "NONE", bg = colors.grayDark }             )  -- ColorColumn (grayDark bg)
  highlight(0, "Comment",        { link = "MannydarkFgRed" }                       )  -- Any comment
  highlight(0, "Conceal",        { link = "MannydarkFgGray" }                 )  -- Concealed (folded) text.
  highlight(0, "Conditional",    { link = "MannydarkFgBlue" }                      )  -- if, then, else, switch
  highlight(0, "Constant",       { link = "MannydarkFgPurple" }                    )  -- Any constant
  highlight(0, "Debug",          { fg = colors.orange, bg = "NONE" }               )  -- Debug statements
  highlight(0, "Define",         { link = "MannydarkFgBlue" }                      )  -- #define
  highlight(0, "Delimiter",      { link = "MannydarkFgWhite" }                     )  -- Delimiters
  highlight(0, "Error",          { link = "MannydarkFgRed", undercurl = true }     )  -- Errors
  highlight(0, "Exception",      { link = "MannydarkFgBlue" }                      )  -- try, catch, throw
  highlight(0, "Float",          { link = "MannydarkFgGreenLight" }                )  -- Floating point
  highlight(0, "Function",       { link = "MannydarkFgOrange" }                    )  -- Function names
  highlight(0, "Identifier",     { link = "MannydarkFgPurple" }                    )  -- Variable names
  highlight(0, "Ignore",         { link = "MannydarkFgGray" }                      )  -- Hidden/ignored
  highlight(0, "Include",        { link = "MannydarkFgBlue" }                      )  -- #include, import
  highlight(0, "Italic",         { fg = "NONE", bg = "NONE", italic = true }       )
  highlight(0, "Keyword",        { link = "MannydarkFgBlue" }                      )  -- Any keyword
  highlight(0, "Label",          { link = "MannydarkFgBlue" }                      )  -- case, default, goto labels
  highlight(0, "Macro",          { fg = colors.pink, bg = "NONE" }                 )  -- Macros
  highlight(0, "MatchParen",     { fg = colors.white, bg = colors.gray }           )  -- Matching parenthesis
  highlight(0, "NonText",        { link = "MannydarkFgGray" }            )  -- NonText, EOB, etc.
  highlight(0, "Normal",         { fg = colors.white, bg = colors.black }          )  -- Normal text
  highlight(0, "Number",         { link = "MannydarkFgGreenLight" }                )  -- Numeric literals
  highlight(0, "Operator",       { link = "MannydarkFgWhite" }                     )  -- +, -, *, /, =, etc.
  highlight(0, "PreCondit",      { link = "MannydarkFgBlue" }                      )  -- #if, #ifdef, #endif
  highlight(0, "PreProc",        { link = "MannydarkFgBlue" }                      )  -- Generic preprocessor
  highlight(0, "Repeat",         { link = "MannydarkFgBlue" }                      )  -- for, while, do
  highlight(0, "Special",        { link = "MannydarkFgPink" }                      )  -- Any special symbol
  highlight(0, "SpecialChar",    { link = "MannydarkFgPink" }                      )  -- \n, \t, etc.
  highlight(0, "SpecialComment", { link = "Comment" }                              )  -- Special comments
  highlight(0, "Statement",      { link = "MannydarkFgBlue" }                      )  -- Any statement
  highlight(0, "StorageClass",   { link = "MannydarkFgTurquoise" }                 )  -- static, const, volatile
  highlight(0, "String",         { link = "MannydarkFgRedLight" }                  )  -- String literals
  highlight(0, "Structure",      { link = "MannydarkFgTurquoise" }                 )  -- struct, union, enum
  highlight(0, "Tag",            { link = "MannydarkFgBlue" }                      )  -- HTML/XML tags
  highlight(0, "Title",          { link = "MannydarkFgWhite" }                     )  -- Titles
  highlight(0, "Todo",           { link = "MannydarkFgRed" }                       )  -- TODO, FIXME, XXX
  highlight(0, "Type",           { link = "MannydarkFgTurquoise" }                 )  -- int, long, char
  highlight(0, "Typedef",        { link = "MannydarkFgTurquoise" }                 )  -- typedef
  highlight(0, "Underlined",     { link = "MannydarkFgBlueLink", underline = true })  -- Underlined text
  highlight(0, "Variable",       { link = "MannydarkFgPurple" }                    )  -- Variables
  highlight(0, "Warning",        { link = "MannydarkFgOrange" }                    )  -- Warnings (orange)



-----------------------------------------------------------------------------------------------------------------------
-- LSP Semantic Token Highlights
-- Extracted into separate function so it can be re-applied after LspAttach
-- (to override plugins like lazydev that set their own highlights)

languageDefaults.applyLspSemanticHighlights = function()
  -----------------------------------------------------------------------------
  -- LSP Semantic Token Types (@lsp.type.xxx)

  highlight(0, "@lsp.type.class",         { fg = colors.turquoise, bg = "NONE" })  -- Class types
  highlight(0, "@lsp.type.comment",       { link = "Comment" }                  )  -- Comments
  highlight(0, "@lsp.type.decorator",     { fg = colors.purple, bg = "NONE" }   )  -- Decorators
  highlight(0, "@lsp.type.enum",          { link = "Type" }                     )  -- Enum types
  highlight(0, "@lsp.type.enumMember",    { link = "Variable" }                 )  -- Enum members
  highlight(0, "@lsp.type.event",         { fg = colors.orange, bg = "NONE" }   )  -- Events
  highlight(0, "@lsp.type.function",      { link = "Function" }                 )  -- Functions
  highlight(0, "@lsp.type.interface",     { fg = colors.turquoise, bg = "NONE" })  -- Interfaces
  highlight(0, "@lsp.type.keyword",       { link = "Keyword" }                  )  -- Keywords
  highlight(0, "@lsp.type.macro",         { fg = colors.pink, bg = "NONE" }     )  -- Macros
  highlight(0, "@lsp.type.method",        { link = "Function" }                 )  -- Methods
  highlight(0, "@lsp.type.modifier",      { fg = colors.blue, bg = "NONE" }     )  -- Modifiers
  highlight(0, "@lsp.type.namespace",     { fg = colors.blue, bg = "NONE" }     )  -- Namespaces
  highlight(0, "@lsp.type.number",        { link = "Number" }                   )  -- Numbers
  highlight(0, "@lsp.type.operator",      { link = "Operator" }                 )  -- Operators
  highlight(0, "@lsp.type.parameter",     { link = "Variable" }                 )  -- Parameters
  highlight(0, "@lsp.type.property",      { link = "Variable" }                 )  -- Properties
  highlight(0, "@lsp.type.regexp",        { fg = colors.orange, bg = "NONE" }   )  -- Regexes
  highlight(0, "@lsp.type.string",        { link = "String" }                   )  -- Strings
  highlight(0, "@lsp.type.struct",        { fg = colors.turquoise, bg = "NONE" })  -- Structs
  highlight(0, "@lsp.type.type",          { fg = colors.turquoise, bg = "NONE" })  -- Types
  highlight(0, "@lsp.type.typeParameter", { link = "Variable" }                 )  -- Type parameters
  highlight(0, "@lsp.type.variable",      { link = "Variable" }                 )  -- Variables

  -----------------------------------------------------------------------------
  -- LSP Semantic Token Modifiers (@lsp.mod.xxx)

  highlight(0, "@lsp.mod.abstract",       { fg = "NONE", bg = "NONE", italic = true }       )  -- Abstract
  highlight(0, "@lsp.mod.async",          { fg = "NONE", bg = "NONE", italic = true }       )  -- Async
  highlight(0, "@lsp.mod.declaration",    { fg = "NONE", bg = "NONE" }                      )  -- Declarations
  highlight(0, "@lsp.mod.defaultLibrary", { fg = "NONE", bg = "NONE" }                      )  -- Standard library
  highlight(0, "@lsp.mod.definition",     { fg = "NONE", bg = "NONE" }                      )  -- Definitions
  highlight(0, "@lsp.mod.deprecated",     { fg = "NONE", bg = "NONE", strikethrough = true })  -- Deprecated
  highlight(0, "@lsp.mod.documentation",  { fg = "NONE", bg = "NONE" }                      )  -- Documentation
  highlight(0, "@lsp.mod.modification",   { fg = "NONE", bg = "NONE" }                      )  -- Modifications
  highlight(0, "@lsp.mod.readonly",       { fg = "NONE", bg = "NONE" }                      )  -- Readonly
  highlight(0, "@lsp.mod.static",         { fg = "NONE", bg = "NONE", italic = true }       )  -- Static
end



  -----------------------------------------------------------------------------
  -- Treesitter Default Captures (@xxx without language suffix)
  -- These are fallbacks for any language

  -- Variables
  highlight(0, "@variable",                    { link = "Variable" })  -- Various variable names
  highlight(0, "@variable.builtin",            { link = "Variable" })  -- Built-in variables (self, this)
  highlight(0, "@variable.parameter",          { link = "Variable" })  -- Function parameters
  highlight(0, "@variable.parameter.builtin",  { link = "Variable" })  -- Special parameters (_)
  highlight(0, "@variable.member",             { link = "Variable" })  -- Object/struct fields

  -- Constants
  highlight(0, "@constant",                    { link = "Constant" })  -- Constants
  highlight(0, "@constant.builtin",            { link = "Constant" })  -- Built-in constants (nil, true)
  highlight(0, "@constant.macro",              { link = "Constant" })  -- Preprocessor constants

  -- Modules/Namespaces
  highlight(0, "@module",                      { link = "Type"     })  -- Modules/namespaces
  highlight(0, "@module.builtin",              { link = "Type"     })  -- Built-in modules

  -- Labels
  highlight(0, "@label",                       { link = "Keyword" })  -- Labels (goto, etc.)

  -- Strings
  highlight(0, "@string",                      { link = "String" })  -- String literals
  highlight(0, "@string.documentation",        { link = "String" })  -- Docstrings
  highlight(0, "@string.regexp",               { fg = colors.orange,     bg = "NONE" })  -- Regular expressions
  highlight(0, "@string.escape",               { fg = colors.pink,       bg = "NONE" })  -- Escape sequences
  highlight(0, "@string.special",              { fg = colors.pink,       bg = "NONE" })  -- Special strings
  highlight(0, "@string.special.symbol",       { fg = colors.purple,     bg = "NONE" })  -- Symbols/atoms
  highlight(0, "@string.special.path",         { fg = colors.redLight,   bg = "NONE" })  -- File paths
  highlight(0, "@string.special.url",          { fg = colors.blueLink,   bg = "NONE", underline = true })  -- URLs

  -- Characters
  highlight(0, "@character",                   { fg = colors.redLight,   bg = "NONE" })  -- Character literals
  highlight(0, "@character.special",           { fg = colors.pink,       bg = "NONE" })  -- Special characters

  -- Numbers
  highlight(0, "@number",                      { link = "Number" })  -- Numeric literals
  highlight(0, "@number.float",                { link = "Float" })  -- Floating point

  -- Booleans
  highlight(0, "@boolean",                     { link = "Boolean" })  -- true, false

  -- Types
  highlight(0, "@type",                        { link = "Type" })  -- Type names
  highlight(0, "@type.builtin",                { fg = colors.turquoise,  bg = "NONE" })  -- Built-in types
  highlight(0, "@type.definition",             { fg = colors.turquoise,  bg = "NONE" })  -- Type definitions
  highlight(0, "@type.qualifier",              { fg = colors.blue,       bg = "NONE" })  -- Type qualifiers (const)

  -- Attributes/Annotations
  highlight(0, "@attribute",                   { fg = colors.purple,     bg = "NONE" })  -- Annotations/decorators
  highlight(0, "@attribute.builtin",           { fg = colors.purple,     bg = "NONE" })  -- Built-in annotations

  -- Properties
  highlight(0, "@property",                    { fg = colors.white,      bg = "NONE" })  -- Object properties

  -- Functions
  highlight(0, "@function",                    { link = "Function" })  -- Function definitions
  highlight(0, "@function.builtin",            { link = "Function" })  -- Built-in functions
  highlight(0, "@function.call",               { link = "Function" })  -- Function calls
  highlight(0, "@function.macro",              { link = "Function" })  -- Macro functions
  highlight(0, "@function.method",             { link = "Function" })  -- Method definitions
  highlight(0, "@function.method.call",        { link = "Function" })  -- Method calls

  -- Constructors
  highlight(0, "@constructor",                 { fg = colors.turquoise,  bg = "NONE" })  -- Constructors

  -- Operators
  highlight(0, "@operator",                    { link = "Operator" })  -- Operators (+, -, *)

  -- Keywords
  highlight(0, "@keyword",                     { link = "Keyword" })  -- Keywords
  highlight(0, "@keyword.coroutine",           { link = "Keyword" })  -- Coroutine keywords
  highlight(0, "@keyword.function",            { link = "Keyword" })  -- function, def, fn
  highlight(0, "@keyword.operator",            { link = "Keyword" })  -- and, or, not
  highlight(0, "@keyword.import",              { link = "Keyword" })  -- import, include, require
  highlight(0, "@keyword.type",                { link = "Keyword" })  -- class, struct, enum
  highlight(0, "@keyword.modifier",            { link = "Keyword" })  -- public, private, static
  highlight(0, "@keyword.repeat",              { link = "Keyword" })  -- for, while, loop
  highlight(0, "@keyword.return",              { link = "Keyword" })  -- return, yield
  highlight(0, "@keyword.exception",           { link = "Keyword" })  -- try, catch, throw
  highlight(0, "@keyword.conditional",         { link = "Keyword" })  -- if, else, switch
  highlight(0, "@keyword.directive",           { link = "Keyword" })  -- Preprocessor directives
  highlight(0, "@keyword.storage",             { link = "Keyword" })  -- Storage keywords
  highlight(0, "@keyword.debug",               { fg = colors.orange,     bg = "NONE" })  -- debugger
  highlight(0, "@keyword.directive.define",    { fg = colors.pink,       bg = "NONE" })  -- #define
  highlight(0, "@keyword.conditional.ternary", { fg = colors.white,      bg = "NONE" })  -- ? :

  -- Punctuation
  highlight(0, "@punctuation.delimiter",       { link = "Delimiter" })  -- ; . ,
  highlight(0, "@punctuation.bracket",         { link = "Delimiter" })  -- () {} []
  highlight(0, "@punctuation.special",         { link = "Delimiter" })  -- Interpolation ${}

  -- Comments
  highlight(0, "@comment",                     { link = "Comment" })  -- Comments
  highlight(0, "@comment.documentation",       { link = "Comment" })  -- Doc comments
  highlight(0, "@comment.error",               { link = "Comment" })  -- ERROR, FIXME
  highlight(0, "@comment.warning",             { fg = colors.orange,     bg = "NONE", bold = true })  -- WARNING, HACK
  highlight(0, "@comment.todo",                { link = "Comment" })  -- TODO, WIP
  highlight(0, "@comment.note",                { fg = colors.turquoise,  bg = "NONE", bold = true })  -- NOTE, INFO, XXX

  -- Markup (Markdown, etc.)
  highlight(0, "@markup.strong",               { link = "Normal"})  -- **bold**
  highlight(0, "@markup.italic",               { link = "Normal" })  -- *italic*
  highlight(0, "@markup.strikethrough",        { fg = colors.gray,       bg = "NONE", strikethrough = true })  -- ~~strike~~
  highlight(0, "@markup.underline",            { fg = colors.white,      bg = "NONE", underline = true })  -- Underlined
  highlight(0, "@markup.heading",              { link = "Normal" })  -- Headings
  highlight(0, "@markup.heading.1",            { link = "Normal" })  -- # H1
  highlight(0, "@markup.heading.2",            { link = "Normal" })  -- ## H2
  highlight(0, "@markup.heading.3",            { link = "Normal" })  -- ### H3
  highlight(0, "@markup.heading.4",            { link = "Normal" })  -- #### H4
  highlight(0, "@markup.heading.5",            { link = "Normal" })  -- ##### H5
  highlight(0, "@markup.heading.6",            { link = "Normal" })  -- ###### H6
  highlight(0, "@markup.quote",                { fg = colors.gray,       bg = "NONE", italic = true })  -- > blockquote
  highlight(0, "@markup.math",                 { fg = colors.greenLight, bg = "NONE" })  -- Math environments
  highlight(0, "@markup.link",                 { fg = colors.blueLink,   bg = "NONE", underline = true })  -- Links
  highlight(0, "@markup.link.label",           { fg = colors.turquoise,  bg = "NONE" })  -- [label]
  highlight(0, "@markup.link.url",             { fg = colors.blueLink,   bg = "NONE" })  -- (url)
  highlight(0, "@markup.raw",                  { fg = colors.redLight,   bg = "NONE" })  -- `inline code`
  highlight(0, "@markup.raw.block",            { fg = colors.redLight,   bg = "NONE" })  -- Code blocks
  highlight(0, "@markup.list",                 { fg = colors.blue,       bg = "NONE" })  -- List markers
  highlight(0, "@markup.list.checked",         { fg = colors.green,      bg = "NONE" })  -- [x] checked
  highlight(0, "@markup.list.unchecked",       { fg = colors.gray,       bg = "NONE" })  -- [ ] unchecked

  -- Diff
  highlight(0, "@diff.plus",                   { fg = colors.green,      bg = "NONE" })  -- Added lines
  highlight(0, "@diff.minus",                  { fg = colors.red,        bg = "NONE" })  -- Removed lines
  highlight(0, "@diff.delta",                  { fg = colors.orange,     bg = "NONE" })  -- Changed lines

  -- Tags (HTML, XML, JSX)
  highlight(0, "@tag",                         { fg = colors.blue,       bg = "NONE" })  -- Tag names
  highlight(0, "@tag.builtin",                 { fg = colors.blue,       bg = "NONE" })  -- Built-in tags
  highlight(0, "@tag.attribute",               { fg = colors.turquoise,  bg = "NONE" })  -- Tag attributes
  highlight(0, "@tag.delimiter",               { fg = colors.white,      bg = "NONE" })  -- < > </ />

  -- Spell (no color override - let underlying syntax colors show through)
  highlight(0, "@spell",                       { fg = "NONE", bg = "NONE" })  -- Content to spell-check
  highlight(0, "@nospell",                     { fg = "NONE", bg = "NONE" })  -- No spell-check

  -- None (for disabling highlighting)
  highlight(0, "@none",                        { fg = "NONE",            bg = "NONE" })


  -----------------------------------------------------------------------------
  -- LSP Semantic Token Types and Modifiers
  -- Applied via separate function to allow re-application after LspAttach

  languageDefaults.applyLspSemanticHighlights()


  -----------------------------------------------------------------------------
  -- LSP Reference Highlighting

  highlight(0, "LspReferenceText",             { fg = "NONE",            bg = colors.blue })  -- References
  highlight(0, "LspReferenceRead",             { fg = "NONE",            bg = colors.blue })  -- Read references
  highlight(0, "LspReferenceWrite",            { fg = "NONE",            bg = colors.blue })  -- Write references


  -----------------------------------------------------------------------------
  -- LSP CodeLens

  highlight(0, "LspCodeLens",                  { fg = colors.gray,       bg = "NONE" })  -- CodeLens
  highlight(0, "LspCodeLensSeparator",         { fg = colors.gray,       bg = "NONE" })  -- CodeLens separator


  -----------------------------------------------------------------------------
  -- LSP Signature Help

  highlight(0, "LspSignatureActiveParameter",  { fg = colors.orange,     bg = "NONE", bold = true })  -- Active parameter


  -----------------------------------------------------------------------------
  -- LSP Inlay Hints

  highlight(0, "LspInlayHint",                 { fg = colors.gray,       bg = "NONE", italic = true })  -- Inlay hints


  -----------------------------------------------------------------------------
  -- Diagnostic Highlight Groups

  -- Base diagnostic groups
  highlight(0, "DiagnosticError",              { fg = colors.red,        bg = "NONE" })  -- Errors
  highlight(0, "DiagnosticWarn",               { fg = colors.yellow,     bg = "NONE" })  -- Warnings
  highlight(0, "DiagnosticInfo",               { fg = colors.blue,       bg = "NONE" })  -- Information
  highlight(0, "DiagnosticHint",               { fg = colors.turquoise,  bg = "NONE" })  -- Hints
  highlight(0, "DiagnosticOk",                 { fg = colors.green,      bg = "NONE" })  -- OK

  -- Virtual text
  highlight(0, "DiagnosticVirtualTextError",   { fg = colors.red,        bg = "NONE" })
  highlight(0, "DiagnosticVirtualTextWarn",    { fg = colors.yellow,     bg = "NONE" })
  highlight(0, "DiagnosticVirtualTextInfo",    { fg = colors.blue,       bg = "NONE" })
  highlight(0, "DiagnosticVirtualTextHint",    { fg = colors.turquoise,  bg = "NONE" })
  highlight(0, "DiagnosticVirtualTextOk",      { fg = colors.green,      bg = "NONE" })

  -- Underline
  highlight(0, "DiagnosticUnderlineError",     { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.red })
  highlight(0, "DiagnosticUnderlineWarn",      { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.yellow })
  highlight(0, "DiagnosticUnderlineInfo",      { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.blue })
  highlight(0, "DiagnosticUnderlineHint",      { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.turquoise })
  highlight(0, "DiagnosticUnderlineOk",        { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.green })

  -- Floating windows
  highlight(0, "DiagnosticFloatingError",      { fg = colors.red,        bg = "NONE" })
  highlight(0, "DiagnosticFloatingWarn",       { fg = colors.yellow,     bg = "NONE" })
  highlight(0, "DiagnosticFloatingInfo",       { fg = colors.blue,       bg = "NONE" })
  highlight(0, "DiagnosticFloatingHint",       { fg = colors.turquoise,  bg = "NONE" })
  highlight(0, "DiagnosticFloatingOk",         { fg = colors.green,      bg = "NONE" })

  -- Signs
  highlight(0, "DiagnosticSignError",          { fg = colors.red,        bg = "NONE" })
  highlight(0, "DiagnosticSignWarn",           { fg = colors.orange,     bg = "NONE" })
  highlight(0, "DiagnosticSignInfo",           { fg = colors.blue,       bg = "NONE" })
  highlight(0, "DiagnosticSignHint",           { fg = colors.turquoise,  bg = "NONE" })
  highlight(0, "DiagnosticSignOk",             { fg = colors.green,      bg = "NONE" })

  -- Deprecated (for backwards compatibility)
  highlight(0, "DiagnosticDeprecated",         { fg = "NONE",            bg = "NONE", strikethrough = true })
  highlight(0, "DiagnosticUnnecessary",        { fg = colors.gray,       bg = "NONE" })


  -----------------------------------------------------------------------------
  -- Spell Checking

  highlight(0, "SpellBad",                     { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.red })
  highlight(0, "SpellCap",                     { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.yellow })
  highlight(0, "SpellLocal",                   { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.turquoise })
  highlight(0, "SpellRare",                    { fg = "NONE",            bg = "NONE", undercurl = true, sp = colors.purple })


  -----------------------------------------------------------------------------
  -- Legacy LSP Groups (for backwards compatibility)

  highlight(0, "LspDiagnosticsDefaultError",       { link = "DiagnosticError" })
  highlight(0, "LspDiagnosticsDefaultWarning",     { link = "DiagnosticWarn" })
  highlight(0, "LspDiagnosticsDefaultInformation", { link = "DiagnosticInfo" })
  highlight(0, "LspDiagnosticsDefaultHint",        { link = "DiagnosticHint" })


  -----------------------------------------------------------------------------
  -- Additional Text Objects (for plugins and special cases)

  highlight(0, "@text",                        { fg = colors.white,      bg = "NONE" })  -- Text content
  highlight(0, "@text.strong",                 { fg = colors.white,      bg = "NONE", bold = true })
  highlight(0, "@text.emphasis",               { fg = colors.white,      bg = "NONE", italic = true })
  highlight(0, "@text.underline",              { fg = colors.white,      bg = "NONE", underline = true })
  highlight(0, "@text.strike",                 { fg = colors.gray,       bg = "NONE", strikethrough = true })
  highlight(0, "@text.title",                  { fg = colors.blue,       bg = "NONE", bold = true })
  highlight(0, "@text.literal",                { fg = colors.redLight,   bg = "NONE" })
  highlight(0, "@text.uri",                    { fg = colors.blueLink,   bg = "NONE", underline = true })
  highlight(0, "@text.math",                   { fg = colors.greenLight, bg = "NONE" })
  highlight(0, "@text.reference",              { fg = colors.turquoise,  bg = "NONE" })
  highlight(0, "@text.environment",            { fg = colors.pink,       bg = "NONE" })
  highlight(0, "@text.environment.name",       { fg = colors.turquoise,  bg = "NONE" })
  highlight(0, "@text.note",                   { fg = colors.turquoise,  bg = "NONE", bold = true })
  highlight(0, "@text.warning",                { fg = colors.orange,     bg = "NONE", bold = true })
  highlight(0, "@text.danger",                 { fg = colors.red,        bg = "NONE", bold = true })
  highlight(0, "@text.todo",                   { fg = colors.red,        bg = "NONE", bold = true })


  -----------------------------------------------------------------------------
  -- Injected Languages (for embedded code)

  highlight(0, "@injection.content",           { fg = colors.white,      bg = "NONE" })
  highlight(0, "@injection.language",          { fg = colors.gray,       bg = "NONE" })


  -----------------------------------------------------------------------------
  -- Additional Semantic Groups

  highlight(0, "@definition",                  { fg = "NONE",            bg = "NONE", underline = true })
  highlight(0, "@definition.constant",         { fg = colors.purple,     bg = "NONE", underline = true })
  highlight(0, "@definition.function",         { fg = colors.orange,     bg = "NONE", underline = true })
  highlight(0, "@definition.method",           { fg = colors.orange,     bg = "NONE", underline = true })
  highlight(0, "@definition.var",              { fg = colors.white,      bg = "NONE", underline = true })
  highlight(0, "@definition.parameter",        { fg = colors.white,      bg = "NONE", underline = true })
  highlight(0, "@definition.type",             { fg = colors.turquoise,  bg = "NONE", underline = true })
  highlight(0, "@definition.field",            { fg = colors.white,      bg = "NONE", underline = true })

  highlight(0, "@reference",                   { fg = "NONE",            bg = "NONE" })
  highlight(0, "@scope",                       { fg = "NONE",            bg = "NONE" })
end

return languageDefaults
