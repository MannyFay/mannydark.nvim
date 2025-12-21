-------------------------------------------------------------------------------
-- React (JSX/TSX)
-- Highlighting for .jsx, .tsx files and JSX syntax in JavaScript/TypeScript.
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local react     = {}


-------------------------------------------------------------------------------
-- Settings

react.setupHighlighting = function()
  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - JSX

  -- Comments
  highlight(0, "@comment.jsx",                { link = "Comment" })  -- Comments (Treesitter).
  highlight(0, "@comment.tsx",                { link = "Comment" })  -- Comments (Treesitter).
  highlight(0, "jsxComment",          { link = "Comment"})  -- {/* comment */} (legacy).
  highlight(0, "tsxBlockComment",     { link = "Comment"            })  -- Block comments (legacy).
  highlight(0, "tsxLineComment",      { link = "Comment"            })  -- Line comments (legacy).
  highlight(0, "tsxCommentInvalid",   { link = "Comment"            })  -- Invalid comment (legacy).
  highlight(0, "@lsp.type.comment.javascriptreact",      { fg = colors.red,       bg = "NONE" })  -- Comments.
  highlight(0, "@lsp.type.comment.typescriptreact",      { fg = colors.red,       bg = "NONE" })  -- Comments.
  highlight(0, "@spell.tsx", { link = "Comment" }) -- Comments.


  -- JSX Tags - Structure
  highlight(0, "jsxTag",              { link = "MannydarkFgPink"            })  -- Overall tag structure
  highlight(0, "jsxTagName",          { link = "Comment"            })  -- HTML tag names (div, span, etc.)
  highlight(0, "jsxComponentName",    { fg = colors.turquoise,  bg = "NONE"            })  -- React component names (PascalCase)
  highlight(0, "jsxIntrinsicTagName", { fg = colors.blue,       bg = "NONE"            })  -- Intrinsic HTML elements

  -- JSX Tags - Punctuation
  highlight(0, "jsxOpenPunct",        { fg = colors.white,      bg = "NONE"            })  -- < and > of opening tag
  highlight(0, "jsxClosePunct",       { fg = colors.white,      bg = "NONE"            })  -- </ of closing tag
  highlight(0, "jsxCloseString",      { fg = colors.white,      bg = "NONE"            })  -- /> self-closing
  highlight(0, "jsxCloseTag",         { fg = colors.white,      bg = "NONE"            })  -- Closing tag structure
  highlight(0, "jsxOpenTag",          { fg = colors.white,      bg = "NONE"            })  -- Opening tag structure
  highlight(0, "jsxPunct",            { fg = colors.white,      bg = "NONE"            })  -- General JSX punctuation

  -- JSX Attributes
  highlight(0, "jsxAttrib",           { fg = colors.purple,     bg = "NONE"            })  -- Attribute names (className, onClick, etc.)
  highlight(0, "jsxAttribKeyword",    { fg = colors.purple,     bg = "NONE"            })  -- Reserved attribute keywords (class, for)
  highlight(0, "jsxEqual",            { fg = colors.white,      bg = "NONE"            })  -- = in attributes
  highlight(0, "jsxString",           { fg = colors.redLight,   bg = "NONE"            })  -- Attribute string values

  -- JSX Expressions
  highlight(0, "jsxExpressionBlock", { fg = colors.white,       bg = "NONE"            })  -- {...} expression blocks
  highlight(0, "jsxBraces",           { fg = colors.white,      bg = "NONE"            })  -- { } braces in expressions
  highlight(0, "jsxCurly",            { fg = colors.white,      bg = "NONE"            })  -- Curly braces

  -- JSX Operators
  highlight(0, "jsxSpreadOperator",   { fg = colors.pink,       bg = "NONE"            })  -- ... spread operator
  highlight(0, "jsxDot",              { fg = colors.white,      bg = "NONE"            })  -- . for namespaced components (Foo.Bar)
  highlight(0, "jsxNamespace",        { fg = colors.white,      bg = "NONE"            })  -- : XML namespace separator


  -- JSX Text
  highlight(0, "jsxText",             { fg = colors.white,      bg = "NONE"            })  -- Text content between tags

  -- JSX Regions (usually cleared/transparent)
  highlight(0, "jsxElement",          { fg = "NONE",            bg = "NONE"            })  -- Entire element region
  highlight(0, "jsxRegion",           { fg = "NONE",            bg = "NONE"            })  -- JSX region
  highlight(0, "jsxTaggedRegion",     { fg = "NONE",            bg = "NONE"            })  -- Tagged template region
  highlight(0, "jsxBackticks",        { fg = colors.redLight,   bg = "NONE"            })  -- Template literal backticks


  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - TSX (TypeScript + JSX)

  -- TSX Tags
  highlight(0, "tsxTag",              { fg = colors.white,      bg = "NONE"            })  -- TSX tag structure
  highlight(0, "tsxTagName",          { fg = colors.blue,       bg = "NONE"            })  -- HTML tag names in TSX
  highlight(0, "tsxIntrinsicTagName", { fg = colors.blue,       bg = "NONE"            })  -- Intrinsic HTML elements
  highlight(0, "tsxComponentName",    { fg = colors.turquoise,  bg = "NONE"            })  -- React component names

  -- TSX Punctuation
  highlight(0, "tsxCloseString",      { fg = colors.white,      bg = "NONE"            })  -- /> self-closing
  highlight(0, "tsxCloseTag",         { fg = colors.white,      bg = "NONE"            })  -- Closing tag

  -- TSX Attributes
  highlight(0, "tsxAttrib",           { fg = colors.purple,     bg = "NONE"            })  -- Attribute names
  highlight(0, "tsxEqual",            { fg = colors.white,      bg = "NONE"            })  -- = in attributes
  highlight(0, "tsxString",           { fg = colors.redLight,   bg = "NONE"            })  -- Attribute string values

  -- TSX Expressions
  highlight(0, "tsxEscJs",            { fg = colors.white,      bg = "NONE"            })  -- Escaped JavaScript
  highlight(0, "tsxEscapeJs",         { fg = colors.white,      bg = "NONE"            })  -- Escaped JavaScript

  -- TSX Fragments
  highlight(0, "tsxFragment",         { fg = colors.white,      bg = "NONE"            })  -- <> </> fragments


  -- TSX Namespace
  highlight(0, "tsxNameSpace",        { fg = colors.turquoise,  bg = "NONE"            })  -- Namespace prefix

  -- TSX Entities
  highlight(0, "tsxEntity",           { fg = colors.pink,       bg = "NONE"            })  -- HTML entities (&nbsp;, etc.)
  highlight(0, "tsxEntityPunct",      { fg = colors.pink,       bg = "NONE"            })  -- & and ; of entities

  -- TSX Regions
  highlight(0, "tsxRegion",           { fg = "NONE",            bg = "NONE"            })  -- TSX region


  -----------------------------------------------------------------------------
  -- React Hooks - Special highlighting for common hooks
  -- (These are function calls, but we can target specific patterns)

  highlight(0, "reactHook",           { fg = colors.orange,     bg = "NONE"            })  -- useState, useEffect, etc.
  highlight(0, "reactHookName",       { fg = colors.orange,     bg = "NONE"            })  -- Hook names
  highlight(0, "reactUseState",       { fg = colors.orange,     bg = "NONE"            })  -- useState
  highlight(0, "reactUseEffect",      { fg = colors.orange,     bg = "NONE"            })  -- useEffect
  highlight(0, "reactUseContext",     { fg = colors.orange,     bg = "NONE"            })  -- useContext
  highlight(0, "reactUseReducer",     { fg = colors.orange,     bg = "NONE"            })  -- useReducer
  highlight(0, "reactUseCallback",    { fg = colors.orange,     bg = "NONE"            })  -- useCallback
  highlight(0, "reactUseMemo",        { fg = colors.orange,     bg = "NONE"            })  -- useMemo
  highlight(0, "reactUseRef",         { fg = colors.orange,     bg = "NONE"            })  -- useRef
  highlight(0, "reactUseLayoutEffect", { fg = colors.orange,    bg = "NONE"            })  -- useLayoutEffect
  highlight(0, "reactUseImperativeHandle", { fg = colors.orange, bg = "NONE"           })  -- useImperativeHandle
  highlight(0, "reactUseDebugValue",  { fg = colors.orange,     bg = "NONE"            })  -- useDebugValue
  highlight(0, "reactUseTransition",  { fg = colors.orange,     bg = "NONE"            })  -- useTransition (React 18+)
  highlight(0, "reactUseDeferredValue", { fg = colors.orange,   bg = "NONE"            })  -- useDeferredValue (React 18+)
  highlight(0, "reactUseId",          { fg = colors.orange,     bg = "NONE"            })  -- useId (React 18+)


  -----------------------------------------------------------------------------
  -- React Component Patterns

  highlight(0, "reactComponent",      { fg = colors.turquoise,  bg = "NONE"            })  -- Component definitions
  highlight(0, "reactFragment",       { fg = colors.white,      bg = "NONE"            })  -- React.Fragment, <>, </>
  highlight(0, "reactCreateElement",  { fg = colors.orange,     bg = "NONE"            })  -- React.createElement
  highlight(0, "reactCloneElement",   { fg = colors.orange,     bg = "NONE"            })  -- React.cloneElement
  highlight(0, "reactForwardRef",     { fg = colors.orange,     bg = "NONE"            })  -- React.forwardRef
  highlight(0, "reactMemo",           { fg = colors.orange,     bg = "NONE"            })  -- React.memo
  highlight(0, "reactLazy",           { fg = colors.orange,     bg = "NONE"            })  -- React.lazy
  highlight(0, "reactSuspense",       { fg = colors.turquoise,  bg = "NONE"            })  -- Suspense component


  -----------------------------------------------------------------------------
  -- Common JSX Attributes (semantic highlighting)

  highlight(0, "jsxClassName",        { fg = colors.purple,     bg = "NONE"            })  -- className attribute
  highlight(0, "jsxOnClick",          { fg = colors.purple,     bg = "NONE"            })  -- onClick and event handlers
  highlight(0, "jsxKey",              { fg = colors.pink,       bg = "NONE"            })  -- key prop (important for React)
  highlight(0, "jsxRef",              { fg = colors.pink,       bg = "NONE"            })  -- ref prop
  highlight(0, "jsxChildren",         { fg = colors.purple,     bg = "NONE"            })  -- children prop
  highlight(0, "jsxStyle",            { fg = colors.purple,     bg = "NONE"            })  -- style attribute
  highlight(0, "jsxDangerouslySetInnerHTML", { fg = colors.red, bg = "NONE"            })  -- dangerouslySetInnerHTML (warning color)


  -----------------------------------------------------------------------------
  -- Treesitter Groups (@xxx.jsx / @xxx.tsx / @xxx.javascript / @xxx.typescript)

  -- Tag Delimiters
  highlight(0, "@tag.delimiter.jsx",          { fg = colors.white,     bg = "NONE" })  -- < > </ />
  highlight(0, "@tag.delimiter.tsx",          { fg = colors.white,     bg = "NONE" })  -- < > </ />
  highlight(0, "@tag.delimiter.javascript",   { fg = colors.white,     bg = "NONE" })  -- JSX in JS files
  highlight(0, "@tag.delimiter.typescript",   { fg = colors.white,     bg = "NONE" })  -- JSX in TS files

  -- Tag Names - Built-in HTML elements (lowercase)
  highlight(0, "@tag.builtin.jsx",            { fg = colors.blue,      bg = "NONE" })  -- div, span, etc.
  highlight(0, "@tag.builtin.tsx",            { fg = colors.blue,      bg = "NONE" })  -- div, span, etc.
  highlight(0, "@tag.builtin.javascript",     { fg = colors.blue,      bg = "NONE" })  -- HTML elements
  highlight(0, "@tag.builtin.typescript",     { fg = colors.blue,      bg = "NONE" })  -- HTML elements

  -- Tag Names - React Components (PascalCase)
  highlight(0, "@tag.jsx",                    { fg = colors.turquoise, bg = "NONE" })  -- Component names
  highlight(0, "@tag.tsx",                    { fg = colors.turquoise, bg = "NONE" })  -- Component names
  highlight(0, "@tag.javascript",             { fg = colors.turquoise, bg = "NONE" })  -- Component names
  highlight(0, "@tag.typescript",             { fg = colors.turquoise, bg = "NONE" })  -- Component names

  -- Tag Attributes
  highlight(0, "@tag.attribute.jsx",          { fg = colors.purple,    bg = "NONE" })  -- Attribute names
  highlight(0, "@tag.attribute.tsx",          { fg = colors.purple,    bg = "NONE" })  -- Attribute names
  highlight(0, "@tag.attribute.javascript",   { fg = colors.purple,    bg = "NONE" })  -- Attribute names
  highlight(0, "@tag.attribute.typescript",   { fg = colors.purple,    bg = "NONE" })  -- Attribute names

  -- URL Attributes (href, src)
  highlight(0, "@string.special.url.jsx",     { fg = colors.redLight,  bg = "NONE" })  -- URL values
  highlight(0, "@string.special.url.tsx",     { fg = colors.redLight,  bg = "NONE" })  -- URL values
  highlight(0, "@string.special.url.javascript", { fg = colors.redLight, bg = "NONE" })  -- URL values
  highlight(0, "@string.special.url.typescript", { fg = colors.redLight, bg = "NONE" })  -- URL values

  -- Markup Captures (semantic HTML elements)
  highlight(0, "@markup.heading.jsx",         { fg = colors.blue,      bg = "NONE", bold = true })  -- h1-h6
  highlight(0, "@markup.heading.tsx",         { fg = colors.blue,      bg = "NONE", bold = true })  -- h1-h6
  highlight(0, "@markup.heading.1.jsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h1
  highlight(0, "@markup.heading.1.tsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h1
  highlight(0, "@markup.heading.2.jsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h2
  highlight(0, "@markup.heading.2.tsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h2
  highlight(0, "@markup.heading.3.jsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h3
  highlight(0, "@markup.heading.3.tsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h3
  highlight(0, "@markup.heading.4.jsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h4
  highlight(0, "@markup.heading.4.tsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h4
  highlight(0, "@markup.heading.5.jsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h5
  highlight(0, "@markup.heading.5.tsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h5
  highlight(0, "@markup.heading.6.jsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h6
  highlight(0, "@markup.heading.6.tsx",       { fg = colors.blue,      bg = "NONE", bold = true })  -- h6

  highlight(0, "@markup.strong.jsx",          { fg = colors.white,     bg = "NONE", bold = true })  -- <strong>, <b>
  highlight(0, "@markup.strong.tsx",          { fg = colors.white,     bg = "NONE", bold = true })  -- <strong>, <b>
  highlight(0, "@markup.italic.jsx",          { fg = colors.white,     bg = "NONE", italic = true })  -- <em>, <i>
  highlight(0, "@markup.italic.tsx",          { fg = colors.white,     bg = "NONE", italic = true })  -- <em>, <i>
  highlight(0, "@markup.strikethrough.jsx",   { fg = colors.white,     bg = "NONE", strikethrough = true })  -- <s>, <del>
  highlight(0, "@markup.strikethrough.tsx",   { fg = colors.white,     bg = "NONE", strikethrough = true })  -- <s>, <del>
  highlight(0, "@markup.underline.jsx",       { fg = colors.white,     bg = "NONE", underline = true })  -- <u>
  highlight(0, "@markup.underline.tsx",       { fg = colors.white,     bg = "NONE", underline = true })  -- <u>
  highlight(0, "@markup.raw.jsx",             { fg = colors.redLight,  bg = "NONE" })  -- <code>, <kbd>
  highlight(0, "@markup.raw.tsx",             { fg = colors.redLight,  bg = "NONE" })  -- <code>, <kbd>
  highlight(0, "@markup.link.label.jsx",      { fg = colors.blue,      bg = "NONE", underline = true })  -- <a> elements
  highlight(0, "@markup.link.label.tsx",      { fg = colors.blue,      bg = "NONE", underline = true })  -- <a> elements

  -- Character References (&nbsp;, etc.)
  highlight(0, "@character.special.jsx",      { fg = colors.pink,      bg = "NONE" })  -- HTML entities
  highlight(0, "@character.special.tsx",      { fg = colors.pink,      bg = "NONE" })  -- HTML entities

  -- JSX-specific punctuation
  highlight(0, "@punctuation.bracket.jsx",    { fg = colors.white,     bg = "NONE" })  -- { } in expressions
  highlight(0, "@punctuation.bracket.tsx",    { fg = colors.white,     bg = "NONE" })  -- { } in expressions

  -- Variables in JSX
  highlight(0, "@variable.jsx",               { link = "Constant" })  -- Variables
  highlight(0, "@variable.tsx",               { link = "Constant" })  -- Variables

  -- Functions/Hooks
  highlight(0, "@function.jsx",               { link = "Function" })  -- Functions
  highlight(0, "@function.tsx",               { link = "Function" })  -- Functions
  highlight(0, "@function.call.jsx",          { link = "Function" })  -- Function calls
  highlight(0, "@function.call.tsx",          { link = "Function" })  -- Function calls
  highlight(0, "@function.builtin.jsx",       { link = "Function" })  -- Built-in functions
  highlight(0, "@function.builtin.tsx",       { link = "Function" })  -- Built-in functions

  -- Methods
  highlight(0, "@function.method.jsx",        { link = "Function" })  -- Methods
  highlight(0, "@function.method.tsx",        { link = "Function" })  -- Methods
  highlight(0, "@function.method.call.jsx",   { link = "Function" })  -- Method calls
  highlight(0, "@function.method.call.tsx",   { link = "Function" })  -- Method calls

  -- Keywords
  highlight(0, "@keyword.jsx",                { link = "Keyword" })  -- Keywords
  highlight(0, "@keyword.tsx",                { link = "Keyword" })  -- Keywords
  highlight(0, "@keyword.function.jsx",       { link = "Keyword" })  -- function keyword
  highlight(0, "@keyword.function.tsx",       { link = "Keyword" })  -- function keyword
  highlight(0, "@keyword.return.jsx",         { link = "Keyword" })  -- return
  highlight(0, "@keyword.return.tsx",         { link = "Keyword" })  -- return

  -- Types (for TSX)
  highlight(0, "@type.tsx",                   { link = "Type" })  -- Type names
  highlight(0, "@type.builtin.tsx",           { fg = colors.blue,      bg = "NONE" })  -- Built-in types
  highlight(0, "@type.definition.tsx",        { fg = colors.turquoise, bg = "NONE" })  -- Type definitions

  -- Strings
  highlight(0, "@string.jsx",                 { link = "String" })  -- Strings
  highlight(0, "@string.tsx",                 { link = "String" })  -- Strings

  -- Numbers
  highlight(0, "@number.jsx",                 { link = "Number" })  -- Numbers
  highlight(0, "@number.tsx",                 { link = "Number" })  -- Numbers

  -- Booleans
  highlight(0, "@boolean.jsx",                { link = "Boolean" })  -- true, false
  highlight(0, "@boolean.tsx",                { link = "Boolean" })  -- true, false


  -- Operators
  highlight(0, "@operator.jsx",               { link = "Normal" })  -- Operators
  highlight(0, "@operator.tsx",               { link = "Normal" })  -- Operators


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (@lsp.type.xxx.javascriptreact / @lsp.type.xxx.typescriptreact)

  -- JavaScript React (JSX)
  highlight(0, "@lsp.type.class.javascriptreact",        { fg = colors.turquoise, bg = "NONE" })  -- Classes/Components
  highlight(0, "@lsp.type.interface.javascriptreact",    { fg = colors.turquoise, bg = "NONE" })  -- Interfaces
  highlight(0, "@lsp.type.type.javascriptreact",         { fg = colors.turquoise, bg = "NONE" })  -- Types
  highlight(0, "@lsp.type.enum.javascriptreact",         { fg = colors.turquoise, bg = "NONE" })  -- Enums
  highlight(0, "@lsp.type.function.javascriptreact",     { fg = colors.orange,    bg = "NONE" })  -- Functions
  highlight(0, "@lsp.type.method.javascriptreact",       { fg = colors.orange,    bg = "NONE" })  -- Methods
  highlight(0, "@lsp.type.variable.javascriptreact",     { link = "Constant"})  -- Variables
  highlight(0, "@lsp.type.parameter.javascriptreact",    { fg = colors.purple,    bg = "NONE" })  -- Parameters
  highlight(0, "@lsp.type.property.javascriptreact",     { fg = colors.purple,    bg = "NONE" })  -- Properties
  highlight(0, "@lsp.type.keyword.javascriptreact",      { fg = colors.blue,      bg = "NONE" })  -- Keywords
  highlight(0, "@lsp.type.string.javascriptreact",       { fg = colors.redLight,  bg = "NONE" })  -- Strings
  highlight(0, "@lsp.type.number.javascriptreact",       { fg = colors.greenLight, bg = "NONE" }) -- Numbers

  -- TypeScript React (TSX)
  highlight(0, "@lsp.type.class.typescriptreact",        { fg = colors.turquoise, bg = "NONE" })  -- Classes/Components
  highlight(0, "@lsp.type.interface.typescriptreact",    { fg = colors.turquoise, bg = "NONE" })  -- Interfaces
  highlight(0, "@lsp.type.type.typescriptreact",         { fg = colors.turquoise, bg = "NONE" })  -- Types
  highlight(0, "@lsp.typemod.typeParameter.typescriptreact", { fg = colors.turquoise, bg = "NONE" }) -- Type parameters
  highlight(0, "@lsp.type.enum.typescriptreact",         { fg = colors.turquoise, bg = "NONE" })  -- Enums
  highlight(0, "@lsp.type.enumMember.typescriptreact",   { fg = colors.pink,      bg = "NONE" })  -- Enum members
  highlight(0, "@lsp.type.function.typescriptreact",     { fg = colors.orange,    bg = "NONE" })  -- Functions
  highlight(0, "@lsp.type.method.typescriptreact",       { fg = colors.orange,    bg = "NONE" })  -- Methods
  highlight(0, "@lsp.type.variable.typescriptreact",     { link = "Constant"})  -- Variables
  highlight(0, "@lsp.type.parameter.typescriptreact",    { fg = colors.purple,    bg = "NONE" })  -- Parameters
  highlight(0, "@lsp.type.property.typescriptreact",     { fg = colors.purple,    bg = "NONE" })  -- Properties
  highlight(0, "@lsp.type.keyword.typescriptreact",      { fg = colors.blue,      bg = "NONE" })  -- Keywords
  highlight(0, "@lsp.type.namespace.typescriptreact",    { fg = colors.turquoise, bg = "NONE" })  -- Namespaces
  highlight(0, "@lsp.type.decorator.typescriptreact",    { fg = colors.pink,      bg = "NONE" })  -- Decorators
  highlight(0, "@lsp.type.string.typescriptreact",       { fg = colors.redLight,  bg = "NONE" })  -- Strings
  highlight(0, "@lsp.type.number.typescriptreact",       { fg = colors.greenLight, bg = "NONE" }) -- Numbers

  -- LSP Modifiers - JavaScript React
  highlight(0, "@lsp.typemod.variable.readonly.javascriptreact",    { link = "Constant" })  -- Constants
  highlight(0, "@lsp.typemod.function.declaration.javascriptreact", { link = "Function"})  -- Function declarations
  highlight(0, "@lsp.typemod.function.defaultLibrary.javascriptreact", { link = "Function"})  -- Built-in functions
  highlight(0, "@lsp.typemod.class.declaration.javascriptreact",    { fg = colors.turquoise, bg = "NONE" })  -- Component declarations
  highlight(0, "@lsp.typemod.variable.defaultLibrary.javascriptreact", { link = "Constant"})  -- Built-in variables

  -- LSP Modifiers - TypeScript React
  highlight(0, "@lsp.typemod.variable.readonly.typescriptreact",    { link = "Constant" })  -- Constants
  highlight(0, "@lsp.typemod.function.declaration.typescriptreact",    { link = "Function"})  -- Function declarations
  highlight(0, "@lsp.typemod.function.defaultLibrary.typescriptreact", { link = "Function"})  -- Built-in functions
  highlight(0, "@lsp.typemod.class.declaration.typescriptreact",    { fg = colors.turquoise, bg = "NONE" })  -- Component declarations
  highlight(0, "@lsp.typemod.variable.defaultLibrary.typescriptreact", { link = "Constant"})  -- Built-in variables
  highlight(0, "@lsp.typemod.type.declaration.typescriptreact",     { fg = colors.turquoise, bg = "NONE" })  -- Type declarations
  highlight(0, "@lsp.typemod.interface.declaration.typescriptreact", { fg = colors.turquoise, bg = "NONE" }) -- Interface declarations



  -----------------------------------------------------------------------------
  -- Styled Components / CSS-in-JS (common in React)

  highlight(0, "styledComponentsTaggedTemplateLiteral", { fg = colors.redLight, bg = "NONE" })  -- Styled template
  highlight(0, "styledDefinition",                      { fg = colors.turquoise, bg = "NONE" }) -- styled.div, styled.span
  highlight(0, "styledComponentName",                   { fg = colors.turquoise, bg = "NONE" }) -- Component name
end

return react
