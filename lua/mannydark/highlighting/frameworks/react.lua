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
  highlight(0, "@comment.jsx",                { link = "Comment" })
  highlight(0, "@comment.tsx",                { link = "Comment" })
  highlight(0, "@comment.documentation.tsx",  { link = "MannydarkFgGreen" })
  highlight(0, "jsxComment",                  { link = "Comment" })
  highlight(0, "tsxBlockComment",             { link = "Comment" })
  highlight(0, "tsxLineComment",              { link = "Comment" })
  highlight(0, "tsxCommentInvalid",           { link = "Comment" })
  highlight(0, "@lsp.type.comment.javascriptreact", { fg = colors.red, bg = "NONE" })
  highlight(0, "@lsp.type.comment.typescriptreact", { fg = colors.red, bg = "NONE" })
  highlight(0, "@spell.tsx",                  {})  -- just spell check, no color

  -- JSX Tags - Structure
  highlight(0, "jsxTag",                      { link = "MannydarkFgPink" })
  highlight(0, "jsxTagName",                  { link = "Comment" })
  highlight(0, "jsxComponentName",            { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "jsxIntrinsicTagName",         { fg = colors.blue,      bg = "NONE" })

  -- JSX Tags - Punctuation
  highlight(0, "jsxOpenPunct",                { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxClosePunct",               { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxCloseString",              { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxCloseTag",                 { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxOpenTag",                  { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxPunct",                    { fg = colors.white,     bg = "NONE" })

  -- JSX Attributes
  highlight(0, "jsxAttrib",                   { fg = colors.purple,    bg = "NONE" })
  highlight(0, "jsxAttribKeyword",            { fg = colors.purple,    bg = "NONE" })
  highlight(0, "jsxEqual",                    { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxString",                   { fg = colors.redLight,  bg = "NONE" })

  -- JSX Expressions
  highlight(0, "jsxExpressionBlock",          { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxBraces",                   { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxCurly",                    { fg = colors.white,     bg = "NONE" })

  -- JSX Operators
  highlight(0, "jsxSpreadOperator",           { fg = colors.pink,      bg = "NONE" })
  highlight(0, "jsxDot",                      { fg = colors.white,     bg = "NONE" })
  highlight(0, "jsxNamespace",                { fg = colors.white,     bg = "NONE" })

  -- JSX Text
  highlight(0, "jsxText",                     { fg = colors.white,     bg = "NONE" })

  -- JSX Regions
  highlight(0, "jsxElement",                  { fg = "NONE",           bg = "NONE" })
  highlight(0, "jsxRegion",                   { fg = "NONE",           bg = "NONE" })
  highlight(0, "jsxTaggedRegion",             { fg = "NONE",           bg = "NONE" })
  highlight(0, "jsxBackticks",                { fg = colors.redLight,  bg = "NONE" })


  -----------------------------------------------------------------------------
  -- Vim Syntax Groups (legacy) - TSX

  -- TSX Tags
  highlight(0, "tsxTag",                      { fg = colors.white,     bg = "NONE" })
  highlight(0, "tsxTagName",                  { fg = colors.blue,      bg = "NONE" })
  highlight(0, "tsxIntrinsicTagName",         { fg = colors.blue,      bg = "NONE" })
  highlight(0, "tsxComponentName",            { fg = colors.turquoise, bg = "NONE" })

  -- TSX Punctuation
  highlight(0, "tsxCloseString",              { fg = colors.white,     bg = "NONE" })
  highlight(0, "tsxCloseTag",                 { fg = colors.white,     bg = "NONE" })

  -- TSX Attributes
  highlight(0, "tsxAttrib",                   { fg = colors.purple,    bg = "NONE" })
  highlight(0, "tsxEqual",                    { fg = colors.white,     bg = "NONE" })
  highlight(0, "tsxString",                   { fg = colors.redLight,  bg = "NONE" })

  -- TSX Expressions
  highlight(0, "tsxEscJs",                    { fg = colors.white,     bg = "NONE" })
  highlight(0, "tsxEscapeJs",                 { fg = colors.white,     bg = "NONE" })

  -- TSX Fragments
  highlight(0, "tsxFragment",                 { fg = colors.white,     bg = "NONE" })

  -- TSX Namespace
  highlight(0, "tsxNameSpace",                { fg = colors.turquoise, bg = "NONE" })

  -- TSX Entities
  highlight(0, "tsxEntity",                   { fg = colors.pink,      bg = "NONE" })
  highlight(0, "tsxEntityPunct",              { fg = colors.pink,      bg = "NONE" })

  -- TSX Regions
  highlight(0, "tsxRegion",                   { fg = "NONE",           bg = "NONE" })


  -----------------------------------------------------------------------------
  -- Treesitter Groups (JSX-specific, non-redundant)

  -- Tag Delimiters (< > /> in JSX) - gray
  highlight(0, "@tag.delimiter.jsx",          { link = "Ignore" })
  highlight(0, "@tag.delimiter.tsx",          { link = "Ignore" })
  highlight(0, "@tag.delimiter.javascript",   { link = "Ignore" })
  highlight(0, "@tag.delimiter.typescript",   { link = "Ignore" })

  -- Tag Names - Built-in HTML elements (lowercase)
  highlight(0, "@tag.builtin.jsx",            { fg = colors.blue,      bg = "NONE" })
  highlight(0, "@tag.builtin.tsx",            { fg = colors.blue,      bg = "NONE" })
  highlight(0, "@tag.builtin.javascript",     { fg = colors.blue,      bg = "NONE" })
  highlight(0, "@tag.builtin.typescript",     { fg = colors.blue,      bg = "NONE" })

  -- Tag Names - React Components (PascalCase)
  highlight(0, "@tag.jsx",                    { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@tag.tsx",                    { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@tag.javascript",             { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@tag.typescript",             { fg = colors.turquoise, bg = "NONE" })

  -- Tag Attributes
  highlight(0, "@tag.attribute.jsx",          { fg = colors.purple,    bg = "NONE" })
  highlight(0, "@tag.attribute.tsx",          { fg = colors.purple,    bg = "NONE" })
  highlight(0, "@tag.attribute.javascript",   { fg = colors.purple,    bg = "NONE" })
  highlight(0, "@tag.attribute.typescript",   { fg = colors.purple,    bg = "NONE" })

  -- Character References (&nbsp;, etc.)
  highlight(0, "@character.special.jsx",      { link = "Constant" })
  highlight(0, "@character.special.tsx",      { link = "Constant" })

  -- JSX-specific punctuation
  highlight(0, "@punctuation.bracket.jsx",    { fg = colors.white,     bg = "NONE" })
  highlight(0, "@punctuation.bracket.tsx",    { fg = colors.white,     bg = "NONE" })

  -- Types (TSX-specific)
  highlight(0, "@type.builtin.tsx",           { fg = colors.blue,      bg = "NONE" })
  highlight(0, "@type.definition.tsx",        { fg = colors.turquoise, bg = "NONE" })


  -----------------------------------------------------------------------------
  -- LSP Semantic Tokens (React-specific)

  -- JavaScript React (JSX)
  highlight(0, "@lsp.type.class.javascriptreact",        { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.interface.javascriptreact",    { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.type.javascriptreact",         { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.enum.javascriptreact",         { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.function.javascriptreact",     { fg = colors.orange,    bg = "NONE" })
  highlight(0, "@lsp.type.method.javascriptreact",       { fg = colors.orange,    bg = "NONE" })
  highlight(0, "@lsp.type.variable.javascriptreact",     { link = "Constant" })
  highlight(0, "@lsp.type.parameter.javascriptreact",    { fg = colors.purple,    bg = "NONE" })
  highlight(0, "@lsp.type.property.javascriptreact",     { fg = colors.purple,    bg = "NONE" })
  highlight(0, "@lsp.type.keyword.javascriptreact",      { fg = colors.blue,      bg = "NONE" })
  highlight(0, "@lsp.type.string.javascriptreact",       { fg = colors.redLight,  bg = "NONE" })
  highlight(0, "@lsp.type.number.javascriptreact",       { fg = colors.greenLight, bg = "NONE" })

  -- TypeScript React (TSX)
  highlight(0, "@lsp.type.class.typescriptreact",        { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.interface.typescriptreact",    { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.type.typescriptreact",         { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.typemod.typeParameter.typescriptreact", { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.enum.typescriptreact",         { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.enumMember.typescriptreact",   { fg = colors.pink,      bg = "NONE" })
  highlight(0, "@lsp.type.function.typescriptreact",     { fg = colors.orange,    bg = "NONE" })
  highlight(0, "@lsp.type.method.typescriptreact",       { fg = colors.orange,    bg = "NONE" })
  highlight(0, "@lsp.type.variable.typescriptreact",     { link = "Constant" })
  highlight(0, "@lsp.type.parameter.typescriptreact",    { fg = colors.purple,    bg = "NONE" })
  highlight(0, "@lsp.type.property.typescriptreact",     { fg = colors.purple,    bg = "NONE" })
  highlight(0, "@lsp.type.keyword.typescriptreact",      { fg = colors.blue,      bg = "NONE" })
  highlight(0, "@lsp.type.namespace.typescriptreact",    { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.type.decorator.typescriptreact",    { fg = colors.pink,      bg = "NONE" })
  highlight(0, "@lsp.type.string.typescriptreact",       { fg = colors.redLight,  bg = "NONE" })
  highlight(0, "@lsp.type.number.typescriptreact",       { fg = colors.greenLight, bg = "NONE" })

  -- LSP Modifiers - JavaScript React
  highlight(0, "@lsp.typemod.variable.readonly.javascriptreact",       { link = "Constant" })
  highlight(0, "@lsp.typemod.function.declaration.javascriptreact",    { link = "Function" })
  highlight(0, "@lsp.typemod.function.defaultLibrary.javascriptreact", { link = "Function" })
  highlight(0, "@lsp.typemod.class.declaration.javascriptreact",       { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.typemod.variable.defaultLibrary.javascriptreact", { link = "Constant" })

  -- LSP Modifiers - TypeScript React
  highlight(0, "@lsp.typemod.variable.readonly.typescriptreact",       { link = "Constant" })
  highlight(0, "@lsp.typemod.function.declaration.typescriptreact",    { link = "Function" })
  highlight(0, "@lsp.typemod.function.defaultLibrary.typescriptreact", { link = "Function" })
  highlight(0, "@lsp.typemod.class.declaration.typescriptreact",       { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.typemod.variable.defaultLibrary.typescriptreact", { link = "Constant" })
  highlight(0, "@lsp.typemod.type.declaration.typescriptreact",        { fg = colors.turquoise, bg = "NONE" })
  highlight(0, "@lsp.typemod.interface.declaration.typescriptreact",   { fg = colors.turquoise, bg = "NONE" })
end

return react
