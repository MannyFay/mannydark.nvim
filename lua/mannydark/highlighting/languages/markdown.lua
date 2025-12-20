-------------------------------------------------------------------------------
-- Markdown
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local markdown  = {}


--------------------------------------------------------------
-- Settings

markdown.setupHighlighting = function()

  ---------------------------------------------------------------
  -- Vim Legacy Syntax Groups (markdown.vim)
  ---------------------------------------------------------------

  -- Headings
  highlight(0, 'markdownH1',                    { link = "Label", bold = true })
  highlight(0, 'markdownH2',                    { link = "Label", bold = true })
  highlight(0, 'markdownH3',                    { link = "Label", bold = true })
  highlight(0, 'markdownH4',                    { link = "Label", bold = true })
  highlight(0, 'markdownH5',                    { link = "Label", bold = true })
  highlight(0, 'markdownH6',                    { link = "Label", bold = true })
  highlight(0, 'markdownH1Delimiter',           { link = "Delimiter" })
  highlight(0, 'markdownH2Delimiter',           { link = "Delimiter" })
  highlight(0, 'markdownH3Delimiter',           { link = "Delimiter" })
  highlight(0, 'markdownH4Delimiter',           { link = "Delimiter" })
  highlight(0, 'markdownH5Delimiter',           { link = "Delimiter" })
  highlight(0, 'markdownH6Delimiter',           { link = "Delimiter" })
  highlight(0, 'markdownHeadingDelimiter',      { link = "Delimiter" })
  highlight(0, 'markdownHeadingRule',           { fg = colors.blue,       bg = 'NONE' })

  -- Text Formatting
  highlight(0, 'markdownBold',                  { link = "Normal" })
  highlight(0, 'markdownBoldDelimiter',         { link = "Keyword" })
  highlight(0, 'markdownItalic',                { link = "Normal"})
  highlight(0, 'markdownItalicDelimiter',       { link = "Keyword" })
  highlight(0, 'markdownBoldItalic',            { link = "Normal" })
  highlight(0, 'markdownBoldItalicDelimiter',   { link = "Keyword" })
  highlight(0, 'markdownStrike',                { link = "Normal"})
  highlight(0, 'markdownStrikeDelimiter',       { link = "Keyword" })

  -- Code
  highlight(0, 'markdownCode',                  { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'markdownCodeBlock',             { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'markdownCodeDelimiter',         { link = "Keyword" })
  highlight(0, 'markdownCodeError',             { fg = colors.red,        bg = 'NONE' })

  -- Links
  highlight(0, 'markdownUrl',                   { link = "Underlined" })
  highlight(0, 'markdownUrlDelimiter',          { link = "Delimiter" })
  highlight(0, 'markdownUrlTitle',              { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, 'markdownUrlTitleDelimiter',     { link = "Delimiter" })
  highlight(0, 'markdownLink',                  { link = "Normal" })
  highlight(0, 'markdownLinkDelimiter',         { link = "Delimiter" })
  highlight(0, 'markdownLinkText',              { link = "Normal" })
  highlight(0, 'markdownLinkTextDelimiter',     { link = "Delimiter" })
  highlight(0, 'markdownAutomaticLink',         { link = "Underlined" })

  -- References
  highlight(0, 'markdownId',                    { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'markdownIdDelimiter',           { link = "Delimiter" })
  highlight(0, 'markdownIdDeclaration',         { fg = colors.purple,     bg = 'NONE' })

  -- Lists
  highlight(0, 'markdownListMarker',            { link = "Keyword" })
  highlight(0, 'markdownOrderedListMarker',     { link = "Keyword" })

  -- Block Elements
  highlight(0, 'markdownBlockquote',            { link = "Normal" })
  highlight(0, 'markdownRule',                  { link = "Keyword" })
  highlight(0, 'markdownLineBreak',             { link = "Keyword" })

  -- Footnotes
  highlight(0, 'markdownFootnote',              { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'markdownFootnoteDefinition',    { fg = colors.purple,     bg = 'NONE' })

  -- Special
  highlight(0, 'markdownEscape',                { link = "Special" })
  highlight(0, 'markdownError',                 { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'markdownValid',                 { fg = colors.green,      bg = 'NONE' })

  -- YAML Front Matter
  highlight(0, 'markdownYamlHead',              { fg = colors.blue,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- vim-markdown (plasticboy/preservim) Syntax Groups
  ---------------------------------------------------------------

  -- Headings
  highlight(0, 'mkdHeading',                    { link = "Label", bold = true })

  -- Text Formatting
  highlight(0, 'mkdBold',                       { link = "Normal" })
  highlight(0, 'mkdItalic',                     { link = "Normal" })
  highlight(0, 'mkdBoldItalic',                 { link = "Normal" })
  highlight(0, 'mkdStrike',                     { link = "Normal" })

  -- Code
  highlight(0, 'mkdCode',                       { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'mkdCodeDelimiter',              { link = "Delimiter" })
  highlight(0, 'mkdCodeStart',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdCodeEnd',                    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdSnippet',                    { fg = colors.green,      bg = 'NONE' })

  -- Links
  highlight(0, 'mkdLink',                       { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'mkdURL',                        { fg = colors.blueLink,   bg = 'NONE', underline = true })
  highlight(0, 'mkdInlineURL',                  { fg = colors.blueLink,   bg = 'NONE', underline = true })
  highlight(0, 'mkdLinkDef',                    { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'mkdLinkDefTarget',              { fg = colors.blueLink,   bg = 'NONE', underline = true })
  highlight(0, 'mkdLinkTitle',                  { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, 'mkdDelimiter',                  { link = "Delimiter" })

  -- References & IDs
  highlight(0, 'mkdID',                         { fg = colors.purple,     bg = 'NONE' })

  -- Lists
  highlight(0, 'mkdListItem',                   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdListItemLine',               { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'mkdListItemCheckbox',           { fg = colors.blue,       bg = 'NONE' })

  -- Block Elements
  highlight(0, 'mkdBlockquote',                 { link = "Normal" })
  highlight(0, 'mkdRule',                       { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdLineBreak',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdNonListItemBlock',           { fg = colors.white,      bg = 'NONE' })

  -- Footnotes
  highlight(0, 'mkdFootnote',                   { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'mkdFootnotes',                  { fg = colors.purple,     bg = 'NONE' })

  -- Math
  highlight(0, 'mkdMath',                       { fg = colors.green,      bg = 'NONE' })

  -- Strings
  highlight(0, 'mkdString',                     { link = "String" })

  ---------------------------------------------------------------
  -- Treesitter Captures (markdown)
  ---------------------------------------------------------------

  -- Headings
  highlight(0, '@markup.heading.markdown',              { link = "Label" })
  highlight(0, '@markup.heading.1.markdown',            { link = "Label", bold = true })
  highlight(0, '@markup.heading.2.markdown',            { link = "Label", bold = true })
  highlight(0, '@markup.heading.3.markdown',            { link = "Label", bold = true })
  highlight(0, '@markup.heading.4.markdown',            { link = "Label", bold = true })
  highlight(0, '@markup.heading.5.markdown',            { link = "Label", bold = true })
  highlight(0, '@markup.heading.6.markdown',            { link = "Label", bold = true })
  highlight(0, '@markup.heading.marker.markdown',       { link = "Label" })

  -- Code
  highlight(0, '@markup.raw.markdown',                  { link = "String" })
  highlight(0, '@markup.raw.block.markdown',            { link = "Keyword" })
  highlight(0, '@markup.raw.delimiter.markdown',        { link = "Keyword" })

  -- Links
  highlight(0, '@markup.link.markdown',                 { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@markup.link.label.markdown',           { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@markup.link.url.markdown',             { fg = colors.blueLink,   bg = 'NONE', underline = true })

  -- Lists
  highlight(0, '@markup.list.markdown',                 { link = "Keyword" })
  highlight(0, '@markup.list.checked.markdown',         { link = "Keyword" })
  highlight(0, '@markup.list.unchecked.markdown',       { link = "Keyword" })

  -- Block Elements
  highlight(0, '@markup.quote.markdown',                { link = "Normal" })

  -- Info String (language identifier in code blocks)
  highlight(0, '@label.markdown',                       { fg = colors.turquoise,  bg = 'NONE' })

  -- Punctuation
  highlight(0, '@punctuation.delimiter.markdown',       { link = "Delimiter" })
  highlight(0, '@punctuation.special.markdown',         { fg = colors.blue,       bg = 'NONE' })

  -- Directives
  highlight(0, '@keyword.directive.markdown',           { link = "Keyword" })

  -- Escapes
  highlight(0, '@string.escape.markdown',               { link = "String" })

  -- Spell
  highlight(0, '@spell.markdown',                       { fg = 'NONE',            bg = 'NONE' })
  highlight(0, '@nospell.markdown',                     { fg = 'NONE',            bg = 'NONE' })

  -- Conceal
  highlight(0, '@conceal.markdown',                     { fg = colors.grey,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- Treesitter Captures (markdown_inline)
  ---------------------------------------------------------------

  -- Text Formatting
  highlight(0, '@markup.strong.markdown_inline',        { link = "Keyword" })
  highlight(0, '@markup.italic.markdown_inline',        { link = "Keyword" })
  highlight(0, '@markup.strikethrough.markdown_inline', { link = "Keyword" })

  -- Code
  highlight(0, '@markup.raw.markdown_inline',           { fg = colors.green,      bg = 'NONE' })
  highlight(0, '@markup.raw.delimiter.markdown_inline', { link = "Keyword" })

  -- Links
  highlight(0, '@markup.link.markdown_inline',          { link = "Label" }) -- Brackets and braces around links
  highlight(0, '@markup.link.label.markdown_inline',    { link = "Normal" })
  highlight(0, '@markup.link.url.markdown_inline',      { link = "Normal", underline = true })

  -- Images
  highlight(0, '@markup.image.markdown_inline',         { fg = colors.purple,     bg = 'NONE' })

  -- Escapes & Special
  highlight(0, '@string.escape.markdown_inline',        { link = "Special" })
  highlight(0, '@character.special.markdown_inline',    { fg = colors.purple,     bg = 'NONE' })

  -- Punctuation
  highlight(0, '@punctuation.delimiter.markdown_inline', { link = "Delimiter" })
  highlight(0, '@punctuation.bracket.markdown_inline',   { fg = colors.white,     bg = 'NONE' })
  highlight(0, '@punctuation.special.markdown_inline',   { fg = colors.blue,      bg = 'NONE' })

  -- Spell
  highlight(0, '@spell.markdown_inline',                { fg = 'NONE',            bg = 'NONE' })
  highlight(0, '@nospell.markdown_inline',              { fg = 'NONE',            bg = 'NONE' })

  -- Conceal
  highlight(0, '@conceal.markdown_inline',              { fg = colors.grey,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- HTML in Markdown (embedded)
  ---------------------------------------------------------------

  highlight(0, '@tag.markdown',                         { link = "Keyword" })
  highlight(0, '@tag.delimiter.markdown',               { link = "Ignore" })
  highlight(0, '@tag.attribute.markdown',               { link = "Type" })

  ---------------------------------------------------------------
  -- Tables (GitHub Flavored Markdown)
  ---------------------------------------------------------------

  highlight(0, 'markdownTable',                         { link = "Normal" })
  highlight(0, 'markdownTableDelimiter',                { link = "Keyword" })
  highlight(0, 'markdownTableHeader',                   { link = "Label" })
  highlight(0, 'markdownTableRow',                      { link = "Normal" })
  highlight(0, 'markdownTableCell',                     { link = "Normal" })
  highlight(0, 'markdownTableAlign',                    { link = "Keyword" })
  highlight(0, 'markdownTableSeparator',                { link = "Keyword" })

  -- Treesitter table captures
  highlight(0, '@markup.heading.table.markdown',        { link = "Label" })
  highlight(0, '@punctuation.special.table.markdown',   { link = "Keyword" })

  ---------------------------------------------------------------
  -- Task Lists (GitHub Flavored Markdown)
  ---------------------------------------------------------------

  highlight(0, 'markdownCheckbox',                      { link = "Keyword" })
  highlight(0, 'markdownCheckboxChecked',               { link = "Keyword" })
  highlight(0, 'markdownCheckboxUnchecked',             { link = "Keyword" })

  ---------------------------------------------------------------
  -- Alerts/Callouts (GitHub Flavored Markdown)
  ---------------------------------------------------------------

  highlight(0, 'markdownAlert',                         { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownAlertNote',                     { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'markdownAlertTip',                      { fg = colors.green,      bg = 'NONE', bold = true })
  highlight(0, 'markdownAlertImportant',                { fg = colors.purple,     bg = 'NONE', bold = true })
  highlight(0, 'markdownAlertWarning',                  { fg = colors.yellow,     bg = 'NONE', bold = true })
  highlight(0, 'markdownAlertCaution',                  { fg = colors.red,        bg = 'NONE', bold = true })

  ---------------------------------------------------------------
  -- render-markdown.nvim Plugin Groups
  ---------------------------------------------------------------

  -- Headings
  highlight(0, 'RenderMarkdownH1',                      { link = "Label" })
  highlight(0, 'RenderMarkdownH2',                      { link = "Label" })
  highlight(0, 'RenderMarkdownH3',                      { link = "Label" })
  highlight(0, 'RenderMarkdownH4',                      { link = "Label" })
  highlight(0, 'RenderMarkdownH5',                      { link = "Label" })
  highlight(0, 'RenderMarkdownH6',                      { link = "Label" })
  highlight(0, 'RenderMarkdownH1Bg',                    { link = "Label" })
  highlight(0, 'RenderMarkdownH2Bg',                    { link = "Label" })
  highlight(0, 'RenderMarkdownH3Bg',                    { link = "Label" })
  highlight(0, 'RenderMarkdownH4Bg',                    { link = "Label" })
  highlight(0, 'RenderMarkdownH5Bg',                    { link = "Label" })
  highlight(0, 'RenderMarkdownH6Bg',                    { link = "Label" })

  -- Code
  highlight(0, 'RenderMarkdownCode',                    { fg = 'NONE',            bg = colors.backgroundLight })
  highlight(0, 'RenderMarkdownCodeInline',              { fg = colors.green,      bg = 'NONE' })

  -- Bullets
  highlight(0, 'RenderMarkdownBullet',                  { link = "Keyword" })

  -- Checkboxes
  highlight(0, 'RenderMarkdownChecked',                 { link = "Keyword" })
  highlight(0, 'RenderMarkdownUnchecked',               { link = "Keyword" })
  highlight(0, 'RenderMarkdownTodo',                    { fg = colors.yellow,     bg = 'NONE' })

  -- Quotes
  highlight(0, 'RenderMarkdownQuote',                   { link = "Normal" })
  highlight(0, 'RenderMarkdownQuote1',                  { link = "Normal" })
  highlight(0, 'RenderMarkdownQuote2',                  { link = "Normal" })
  highlight(0, 'RenderMarkdownQuote3',                  { link = "Normal" })
  highlight(0, 'RenderMarkdownQuote4',                  { link = "Normal" })
  highlight(0, 'RenderMarkdownQuote5',                  { link = "Normal" })
  highlight(0, 'RenderMarkdownQuote6',                  { link = "Normal" })

  -- Horizontal Rules
  highlight(0, 'RenderMarkdownDash',                    { fg = colors.blue,       bg = 'NONE' })

  -- Links
  highlight(0, 'RenderMarkdownLink',                    { fg = colors.blueLink,   bg = 'NONE', underline = true })

  -- Tables
  highlight(0, 'RenderMarkdownTableHead',               { link = "Label" })
  highlight(0, 'RenderMarkdownTableRow',                { link = "Normal" })
  highlight(0, 'RenderMarkdownTableFill',               { fg = colors.grey,       bg = 'NONE' })

  -- Math
  highlight(0, 'RenderMarkdownMath',                    { fg = colors.green,      bg = 'NONE' })

  -- Callouts (render-markdown.nvim)
  highlight(0, 'RenderMarkdownInfo',                    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'RenderMarkdownSuccess',                 { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'RenderMarkdownHint',                    { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'RenderMarkdownWarn',                    { fg = colors.yellow,     bg = 'NONE' })
  highlight(0, 'RenderMarkdownError',                   { fg = colors.red,        bg = 'NONE' })

  ---------------------------------------------------------------
  -- markview.nvim Plugin Groups
  ---------------------------------------------------------------

  -- Headings
  highlight(0, 'MarkviewHeading1',                      { link = "Label" })
  highlight(0, 'MarkviewHeading2',                      { link = "Label" })
  highlight(0, 'MarkviewHeading3',                      { link = "Label" })
  highlight(0, 'MarkviewHeading4',                      { link = "Label" })
  highlight(0, 'MarkviewHeading5',                      { link = "Label" })
  highlight(0, 'MarkviewHeading6',                      { link = "Label" })
  highlight(0, 'MarkviewHeading1Sign',                  { link = "Label" })
  highlight(0, 'MarkviewHeading2Sign',                  { link = "Label" })
  highlight(0, 'MarkviewHeading3Sign',                  { link = "Label" })
  highlight(0, 'MarkviewHeading4Sign',                  { link = "Label" })
  highlight(0, 'MarkviewHeading5Sign',                  { link = "Label" })
  highlight(0, 'MarkviewHeading6Sign',                  { link = "Label" })

  -- Code
  highlight(0, 'MarkviewCode',                          { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'MarkviewCodeInfo',                      { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'MarkviewInlineCode',                    { fg = colors.green,      bg = 'NONE' })

  -- Block Quotes
  highlight(0, 'MarkviewBlockQuoteDefault',             { link = "Normal" })
  highlight(0, 'MarkviewBlockQuoteOk',                  { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteWarn',                { fg = colors.yellow,     bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteError',               { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteNote',                { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteSpecial',             { fg = colors.purple,     bg = 'NONE' })

  -- Checkboxes
  highlight(0, 'MarkviewCheckboxChecked',               { link = "Keyword" })
  highlight(0, 'MarkviewCheckboxUnchecked',             { link = "Keyword" })
  highlight(0, 'MarkviewCheckboxPending',               { fg = colors.yellow,     bg = 'NONE' })
  highlight(0, 'MarkviewCheckboxProgress',              { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewCheckboxCancelled',             { fg = colors.red,        bg = 'NONE', strikethrough = true })

  -- Lists
  highlight(0, 'MarkviewListItemMinus',                 { link = "Keyword" })
  highlight(0, 'MarkviewListItemPlus',                  { link = "Keyword" })
  highlight(0, 'MarkviewListItemStar',                  { link = "Keyword" })

  -- Tables
  highlight(0, 'MarkviewTableHeader',                   { link = "Label" })
  highlight(0, 'MarkviewTableBorder',                   { link = "Keyword" })
  highlight(0, 'MarkviewTableAlignLeft',                { link = "Keyword" })
  highlight(0, 'MarkviewTableAlignRight',               { link = "Keyword" })
  highlight(0, 'MarkviewTableAlignCenter',              { link = "Keyword" })

  -- Horizontal Rules
  highlight(0, 'MarkviewGradient1',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient2',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient3',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient4',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient5',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient6',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient7',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient8',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient9',                     { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewGradient10',                    { fg = colors.grey,       bg = 'NONE' })

  -- Links
  highlight(0, 'MarkviewHyperlink',                     { fg = colors.blueLink,   bg = 'NONE', underline = true })
  highlight(0, 'MarkviewImageLink',                     { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'MarkviewEmail',                         { fg = colors.blueLink,   bg = 'NONE', underline = true })

  -- Latex
  highlight(0, 'MarkviewLatex',                         { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'MarkviewLatexSubscript',                { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'MarkviewLatexSuperscript',              { fg = colors.green,      bg = 'NONE' })

  ---------------------------------------------------------------
  -- headlines.nvim Plugin Groups
  ---------------------------------------------------------------

  highlight(0, 'Headline',                              { link = "Label" })
  highlight(0, 'Headline1',                             { link = "Label" })
  highlight(0, 'Headline2',                             { link = "Label" })
  highlight(0, 'Headline3',                             { link = "Label" })
  highlight(0, 'Headline4',                             { link = "Label" })
  highlight(0, 'Headline5',                             { link = "Label" })
  highlight(0, 'Headline6',                             { link = "Label" })
  highlight(0, 'CodeBlock',                             { fg = 'NONE',            bg = colors.backgroundLight })
  highlight(0, 'Dash',                                  { link = "Keyword" })
  highlight(0, 'DoubleDash',                            { link = "Keyword" })
  highlight(0, 'Quote',                                 { link = "Normal" })

  ---------------------------------------------------------------
  -- nvim-cmp Markdown Completions
  ---------------------------------------------------------------

  highlight(0, 'CmpItemKindMarkdown',                   { fg = colors.blue,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- LSP Semantic Tokens for Markdown
  ---------------------------------------------------------------

  highlight(0, '@lsp.type.class.markdown',              { fg = colors.blue,       bg = 'NONE', bold = true })

  ---------------------------------------------------------------
  -- MDX (Markdown with JSX) Support
  ---------------------------------------------------------------

  -- MDX-specific captures
  highlight(0, '@tag.mdx',                              { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@tag.delimiter.mdx',                    { link = "Delimiter" })
  highlight(0, '@tag.attribute.mdx',                    { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@punctuation.bracket.mdx',              { fg = colors.white,      bg = 'NONE' })

  -- JSX in MDX
  highlight(0, '@keyword.import.mdx',                   { link = "Keyword" })
  highlight(0, '@keyword.export.mdx',                   { link = "Keyword" })
  highlight(0, '@variable.mdx',                         { link = "Variable" })
  highlight(0, '@function.mdx',                         { link = "Function" })
  highlight(0, '@string.mdx',                           { link = "String" })

  ---------------------------------------------------------------
  -- Obsidian-specific Markdown Syntax
  ---------------------------------------------------------------

  -- Wiki-style links
  highlight(0, 'ObsidianLink',                          { fg = colors.blueLink,   bg = 'NONE', underline = true })
  highlight(0, 'ObsidianLinkText',                      { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'ObsidianLinkAlias',                     { fg = colors.purple,     bg = 'NONE' })

  -- Tags
  highlight(0, 'ObsidianTag',                           { fg = colors.turquoise,  bg = 'NONE' })

  -- Highlights
  highlight(0, 'ObsidianHighlight',                     { fg = colors.black,      bg = colors.yellow })

  -- Block references
  highlight(0, 'ObsidianBlockRef',                      { fg = colors.purple,     bg = 'NONE', italic = true })

  -- Embeds
  highlight(0, 'ObsidianEmbed',                         { fg = colors.purple,     bg = 'NONE' })

  -- Comments
  highlight(0, 'ObsidianComment',                       { link = "Comment" })

  -- Callouts/Admonitions
  highlight(0, 'ObsidianCallout',                       { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'ObsidianCalloutNote',                   { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutAbstract',               { fg = colors.turquoise,  bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutInfo',                   { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutTodo',                   { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutTip',                    { fg = colors.green,      bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutSuccess',                { fg = colors.green,      bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutQuestion',               { fg = colors.yellow,     bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutWarning',                { fg = colors.yellow,     bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutFailure',                { fg = colors.red,        bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutDanger',                 { fg = colors.red,        bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutBug',                    { fg = colors.red,        bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutExample',                { fg = colors.purple,     bg = 'NONE', bold = true })
  highlight(0, 'ObsidianCalloutQuote',                  { fg = colors.orange,     bg = 'NONE', bold = true })

  ---------------------------------------------------------------
  -- obsidian.nvim Plugin Groups
  ---------------------------------------------------------------

  highlight(0, 'ObsidianRefText',                       { fg = colors.purple,     bg = 'NONE', underline = true })
  highlight(0, 'ObsidianExtLinkIcon',                   { fg = colors.blueLink,   bg = 'NONE' })
  highlight(0, 'ObsidianDone',                          { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'ObsidianRightArrow',                    { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'ObsidianTilde',                         { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'ObsidianBullet',                        { fg = colors.blue,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- Mermaid Diagrams (in Markdown code blocks)
  ---------------------------------------------------------------

  highlight(0, 'mermaidGraph',                          { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mermaidNode',                           { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'mermaidEdge',                           { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'mermaidLabel',                          { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, 'mermaidKeyword',                        { link = "Keyword" })
  highlight(0, 'mermaidComment',                        { link = "Comment" })

  ---------------------------------------------------------------
  -- Latex/Math in Markdown
  ---------------------------------------------------------------

  highlight(0, 'markdownMath',                          { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'markdownMathDelimiter',                 { link = "Delimiter" })
  highlight(0, '@markup.math.markdown',                 { fg = colors.green,      bg = 'NONE' })
  highlight(0, '@markup.math.markdown_inline',          { fg = colors.green,      bg = 'NONE' })

  ---------------------------------------------------------------
  -- YAML Front Matter (in Markdown)
  ---------------------------------------------------------------

  highlight(0, '@keyword.yaml.frontmatter',             { link = "Keyword" })
  highlight(0, '@property.yaml.frontmatter',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@string.yaml.frontmatter',              { link = "String" })
  highlight(0, '@number.yaml.frontmatter',              { link = "Number" })

  ---------------------------------------------------------------
  -- TOML Front Matter (in Markdown - Hugo style)
  ---------------------------------------------------------------

  highlight(0, '@keyword.toml.frontmatter',             { link = "Keyword" })
  highlight(0, '@property.toml.frontmatter',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@string.toml.frontmatter',              { link = "String" })
  highlight(0, '@number.toml.frontmatter',              { link = "Number" })

end

return markdown
