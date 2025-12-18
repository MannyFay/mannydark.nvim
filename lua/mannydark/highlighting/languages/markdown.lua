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
  highlight(0, 'markdownH1',                    { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'markdownH2',                    { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'markdownH3',                    { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'markdownH4',                    { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'markdownH5',                    { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'markdownH6',                    { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'markdownH1Delimiter',           { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownH2Delimiter',           { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownH3Delimiter',           { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownH4Delimiter',           { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownH5Delimiter',           { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownH6Delimiter',           { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownHeadingDelimiter',      { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownHeadingRule',           { fg = colors.blue,       bg = 'NONE' })

  -- Text Formatting
  highlight(0, 'markdownBold',                  { fg = colors.white,      bg = 'NONE', bold = true })
  highlight(0, 'markdownBoldDelimiter',         { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownItalic',                { fg = colors.white,      bg = 'NONE', italic = true })
  highlight(0, 'markdownItalicDelimiter',       { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownBoldItalic',            { fg = colors.white,      bg = 'NONE', bold = true, italic = true })
  highlight(0, 'markdownBoldItalicDelimiter',   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownStrike',                { fg = colors.grey,       bg = 'NONE', strikethrough = true })
  highlight(0, 'markdownStrikeDelimiter',       { fg = colors.blue,       bg = 'NONE' })

  -- Code
  highlight(0, 'markdownCode',                  { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'markdownCodeBlock',             { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'markdownCodeDelimiter',         { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownCodeError',             { fg = colors.red,        bg = 'NONE' })

  -- Links
  highlight(0, 'markdownUrl',                   { fg = colors.blueLink,   bg = 'NONE', underline = true })
  highlight(0, 'markdownUrlDelimiter',          { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'markdownUrlTitle',              { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, 'markdownUrlTitleDelimiter',     { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'markdownLink',                  { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'markdownLinkDelimiter',         { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'markdownLinkText',              { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'markdownLinkTextDelimiter',     { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'markdownAutomaticLink',         { fg = colors.blueLink,   bg = 'NONE', underline = true })

  -- References
  highlight(0, 'markdownId',                    { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'markdownIdDelimiter',           { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'markdownIdDeclaration',         { fg = colors.purple,     bg = 'NONE' })

  -- Lists
  highlight(0, 'markdownListMarker',            { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownOrderedListMarker',     { fg = colors.blue,       bg = 'NONE' })

  -- Block Elements
  highlight(0, 'markdownBlockquote',            { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'markdownRule',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownLineBreak',             { fg = colors.blue,       bg = 'NONE' })

  -- Footnotes
  highlight(0, 'markdownFootnote',              { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'markdownFootnoteDefinition',    { fg = colors.purple,     bg = 'NONE' })

  -- Special
  highlight(0, 'markdownEscape',                { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'markdownError',                 { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'markdownValid',                 { fg = colors.green,      bg = 'NONE' })

  -- YAML Front Matter
  highlight(0, 'markdownYamlHead',              { fg = colors.blue,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- vim-markdown (plasticboy/preservim) Syntax Groups
  ---------------------------------------------------------------

  -- Headings
  highlight(0, 'mkdHeading',                    { fg = colors.blue,       bg = 'NONE' })

  -- Text Formatting
  highlight(0, 'mkdBold',                       { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdItalic',                     { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdBoldItalic',                 { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdStrike',                     { fg = colors.grey,       bg = 'NONE', strikethrough = true })

  -- Code
  highlight(0, 'mkdCode',                       { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'mkdCodeDelimiter',              { fg = colors.blue,       bg = 'NONE' })
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
  highlight(0, 'mkdDelimiter',                  { fg = colors.white,      bg = 'NONE' })

  -- References & IDs
  highlight(0, 'mkdID',                         { fg = colors.purple,     bg = 'NONE' })

  -- Lists
  highlight(0, 'mkdListItem',                   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdListItemLine',               { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'mkdListItemCheckbox',           { fg = colors.blue,       bg = 'NONE' })

  -- Block Elements
  highlight(0, 'mkdBlockquote',                 { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'mkdRule',                       { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdLineBreak',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mkdNonListItemBlock',           { fg = colors.white,      bg = 'NONE' })

  -- Footnotes
  highlight(0, 'mkdFootnote',                   { fg = colors.purple,     bg = 'NONE' })
  highlight(0, 'mkdFootnotes',                  { fg = colors.purple,     bg = 'NONE' })

  -- Math
  highlight(0, 'mkdMath',                       { fg = colors.green,      bg = 'NONE' })

  -- Strings
  highlight(0, 'mkdString',                     { fg = colors.redLight,   bg = 'NONE' })

  ---------------------------------------------------------------
  -- Treesitter Captures (markdown)
  ---------------------------------------------------------------

  -- Headings
  highlight(0, '@markup.heading.markdown',              { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@markup.heading.1.markdown',            { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@markup.heading.2.markdown',            { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@markup.heading.3.markdown',            { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@markup.heading.4.markdown',            { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@markup.heading.5.markdown',            { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@markup.heading.6.markdown',            { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@markup.heading.marker.markdown',       { fg = colors.blue,       bg = 'NONE' })

  -- Code
  highlight(0, '@markup.raw.markdown',                  { fg = colors.green,      bg = 'NONE' })
  highlight(0, '@markup.raw.block.markdown',            { fg = colors.green,      bg = 'NONE' })
  highlight(0, '@markup.raw.delimiter.markdown',        { fg = colors.blue,       bg = 'NONE' })

  -- Links
  highlight(0, '@markup.link.markdown',                 { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@markup.link.label.markdown',           { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@markup.link.url.markdown',             { fg = colors.blueLink,   bg = 'NONE', underline = true })

  -- Lists
  highlight(0, '@markup.list.markdown',                 { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@markup.list.checked.markdown',         { fg = colors.green,      bg = 'NONE' })
  highlight(0, '@markup.list.unchecked.markdown',       { fg = colors.grey,       bg = 'NONE' })

  -- Block Elements
  highlight(0, '@markup.quote.markdown',                { fg = colors.orange,     bg = 'NONE' })

  -- Info String (language identifier in code blocks)
  highlight(0, '@label.markdown',                       { fg = colors.turquoise,  bg = 'NONE' })

  -- Punctuation
  highlight(0, '@punctuation.delimiter.markdown',       { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@punctuation.special.markdown',         { fg = colors.blue,       bg = 'NONE' })

  -- Directives
  highlight(0, '@keyword.directive.markdown',           { fg = colors.blue,       bg = 'NONE' })

  -- Escapes
  highlight(0, '@string.escape.markdown',               { fg = colors.purple,     bg = 'NONE' })

  -- Spell
  highlight(0, '@spell.markdown',                       { fg = 'NONE',            bg = 'NONE' })
  highlight(0, '@nospell.markdown',                     { fg = 'NONE',            bg = 'NONE' })

  -- Conceal
  highlight(0, '@conceal.markdown',                     { fg = colors.grey,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- Treesitter Captures (markdown_inline)
  ---------------------------------------------------------------

  -- Text Formatting
  highlight(0, '@markup.strong.markdown_inline',        { fg = colors.white,      bg = 'NONE', bold = true })
  highlight(0, '@markup.italic.markdown_inline',        { fg = colors.white,      bg = 'NONE', italic = true })
  highlight(0, '@markup.strikethrough.markdown_inline', { fg = colors.grey,       bg = 'NONE', strikethrough = true })

  -- Code
  highlight(0, '@markup.raw.markdown_inline',           { fg = colors.green,      bg = 'NONE' })
  highlight(0, '@markup.raw.delimiter.markdown_inline', { fg = colors.blue,       bg = 'NONE' })

  -- Links
  highlight(0, '@markup.link.markdown_inline',          { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@markup.link.label.markdown_inline',    { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@markup.link.url.markdown_inline',      { fg = colors.blueLink,   bg = 'NONE', underline = true })

  -- Images
  highlight(0, '@markup.image.markdown_inline',         { fg = colors.purple,     bg = 'NONE' })

  -- Escapes & Special
  highlight(0, '@string.escape.markdown_inline',        { fg = colors.purple,     bg = 'NONE' })
  highlight(0, '@character.special.markdown_inline',    { fg = colors.purple,     bg = 'NONE' })

  -- Punctuation
  highlight(0, '@punctuation.delimiter.markdown_inline', { fg = colors.white,     bg = 'NONE' })
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

  highlight(0, '@tag.markdown',                         { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@tag.delimiter.markdown',               { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@tag.attribute.markdown',               { fg = colors.turquoise,  bg = 'NONE' })

  ---------------------------------------------------------------
  -- Tables (GitHub Flavored Markdown)
  ---------------------------------------------------------------

  highlight(0, 'markdownTable',                         { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'markdownTableDelimiter',                { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownTableHeader',                   { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'markdownTableRow',                      { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'markdownTableCell',                     { fg = colors.white,      bg = 'NONE' })
  highlight(0, 'markdownTableAlign',                    { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownTableSeparator',                { fg = colors.blue,       bg = 'NONE' })

  -- Treesitter table captures
  highlight(0, '@markup.heading.table.markdown',        { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, '@punctuation.special.table.markdown',   { fg = colors.blue,       bg = 'NONE' })

  ---------------------------------------------------------------
  -- Task Lists (GitHub Flavored Markdown)
  ---------------------------------------------------------------

  highlight(0, 'markdownCheckbox',                      { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'markdownCheckboxChecked',               { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'markdownCheckboxUnchecked',             { fg = colors.grey,       bg = 'NONE' })

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
  highlight(0, 'RenderMarkdownH1',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'RenderMarkdownH2',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'RenderMarkdownH3',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'RenderMarkdownH4',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'RenderMarkdownH5',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'RenderMarkdownH6',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'RenderMarkdownH1Bg',                    { fg = colors.blue,       bg = colors.backgroundLight })
  highlight(0, 'RenderMarkdownH2Bg',                    { fg = colors.blue,       bg = colors.backgroundLight })
  highlight(0, 'RenderMarkdownH3Bg',                    { fg = colors.blue,       bg = colors.backgroundLight })
  highlight(0, 'RenderMarkdownH4Bg',                    { fg = colors.blue,       bg = colors.backgroundLight })
  highlight(0, 'RenderMarkdownH5Bg',                    { fg = colors.blue,       bg = colors.backgroundLight })
  highlight(0, 'RenderMarkdownH6Bg',                    { fg = colors.blue,       bg = colors.backgroundLight })

  -- Code
  highlight(0, 'RenderMarkdownCode',                    { fg = 'NONE',            bg = colors.backgroundLight })
  highlight(0, 'RenderMarkdownCodeInline',              { fg = colors.green,      bg = 'NONE' })

  -- Bullets
  highlight(0, 'RenderMarkdownBullet',                  { fg = colors.blue,       bg = 'NONE' })

  -- Checkboxes
  highlight(0, 'RenderMarkdownChecked',                 { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'RenderMarkdownUnchecked',               { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'RenderMarkdownTodo',                    { fg = colors.yellow,     bg = 'NONE' })

  -- Quotes
  highlight(0, 'RenderMarkdownQuote',                   { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'RenderMarkdownQuote1',                  { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'RenderMarkdownQuote2',                  { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'RenderMarkdownQuote3',                  { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'RenderMarkdownQuote4',                  { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'RenderMarkdownQuote5',                  { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'RenderMarkdownQuote6',                  { fg = colors.orange,     bg = 'NONE' })

  -- Horizontal Rules
  highlight(0, 'RenderMarkdownDash',                    { fg = colors.blue,       bg = 'NONE' })

  -- Links
  highlight(0, 'RenderMarkdownLink',                    { fg = colors.blueLink,   bg = 'NONE', underline = true })

  -- Tables
  highlight(0, 'RenderMarkdownTableHead',               { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'RenderMarkdownTableRow',                { fg = colors.white,      bg = 'NONE' })
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
  highlight(0, 'MarkviewHeading1',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'MarkviewHeading2',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'MarkviewHeading3',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'MarkviewHeading4',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'MarkviewHeading5',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'MarkviewHeading6',                      { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'MarkviewHeading1Sign',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewHeading2Sign',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewHeading3Sign',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewHeading4Sign',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewHeading5Sign',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewHeading6Sign',                  { fg = colors.blue,       bg = 'NONE' })

  -- Code
  highlight(0, 'MarkviewCode',                          { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'MarkviewCodeInfo',                      { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, 'MarkviewInlineCode',                    { fg = colors.green,      bg = 'NONE' })

  -- Block Quotes
  highlight(0, 'MarkviewBlockQuoteDefault',             { fg = colors.orange,     bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteOk',                  { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteWarn',                { fg = colors.yellow,     bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteError',               { fg = colors.red,        bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteNote',                { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewBlockQuoteSpecial',             { fg = colors.purple,     bg = 'NONE' })

  -- Checkboxes
  highlight(0, 'MarkviewCheckboxChecked',               { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'MarkviewCheckboxUnchecked',             { fg = colors.grey,       bg = 'NONE' })
  highlight(0, 'MarkviewCheckboxPending',               { fg = colors.yellow,     bg = 'NONE' })
  highlight(0, 'MarkviewCheckboxProgress',              { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewCheckboxCancelled',             { fg = colors.red,        bg = 'NONE', strikethrough = true })

  -- Lists
  highlight(0, 'MarkviewListItemMinus',                 { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewListItemPlus',                  { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewListItemStar',                  { fg = colors.blue,       bg = 'NONE' })

  -- Tables
  highlight(0, 'MarkviewTableHeader',                   { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'MarkviewTableBorder',                   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewTableAlignLeft',                { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewTableAlignRight',               { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'MarkviewTableAlignCenter',              { fg = colors.blue,       bg = 'NONE' })

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

  highlight(0, 'Headline',                              { fg = colors.blue,       bg = colors.backgroundLight, bold = true })
  highlight(0, 'Headline1',                             { fg = colors.blue,       bg = colors.backgroundLight, bold = true })
  highlight(0, 'Headline2',                             { fg = colors.blue,       bg = colors.backgroundLight, bold = true })
  highlight(0, 'Headline3',                             { fg = colors.blue,       bg = colors.backgroundLight, bold = true })
  highlight(0, 'Headline4',                             { fg = colors.blue,       bg = colors.backgroundLight, bold = true })
  highlight(0, 'Headline5',                             { fg = colors.blue,       bg = colors.backgroundLight, bold = true })
  highlight(0, 'Headline6',                             { fg = colors.blue,       bg = colors.backgroundLight, bold = true })
  highlight(0, 'CodeBlock',                             { fg = 'NONE',            bg = colors.backgroundLight })
  highlight(0, 'Dash',                                  { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'DoubleDash',                            { fg = colors.blue,       bg = 'NONE', bold = true })
  highlight(0, 'Quote',                                 { fg = colors.orange,     bg = 'NONE' })

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
  highlight(0, '@tag.delimiter.mdx',                    { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@tag.attribute.mdx',                    { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@punctuation.bracket.mdx',              { fg = colors.white,      bg = 'NONE' })

  -- JSX in MDX
  highlight(0, '@keyword.import.mdx',                   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@keyword.export.mdx',                   { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@variable.mdx',                         { fg = colors.white,      bg = 'NONE' })
  highlight(0, '@function.mdx',                         { fg = colors.orange,     bg = 'NONE' })
  highlight(0, '@string.mdx',                           { fg = colors.redLight,   bg = 'NONE' })

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
  highlight(0, 'ObsidianComment',                       { fg = colors.red,        bg = 'NONE' })

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
  highlight(0, 'mermaidKeyword',                        { fg = colors.blue,       bg = 'NONE' })
  highlight(0, 'mermaidComment',                        { fg = colors.red,        bg = 'NONE' })

  ---------------------------------------------------------------
  -- Latex/Math in Markdown
  ---------------------------------------------------------------

  highlight(0, 'markdownMath',                          { fg = colors.green,      bg = 'NONE' })
  highlight(0, 'markdownMathDelimiter',                 { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@markup.math.markdown',                 { fg = colors.green,      bg = 'NONE' })
  highlight(0, '@markup.math.markdown_inline',          { fg = colors.green,      bg = 'NONE' })

  ---------------------------------------------------------------
  -- YAML Front Matter (in Markdown)
  ---------------------------------------------------------------

  highlight(0, '@keyword.yaml.frontmatter',             { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@property.yaml.frontmatter',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@string.yaml.frontmatter',              { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@number.yaml.frontmatter',              { fg = colors.greenLight, bg = 'NONE' })

  ---------------------------------------------------------------
  -- TOML Front Matter (in Markdown - Hugo style)
  ---------------------------------------------------------------

  highlight(0, '@keyword.toml.frontmatter',             { fg = colors.blue,       bg = 'NONE' })
  highlight(0, '@property.toml.frontmatter',            { fg = colors.turquoise,  bg = 'NONE' })
  highlight(0, '@string.toml.frontmatter',              { fg = colors.redLight,   bg = 'NONE' })
  highlight(0, '@number.toml.frontmatter',              { fg = colors.greenLight, bg = 'NONE' })

end

return markdown
