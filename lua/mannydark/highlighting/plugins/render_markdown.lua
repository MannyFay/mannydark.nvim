-------------------------------------------------------------------------------
-- render-markdown.nvim (MeanderingProgrammer/render-markdown.nvim)
-- https://github.com/MeanderingProgrammer/render-markdown.nvim
-------------------------------------------------------------------------------

local colors          = require("mannydark.palette")
local highlight       = vim.api.nvim_set_hl
local render_markdown = {}


-------------------------------------------------------------------------------
-- Settings

render_markdown.setupHighlighting = function()
  ------------------------------------------------------------------------------
  -- Headings (foreground/icons)

  highlight(0, "RenderMarkdownH1", { fg = colors.purple, bold = true })
  highlight(0, "RenderMarkdownH2", { fg = colors.blue, bold = true })
  highlight(0, "RenderMarkdownH3", { fg = colors.blue, bold = true })
  highlight(0, "RenderMarkdownH4", { fg = colors.blue, bold = true })
  highlight(0, "RenderMarkdownH5", { fg = colors.blue, bold = true })
  highlight(0, "RenderMarkdownH6", { fg = colors.blue, bold = true })

  ------------------------------------------------------------------------------
  -- Heading backgrounds (transparent)

  highlight(0, "RenderMarkdownH1Bg", { bg = "NONE" })
  highlight(0, "RenderMarkdownH2Bg", { bg = "NONE" })
  highlight(0, "RenderMarkdownH3Bg", { bg = "NONE" })
  highlight(0, "RenderMarkdownH4Bg", { bg = "NONE" })
  highlight(0, "RenderMarkdownH5Bg", { bg = "NONE" })
  highlight(0, "RenderMarkdownH6Bg", { bg = "NONE" })

  ------------------------------------------------------------------------------
  -- Code blocks

  highlight(0, "RenderMarkdownCode",         { bg = colors.grayDark })
  highlight(0, "RenderMarkdownCodeInfo",     { fg = colors.gray, italic = true })
  highlight(0, "RenderMarkdownCodeBorder",   { fg = colors.grayLight })
  highlight(0, "RenderMarkdownCodeFallback", { fg = colors.white, bg = colors.grayDark })
  highlight(0, "RenderMarkdownCodeInline",   { fg = colors.redLight, bg = colors.grayDark })

  ------------------------------------------------------------------------------
  -- Blockquotes

  highlight(0, "RenderMarkdownQuote",  { fg = colors.gray, italic = true })
  highlight(0, "RenderMarkdownQuote1", { fg = colors.gray, italic = true })
  highlight(0, "RenderMarkdownQuote2", { fg = colors.gray, italic = true })
  highlight(0, "RenderMarkdownQuote3", { fg = colors.gray, italic = true })
  highlight(0, "RenderMarkdownQuote4", { fg = colors.gray, italic = true })
  highlight(0, "RenderMarkdownQuote5", { fg = colors.gray, italic = true })
  highlight(0, "RenderMarkdownQuote6", { fg = colors.gray, italic = true })

  ------------------------------------------------------------------------------
  -- Inline highlights

  highlight(0, "RenderMarkdownInlineHighlight", { bg = colors.orangeDark })

  ------------------------------------------------------------------------------
  -- Lists & structure

  highlight(0, "RenderMarkdownBullet", { fg = colors.blue, bold = true })
  highlight(0, "RenderMarkdownDash",   { fg = colors.gray, bg = "NONE" })
  highlight(0, "RenderMarkdownSign",   { fg = colors.gray })
  highlight(0, "RenderMarkdownIndent", { fg = colors.grayLight })

  ------------------------------------------------------------------------------
  -- Math

  highlight(0, "RenderMarkdownMath", { fg = colors.greenLight })

  ------------------------------------------------------------------------------
  -- HTML comments

  highlight(0, "RenderMarkdownHtmlComment", { fg = colors.red, italic = true })

  ------------------------------------------------------------------------------
  -- Links

  highlight(0, "RenderMarkdownLink",      { fg = colors.blueLink, underline = true })
  highlight(0, "RenderMarkdownLinkTitle", { fg = colors.blue })
  highlight(0, "RenderMarkdownWikiLink",  { fg = colors.purple, underline = true })

  ------------------------------------------------------------------------------
  -- Checkboxes

  highlight(0, "RenderMarkdownUnchecked", { fg = colors.orange, bold = true })
  highlight(0, "RenderMarkdownChecked",   { fg = colors.green, bold = true })
  highlight(0, "RenderMarkdownTodo",      { fg = colors.orange })

  ------------------------------------------------------------------------------
  -- Tables

  highlight(0, "RenderMarkdownTableHead", { fg = colors.blue, bold = true })
  highlight(0, "RenderMarkdownTableRow",  { fg = colors.white })
  highlight(0, "RenderMarkdownTableFill", { fg = colors.grayDark })

  ------------------------------------------------------------------------------
  -- Callouts (diagnostic-style)

  highlight(0, "RenderMarkdownSuccess", { fg = colors.green })
  highlight(0, "RenderMarkdownInfo",    { fg = colors.blue })
  highlight(0, "RenderMarkdownHint",    { fg = colors.turquoise })
  highlight(0, "RenderMarkdownWarn",    { fg = colors.orange })
  highlight(0, "RenderMarkdownError",   { fg = colors.red })
end

return render_markdown
