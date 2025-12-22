-------------------------------------------------------------------------------
-- Markdown
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local markdown  = {}


--------------------------------------------------------------
-- Settings

markdown.setupHighlighting = function()
  highlight(0, "@markup.heading.1.markdown", { link = "Keyword" })
  highlight(0, "@markup.heading.2.markdown", { link = "Keyword" })
  highlight(0, "@markup.heading.3.markdown", { link = "Keyword" })
  highlight(0, "@markup.heading.4.markdown", { link = "Keyword" })
  highlight(0, "@markup.heading.5.markdown", { link = "Keyword" })
  highlight(0, "@markup.heading.6.markdown", { link = "Keyword" })
  highlight(0, "@conceal.markdown_inline", { link = "Keyword" })
  highlight(0, "@lsp.type.class.markdown", { link = "MannydarkFgWhite" })
  highlight(0, "@markup.link.markdown_inline", { link = "Keyword" })
  highlight(0, "@markup.link.url.markdown_inline", { fg = colors.white, bg = "NONE", underline = true })
  highlight(0, "@markup.link.url.markdown", { fg = colors.white, bg = "NONE", underline = true })
  highlight(0, "@markup.raw.markdown_inline", { link = "MannydarkFgWhite" })
  highlight(0, "@markup.raw.block.markdown", { link = "MannydarkFgWhite" })
  highlight(0, "@punctuation.special.markdown", { link = "Keyword" })
  highlight(0, "@markup.quote.markdown", { link = "MannydarkFgWhite" })
end

return markdown
