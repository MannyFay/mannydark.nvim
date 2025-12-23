-------------------------------------------------------------------------------
-- HTML Files
-- Highlighting for .html, .htm, .xhtml, .shtml files.
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local html      = {}


-------------------------------------------------------------------------------
-- Settings

html.setupHighlighting = function()
  highlight(0, "@tag.attribute.html",      { link = "Variable" })  -- Attributes (class, id, href, etc.) - purple
  highlight(0, "@string.special.url.html", { fg = colors.redLight, bg = "NONE", underline = true })
  highlight(0, "@character.special.html",  { link = "Keyword" })
  highlight(0, "htmlTag",                  { link = "Ignore" })  -- <, >, />
end

return html
