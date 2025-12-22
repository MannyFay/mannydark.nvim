-------------------------------------------------------------------------------
-- XML Files
-- Highlighting for .xml, .xsd, .xslt, .svg, .plist, .xaml files.
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local xml       = {}


-------------------------------------------------------------------------------
-- Settings

xml.setupHighlighting = function()
  highlight(0, "@punctuation.delimiter.xml", { link = "String" })  -- Quotes on strings.
  highlight(0, "@string.special.xml",        { link = "String" })  -- Strings like "UTF-8".
  highlight(0, "@string.special.url.xml",    { fg = colors.redLight, bg = "NONE", underline = true })
  highlight(0, "@character.special.xml",     { link = "MannydarkFgWhite" })  -- *, ?, etc.
  highlight(0, "@character.xml",             { link = "Constant" })  -- *, ?, etc.
end

return xml
