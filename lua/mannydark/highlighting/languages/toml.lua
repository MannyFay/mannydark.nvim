-------------------------------------------------------------------------------
-- Toml Highlighting
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Variables

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local toml      = {}


-------------------------------------------------------------------------------
-- Highlighting

toml.setupHighlighting = function()
  highlight(0, "@string.special.toml", { fg = colors.redLight, bg = "NONE" })  -- Strings without quotes.


  ----------------------- Not used by now:

end

return toml

