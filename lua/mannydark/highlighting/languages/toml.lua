-------------------------------------------------------------------------------
-- TOML Files
-- Highlighting for .toml files.
-------------------------------------------------------------------------------

-- local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local toml      = {}


-------------------------------------------------------------------------------
-- Settings

toml.setupHighlighting = function()
  highlight(0, "@string.special.toml", { link = "String" })
end

return toml
