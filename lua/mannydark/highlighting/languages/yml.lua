-------------------------------------------------------------------------------
-- YAML Files
-- Highlighting for .yml and .yaml files.
-------------------------------------------------------------------------------

-- local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local yml       = {}


-------------------------------------------------------------------------------
-- Settings

yml.setupHighlighting = function()
  highlight(0, "@punctuation.special.yaml", { link = "Keyword" })  -- ---
end

return yml
