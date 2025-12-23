-------------------------------------------------------------------------------
-- Jsonnet Files
-- Highlighting for .jsonnet, .libsonnet files.
-------------------------------------------------------------------------------

-- local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local jsonnet   = {}


-------------------------------------------------------------------------------
-- Settings

jsonnet.setupHighlighting = function()
  highlight(0, "@comment.documentation.jsonnet", { link = "MannydarkFgGreen" })  -- Doc blocks (/** ... */ or ///)
end

return jsonnet
