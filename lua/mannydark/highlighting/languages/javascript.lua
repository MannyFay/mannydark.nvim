-------------------------------------------------------------------------------
-- JavaScript Files
-- Highlighting for .js, .mjs, .cjs files.
-------------------------------------------------------------------------------

-- local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local javascript = {}


-------------------------------------------------------------------------------
-- Settings

javascript.setupHighlighting = function()
  highlight(0, "@character.special.javascript", { link = "MannydarkFgWhite" })
end

return javascript
