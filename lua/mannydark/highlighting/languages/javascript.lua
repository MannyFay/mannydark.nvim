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
  highlight(0, "@tag.delimiter.javascript", { link = "Ignore" })
  highlight(0, "@comment.documentation.javascript", { link = "MannydarkFgGreen" })



end

return javascript
