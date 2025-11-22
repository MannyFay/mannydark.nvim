-------------------------------------------------------------------------------
-- SCSS Highlighting
-------------------------------------------------------------------------------


-------------------------------------------------------------------------------
-- Variables

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local scss      = {}


-------------------------------------------------------------------------------
-- Highlighting

scss.setupHighlighting = function()
  highlight(0, "@character.special.scss", { fg = colors.blue, bg = "NONE" })  -- Chars like *, &.


  ----------------------- Not used by now:

end

return scss

