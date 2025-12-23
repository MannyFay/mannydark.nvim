-------------------------------------------------------------------------------
-- C Files
-- Highlighting for .c, .h files.
-------------------------------------------------------------------------------

-- local colors  = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local c       = {}


-------------------------------------------------------------------------------
-- Settings

c.setupHighlighting = function()
  highlight(0, "@comment.documentation.c", { link = "MannydarkFgGreen" })  -- Doc blocks (/** ... */ or ///)
  highlight(0, "@type.builtin.c", { link = "Keyword" })  -- int, char, float, etc.
end

return c
