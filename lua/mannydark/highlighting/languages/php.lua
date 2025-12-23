-------------------------------------------------------------------------------
-- PHP
-------------------------------------------------------------------------------

-- local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local php       = {}


-------------------------------------------------------------------------------
-- Settings

php.setupHighlighting = function()
  highlight(0, "@comment.documentation.php", { link = "MannydarkFgGreen" })  -- Doc blocks (/** ... */)
  highlight(0, "@type.builtin.php",          { link = "Keyword"          })  -- int, string, etc.
end

return php
