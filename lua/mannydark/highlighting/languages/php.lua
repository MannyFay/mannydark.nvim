-------------------------------------------------------------------------------
-- PHP
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local php       = {}


-------------------------------------------------------------------------------
-- Settings

php.setupHighlighting = function()
  -- highlight(0, "@punctuation.bracket.php", { link = "Error"})  -- Errors


end

return php
