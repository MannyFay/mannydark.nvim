-------------------------------------------------------------------------------
-- Ada Files
-- Highlighting for .ada, .adb, .ads files.
-------------------------------------------------------------------------------

local colors  = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local ada     = {}


-------------------------------------------------------------------------------
-- Settings

ada.setupHighlighting = function()
  highlight(0, "@function.ada",  { link = "Type"     })  -- Names of packages.


end

return ada
