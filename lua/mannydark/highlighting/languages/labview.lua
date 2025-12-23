-------------------------------------------------------------------------------
-- LabVIEW
-- Highlighting for LabVIEW-related text files:
-- - MathScript (.m files in LabVIEW context)
-- - Formula Node (C-like syntax)
-- - Project files (.lvproj, .lvlib, .lvclass - XML-based)
-- Note: .vi files are binary and cannot be syntax highlighted
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local labview   = {}


-------------------------------------------------------------------------------
-- Settings

labview.setupHighlighting = function()

end

return labview