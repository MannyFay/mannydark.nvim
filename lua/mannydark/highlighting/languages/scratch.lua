-------------------------------------------------------------------------------
-- Scratch
-- Highlighting for Scratch-related text formats:
-- - Scratchblocks syntax (text representation of Scratch blocks)
-- - SB3/SB2 project.json (JSON structure)
-- Note: .sb3/.sb2 files are ZIP archives containing JSON
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local scratch   = {}


-------------------------------------------------------------------------------
-- Settings

scratch.setupHighlighting = function()

end

return scratch