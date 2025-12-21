-------------------------------------------------------------------------------
-- Shell (POSIX sh)
-- Base highlighting for POSIX shell scripts (.sh files).
-- These sh* groups are shared with bash, ksh, dash, and other sh-compatible shells.
-------------------------------------------------------------------------------

local colors    = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local shell     = {}


-------------------------------------------------------------------------------
-- Settings

shell.setupHighlighting = function()

end

return shell