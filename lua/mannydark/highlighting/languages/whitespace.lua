-------------------------------------------------------------------------------
-- Whitespace Files
-- Highlighting for .ws, .whitespace files.
-- Whitespace is an esoteric language where only space, tab, and linefeed
-- are significant. All other characters are ignored (comments).
-------------------------------------------------------------------------------

local colors  = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local whitespace = {}


-------------------------------------------------------------------------------
-- Settings

whitespace.setupHighlighting = function()

end

return whitespace