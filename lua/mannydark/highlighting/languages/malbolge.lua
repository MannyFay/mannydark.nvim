-------------------------------------------------------------------------------
-- Malbolge Files
-- Highlighting for .mal, .malbolge files.
-- Malbolge is an esoteric language designed by Ben Olmstead in 1998 to be
-- as difficult to program in as possible. Named after the 8th circle of Hell.
-------------------------------------------------------------------------------

local colors  = require("mannydark.palette")
local highlight = vim.api.nvim_set_hl
local malbolge = {}


-------------------------------------------------------------------------------
-- Settings

malbolge.setupHighlighting = function()

end

return malbolge