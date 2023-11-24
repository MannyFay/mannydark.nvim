------------------------------------------------------------------------------
-- Laravel Blade Files
-- Highlighting for Laravel Blade files (*.blade.php).
------------------------------------------------------------------------------

local colors       = require('mannydark.palette')
local highlight    = vim.api.nvim_set_hl
local laravelBlade = {}


--------------------------------------------------------------
-- Settings

laravelBlade.setupHighlighting = function()
  highlight(0, 'bladeDelimiter', { fg = colors.white,  bg = 'NONE' })
  highlight(0, 'bladeEcho',      { fg = colors.orange, bg = 'NONE' })
end

return laravelBlade
