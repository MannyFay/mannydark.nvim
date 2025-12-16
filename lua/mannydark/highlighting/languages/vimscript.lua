-------------------------------------------------------------------------------
-- Vim Script Language
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local vimscript = {}


-------------------------------------------------------------------------------
-- Settings

vimscript.setupHighlighting = function()
  highlight(0, '@character.special.vim', { fg = colors.blue,      bg = 'NONE' })

  ----------------------- Not used by now:

end

return vimscript

