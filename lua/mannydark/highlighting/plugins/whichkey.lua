------------------------------------------------------------------------------
-- WhichKey Neovim Plugin
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local whichKey  = {}


--------------------------------------------------------------
-- Settings

whichKey.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, 'WhichKey',          { fg = colors.purple, bg = colors.redLight })
  highlight(0, 'WhichKeySeperator', { fg = colors.purple, bg = colors.redLight })
  highlight(0, 'WhichKeyGroup',     { fg = colors.purple, bg = colors.redLight })
  highlight(0, 'WhichKeyDesc',      { fg = colors.purple, bg = colors.redLight })
  highlight(0, 'WhichKeyFloat',     { fg = colors.purple, bg = colors.redLight })
end

return whichKey
