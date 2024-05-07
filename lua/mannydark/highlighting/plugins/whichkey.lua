-------------------------------------------------------------------------------
-- WhichKey Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local whichKey  = {}


--------------------------------------------------------------
-- Settings

whichKey.setupHighlighting = function()
  highlight(0, 'WhichKey',          { fg = colors.blue, bg = colors.grayDark })
  highlight(0, 'WhichKeyGroup',     { fg = colors.purple, bg = colors.grayDark })
  highlight(0, 'WhichKeySeperator', { fg = colors.white, bg = colors.grayDark })
  highlight(0, 'WhichKeyDesc',      { fg = colors.purple, bg = colors.grayDark })
  highlight(0, 'WhichKeyFloat',     { fg = colors.purple, bg = colors.grayDark })
  highlight(0, 'WhichKeyBorder',    { fg = colors.purple, bg = colors.grayDark })
  highlight(0, 'WhichKeyValue',     { fg = colors.purple, bg = colors.grayDark })
end

return whichKey

