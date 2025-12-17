-------------------------------------------------------------------------------
-- Notify Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local notify    = {}


--------------------------------------------------------------
-- Settings

notify.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, 'NotifyERRORBorder', { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyWARNBorder',  { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyINFOBorder',  { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyDEBUGBorder', { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyTRACEBorder', { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyERRORIcon',   { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyWARNIcon',    { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyINFOIcon',    { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyDEBUGIcon',   { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyTRACEIcon',   { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyERRORTitle',  { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyWARNTitle',   { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyINFOTitle',   { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyDEBUGTitle',  { fg = colors.red, bg = colors.redLight })
  highlight(0, 'NotifyTRACETitle',  { fg = colors.red, bg = colors.redLight })
end

return notify

