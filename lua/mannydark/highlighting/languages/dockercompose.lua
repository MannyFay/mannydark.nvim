------------------------------------------------------------------------------
-- Docker Compose
------------------------------------------------------------------------------

local colors        = require('mannydark.palette')
local highlight     = vim.api.nvim_set_hl
local dockerCompose = {}


--------------------------------------------------------------
-- Settings

dockerCompose.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, 'dockercomposeKeywords', { fg = colors.blue, bg = colors.pink })
  highlight(0, 'bashStatement',         { fg = colors.blue, bg = colors.pink })
  highlight(0, 'dockercomposeString',   { fg = colors.blue, bg = colors.pink })
  highlight(0, 'dockercomposeString1',  { fg = colors.blue, bg = colors.pink })
  highlight(0, 'dockercomposeEmail',    { fg = colors.blue, bg = colors.pink })
  highlight(0, 'dockercomposeUrl',      { fg = colors.blue, bg = colors.pink })
  highlight(0, 'dockercomposeTodo',     { fg = colors.blue, bg = colors.pink })
  highlight(0, 'dockercomposeComment',  { fg = colors.blue, bg = colors.pink })
end

return dockerCompose
