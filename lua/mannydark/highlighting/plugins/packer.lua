------------------------------------------------------------------------------
-- Packer Neovim Plugin
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local packer    = {}


--------------------------------------------------------------
-- Settings

packer.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, 'packerString',        { fg = colors.red, bg = colors.green })
  highlight(0, 'packerHash',          { fg = colors.red, bg = colors.green })
  highlight(0, 'packerOutput',        { fg = colors.red, bg = colors.green })
  highlight(0, 'packerRelDate',       { fg = colors.red, bg = colors.green })
  highlight(0, 'packerSuccess',       { fg = colors.red, bg = colors.green })
  highlight(0, 'packerStatusSuccess', { fg = colors.red, bg = colors.green })
end

return packer
