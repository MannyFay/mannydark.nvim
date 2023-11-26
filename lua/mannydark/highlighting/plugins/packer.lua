------------------------------------------------------------------------------
-- Packer Neovim Plugin
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local packer    = {}


--------------------------------------------------------------
-- Settings

packer.setupHighlighting = function()
  highlight(0, 'packerSuccess',       { fg = colors.green,    bg = 'NONE' })  -- Left sign column.
  highlight(0, 'packerOutput',        { fg = colors.blue,     bg = 'NONE' })  -- Topic of Packers doings.
  highlight(0, 'packerHash',          { fg = colors.green,    bg = 'NONE' })  -- Message of Packers doings.
  highlight(0, 'packerStatusSuccess', { fg = colors.green,    bg = 'NONE' })  -- Success message.
  highlight(0, 'packerString',        { fg = colors.redLight, bg = 'NONE' })  -- Message what user can do.

  ----------------------- Not used by now:
  highlight(0, 'packerRelDate',       { fg = colors.red, bg = colors.blue })
end

return packer

