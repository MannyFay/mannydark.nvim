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
  highlight(0, 'packerOutput',        { fg = colors.green,    bg = 'NONE' })  -- Topic of Packers doings.
  highlight(0, 'packerHash',          { fg = colors.white,    bg = 'NONE' })  -- Message of Packers doings.
  highlight(0, 'packerStatusSuccess', { fg = colors.green,    bg = 'NONE' })  -- Success message.
  highlight(0, 'packerString',        { fg = colors.redLight, bg = 'NONE' })  -- Message what user can do.
  highlight(0, 'packerRelDate',       { fg = colors.white,    bg = 'NONE' })  -- Timestamp of commit release.
end

return packer

