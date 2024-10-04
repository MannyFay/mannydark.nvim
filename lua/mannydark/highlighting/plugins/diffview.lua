-------------------------------------------------------------------------------
-- Diffview Neovim Plugin
-------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local diffview  = {}


--------------------------------------------------------------
-- Settings

diffview.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, 'DiffViewNormal',             { fg = colors.purple, bg = colors.blue })
  highlight(0, 'DiffviewStatusAdded',        { fg = colors.purple, bg = colors.green })
  highlight(0, 'DiffviewStatusModified',     { fg = colors.purple, bg = colors.greenLight })
  highlight(0, 'DiffviewStatusRenamed',      { fg = colors.purple, bg = colors.red })
  highlight(0, 'DiffviewStatusDeleted',      { fg = colors.purple, bg = colors.turquoise })
  highlight(0, 'DiffviewFilePanelInsertion', { fg = colors.purple, bg = colors.orange })
  highlight(0, 'DiffviewFilePanelDeletion',  { fg = colors.turquoise, bg = colors.pink })
  highlight(0, 'DiffviewVertSplit',          { fg = colors.turquoise, bg = colors.blue })
end

return diffview
