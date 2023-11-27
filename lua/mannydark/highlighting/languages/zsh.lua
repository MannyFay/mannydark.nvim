------------------------------------------------------------------------------
-- ZSH
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local zsh       = {}


--------------------------------------------------------------
-- Settings

zsh.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, 'zshKSHFunction',     { fg = colors.pink, bg = colors.greenLight })
  highlight(0, 'zshVariableDef',     { fg = colors.pink, bg = colors.green })
  highlight(0, 'zshOperator',        { fg = colors.pink, bg = colors.blue })
  highlight(0, 'zshDelim',           { fg = colors.pink, bg = colors.blueLink })
  highlight(0, 'zshBraces',          { fg = colors.pink, bg = colors.white })
  highlight(0, 'zshBrackets',        { fg = colors.pink, bg = colors.gray })
  highlight(0, 'zshFlag',            { fg = colors.pink, bg = colors.orange })
  highlight(0, 'zshDeref',           { fg = colors.pink, bg = colors.turquoise })
  highlight(0, 'zshFunction',        { fg = colors.pink, bg = colors.purple })
  highlight(0, 'zshStringDelimiter', { fg = colors.pink, bg = colors.red })
  highlight(0, 'zshSubst',           { fg = colors.pink, bg = colors.redLight })
  highlight(0, 'zshSubstDelim',      { fg = colors.blue, bg = colors.greenLight })
end

return zsh

