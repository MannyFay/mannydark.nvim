------------------------------------------------------------------------------
-- Telescope Neovim Plugin
------------------------------------------------------------------------------

local colors    = require('mannydark.palette')
local highlight = vim.api.nvim_set_hl
local telescope = {}


--------------------------------------------------------------
-- Settings

telescope.setupHighlighting = function()
  ----------------------- Not used by now:
  highlight(0, 'TelescopeSelection',      { fg = colors.blue, bg = colors.pink                              })
  highlight(0, 'TelescopeSelectionCaret', { fg = colors.red,  bg = colors.pink                              })
  highlight(0, 'TelescopeMatching',       { fg = colors.blue, bg = colors.red, bold = true, italic = true, })
  highlight(0, 'TelescopeBorder',         { fg = colors.blue, bg = colors.orange                              })
  highlight(0, 'TelescopeNormal',         { fg = colors.blue, bg = colors.green                              })
  highlight(0, 'TelescopePromptPrefix',   { fg = colors.blue, bg = colors.redLight                              })
  highlight(0, 'TelescopePromptTitle',    { fg = colors.blue, bg = colors.turquoise, bold = true,                })
  highlight(0, 'TelescopeResultsTitle',   { fg = colors.blue, bg = colors.purple, bold = true,                })
  highlight(0, 'TelescopePreviewTitle',   { fg = colors.blue, bg = colors.greenLight, bold = true,                })
  highlight(0, 'TelescopePromptCounter',  { fg = colors.blue, bg = colors.white                              })
  highlight(0, 'TelescopePreviewHyphen',  { fg = colors.green, bg = colors.pink                              })
end

return telescope
